;;; org-learn-study.el --- Evidence-based study techniques for org-learn -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Khalid

;; Author: Khalid
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.0"))
;; Keywords: org, learning
;; URL: https://github.com/khaalidrafi/org-learn.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Spec 016 (specs/016-evidence-study.org): the study techniques the
;; research actually backs, on top of the spec 002 SM-2 engine.  Each
;; technique names its source; the citations live in the spec.
;;
;; - Interleaving -- `org-learn-review-interleaved' walks the SAME due
;;   queue as `org-learn-review', but in an interleaved order: a
;;   deterministic day-seeded shuffle whose repair pass spreads
;;   same-topic neighbors apart.  Taylor and Rohrer (2010, Applied
;;   Cognitive Psychology) found interleaved practice doubled
;;   next-day test scores; Rohrer et al.'s 2020 randomized controlled
;;   trial confirmed it in classrooms.  `org-learn-study--interleave'
;;   is the pure step, testable with no buffer at all.
;; - Feynman technique -- `org-learn-feynman' prompts for a plain-
;;   language explanation of the entry at point and files it as a
;;   dated child heading.  The note is visible in the file but never
;;   inside the review prompt: `org-learn-review--entry-content'
;;   excludes subheadings by law.
;; - Deliberate practice -- `org-learn-practice' clocks a focused
;;   session with an explicit goal (Ericsson 1993; the goal is stored
;;   as LEARN_GOAL); `org-learn-practice-end' prompts the reflection
;;   and files both as a dated child note -- the feedback loop the
;;   deliberate-practice literature demands.
;; - Pomodoro -- `org-learn-pomodoro' runs focus/break cycles over
;;   org-timer with phase notifications (`org-learn-notify' when its
;;   module is loaded, `message' otherwise).
;;
;; Seams (the repo's standard law): the clock, the timer and the
;; notifier are defcustom functions, so the ERT suite never needs a
;; live clock, timer or notification daemon.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'org-timer)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org-learn)
(require 'org-learn-review)

;; org-learn-notify.el is optional at load time; the pomodoro
;; callback goes through `org-learn-study-notify-function' anyway.
(declare-function org-learn-notify "org-learn-notify" (title body &rest args))

(defgroup org-learn-study nil
  "Evidence-based study techniques for org-learn."
  :group 'org-learn
  :prefix "org-learn-")

(defcustom org-learn-study-clock-function #'org-clock-in
  "Clock seam: function starting a study clock on the entry at point.
`org-learn-practice' and the pomodoro go through it; tests bind a
recording stub."
  :type 'function
  :group 'org-learn-study)

(defcustom org-learn-study-clock-out-function #'org-clock-out
  "Clock-out seam: function ending the running clock.
Tests bind a recording stub."
  :type 'function
  :group 'org-learn-study)

(defcustom org-learn-study-timer-function #'org-timer-set-timer
  "Timer seam: function of one argument, minutes, starting a timer.
The pomodoro state machine drives it; tests call the tick directly."
  :type 'function
  :group 'org-learn-study)

(defcustom org-learn-study-notify-function
  (lambda (title body)
    (if (fboundp 'org-learn-notify)
        (org-learn-notify title body)
      (message "org-learn: %s -- %s" title body)))
  "Notify seam: function of TITLE and BODY for pomodoro phases.
The default routes to `org-learn-notify' when org-learn-notify.el is
loaded and falls back to `message' otherwise."
  :type 'function
  :group 'org-learn-study)

;;; Interleaving (spec 016; Taylor & Rohrer 2010, Rohrer et al. 2020).

(defun org-learn-study--random (seed)
  "Return a deterministic generator for SEED, a non-negative integer.
The closure produces successive floats in [0, 1) through a simple
linear congruential step -- deterministic across Emacs builds and
days, so a session order never depends on runtime randomness."
  (let ((x (logand (max 1 seed) #x3fffffff)))
    (lambda ()
      (setq x (mod (+ (* x 48271) 11) #x3fffffff))
      (/ (float x) #x3fffffff))))

(defun org-learn-study--shuffle (items seed)
  "Return a copy of ITEMS in Fisher-Yates order from the SEED generator."
  (let* ((vec (apply #'vector items))
         (rand (org-learn-study--random seed))
         (n (length vec)))
    (cl-loop for i from (- n 1) downto 1
             do (let* ((j (floor (* (funcall rand) (1+ i))))
                       (tmp (aref vec i)))
                  (aset vec i (aref vec j))
                  (aset vec j tmp)))
    (append vec nil)))

(defun org-learn-study--interleave (items &optional topic-fn seed)
  "Return ITEMS reordered so same-topic neighbors sit far apart.
TOPIC-FN maps an item to its topic string (default: the item itself,
so a flat list never blocks); SEED drives the shuffle, default 1.
Two passes: a deterministic Fisher-Yates shuffle, then a repair walk
that swaps any item whose predecessor shares its topic with the next
following item of a different topic.  Pure: ITEMS is never mutated
and no buffer is touched."
  (let* ((topic (or topic-fn (lambda (item) (format "%s" item))))
         (order (org-learn-study--shuffle items (or seed 1)))
         (n (length order)))
    (cl-loop for i from 1 below n
             do (let ((cur (nth i order)))
                  (when (string= (funcall topic (nth (1- i) order))
                                 (funcall topic cur))
                    (let ((j (cl-loop for k from (1+ i) below n
                                      when (not (string=
                                                 (funcall topic (nth k order))
                                                 (funcall topic cur)))
                                      return k)))
                      (when j
                        (setf (nth i order) (nth j order)
                              (nth j order) cur))))))
    order))

(defun org-learn-study--topic-of (marker)
  "Return the topic bucket of MARKER's entry.
A level-1 entry is its own topic; a deeper entry belongs to its
level-1 ancestor (the dashboard's topic vocabulary)."
  (org-with-point-at marker
    (when (> (org-current-level) 1)
      (while (and (> (org-current-level) 1)
                  (org-up-heading-safe))))
    (org-get-heading t t t t)))

;;;###autoload
(defun org-learn-review-interleaved ()
  "Run a review session in an interleaved order (spec 016).
The due queue is exactly `org-learn-review's -- same items, same
SM-2 grading -- but the walk order interleaves topics: same-topic
items are spread apart instead of arriving as one block, the
practice schedule Taylor and Rohrer (2010) showed roughly doubles
next-day retention compared with blocked practice.  The order is
seeded from today's date, so one day's session is deterministic."
  (interactive)
  (let* ((files (seq-filter #'file-exists-p
                            (org-learn-review--scoped-files)))
         (pre-existing (mapcar #'get-file-buffer files))
         (buffers (mapcar #'find-file-noselect files))
         items)
    (unwind-protect
        (progn
          (dolist (buffer buffers)
            (setq items (append items (org-learn-review--collect-items buffer))))
          (let ((ordered
                 (org-learn-study--interleave
                  items
                  (lambda (item)
                    (org-learn-study--topic-of (nth 0 item)))
                  (string-to-number
                   (format-time-string "%Y%m%d")))))
            (catch 'org-learn-review-quit
              (dolist (item ordered)
                (let* ((marker (nth 0 item))
                       (content (with-current-buffer (marker-buffer marker)
                                  (goto-char marker)
                                  (org-learn-review--entry-content)))
                       (grade (progn
                                (with-output-to-temp-buffer
                                    "*Org Learn Review*"
                                  (princ content))
                                (org-learn-review--prompt-quality))))
                  (cond
                   ((eq grade 'quit)
                    (throw 'org-learn-review-quit t))
                   ((eq grade 'skip))
                   (t
                    (with-current-buffer (marker-buffer marker)
                      (goto-char marker)
                      (org-learn-review--process-entry grade)
                      (save-buffer)))))))))
      (dolist (item items)
        (let ((marker (nth 0 item)))
          (when (and (markerp marker) (marker-buffer marker))
            (set-marker marker nil))))
      (let ((review (get-buffer "*Org Learn Review*")))
        (when review (kill-buffer review)))
      (dolist (buffer buffers)
        (unless (memq buffer pre-existing)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

;;; Feynman technique (spec 016).

(defun org-learn-study--read-multiline (prompt)
  "Read lines from the minibuffer until an empty line ends input.
Return the joined text, without the terminating empty line."
  (let ((lines nil)
        (more t))
    (while more
      (let ((line (read-string (if lines
                                   "  (empty line finishes) "
                                 prompt))))
        (if (string= line "")
            (setq more nil)
          (push line lines))))
    (string-join (nreverse lines) "\n")))

;;;###autoload
(defun org-learn-feynman ()
  "File a Feynman explanation for the entry at point (spec 016).
Prompts for a plain-language explanation -- as if teaching a curious
twelve-year-old -- and files it as a dated child heading under the
entry.  The explanation lives in the file, never inside the review
prompt: `org-learn-review--entry-content' excludes subheadings, so
reviews stay prompt-first (active recall), and the note is there to
reread after grading."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer; org-learn-feynman needs one"))
  (org-back-to-heading t)
  (let* ((topic (org-get-heading t t t t))
         (text (string-trim
                (org-learn-study--read-multiline
                 (format "Explain %s as if to a curious 12-year-old.  Empty line finishes: "
                         topic)))))
    (when (string= text "")
      (user-error "Empty explanation; nothing filed"))
    (org-with-point-at (point)
      (goto-char (org-end-of-subtree t))
      (unless (bolp) (insert "\n"))
      (insert "** Feynman " (format-time-string "%Y-%m-%d") "\n"
              (org-learn--indent-block text) "\n"))
    (message "org-learn: Feynman note filed under %s" topic)))

;;; Deliberate practice (spec 016; Ericsson 1993, 2008).

;;;###autoload
(defun org-learn-practice (goal)
  "Start a deliberate-practice session at the entry at point.
Deliberate practice is targeted work on a well-defined goal with a
feedback loop -- not autopilot repetition.  GOAL is stored as the
entry's LEARN_GOAL property, the clock starts through
`org-learn-study-clock-function', and `org-learn-practice-end'
closes the loop by filing your reflection."
  (interactive
   (list (read-string "Practice goal -- what exactly improves: ")))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer; org-learn-practice needs one"))
  (unless (ignore-errors (org-back-to-heading t))
    (user-error "No heading at point; org-learn-practice needs one"))
  (when (string= (string-trim goal) "")
    (user-error "A practice session needs an explicit goal"))
  (org-entry-put (point) "LEARN_GOAL" (string-trim goal))
  (funcall org-learn-study-clock-function)
  (message "org-learn: practice clocked -- %s (end with M-x org-learn-practice-end)"
           (string-trim goal)))

;;;###autoload
(defun org-learn-practice-end (reflection)
  "End the practice session: clock out and file REFLECTION.
The reflection and the session's LEARN_GOAL land as a dated child
note under the entry -- the feedback record deliberate practice
builds on.  An empty REFLECTION is refused: the loop is the point."
  (interactive
   (list (read-string "What improved, what is next: ")))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer; org-learn-practice-end needs one"))
  (unless (ignore-errors (org-back-to-heading t))
    (user-error "No heading at point; org-learn-practice-end needs one"))
  (when (string= (string-trim reflection) "")
    (user-error "Empty reflection; the feedback loop is the point"))
  (let ((goal (or (org-entry-get (point) "LEARN_GOAL") "-")))
    (funcall org-learn-study-clock-out-function)
    (org-with-point-at (point)
      (goto-char (org-end-of-subtree t))
      (unless (bolp) (insert "\n"))
      (insert "** Practice " (format-time-string "%Y-%m-%d") "\n"
              "  Goal: " goal "\n"
              "  Reflection: " (string-trim reflection) "\n"))
    (message "org-learn: practice session filed")))

;;; Pomodoro cycles (spec 016).

(defcustom org-learn-pomodoro-focus 25
  "Focus block length in minutes for `org-learn-pomodoro'."
  :type 'natnum
  :group 'org-learn-study)

(defcustom org-learn-pomodoro-break 5
  "Break length in minutes for `org-learn-pomodoro'."
  :type 'natnum
  :group 'org-learn-study)

(defvar org-learn-pomodoro--state nil
  "Current pomodoro state, or nil when none runs.
A plist with :phase (focus or break), :done, :cycles, :focus and
:break minutes.")

(defun org-learn-pomodoro--notify (body)
  "Send BODY through the notify seam."
  (funcall org-learn-study-notify-function "org-learn pomodoro" body))

(defun org-learn-pomodoro--tick ()
  "Timer-done callback: advance the pomodoro state machine.
A finished focus block clocks out and either starts the break or
ends the run; a finished break starts the next focus block.  The
callback removes itself when the run ends."
  (when org-learn-pomodoro--state
    (let* ((state org-learn-pomodoro--state)
           (phase (plist-get state :phase)))
      (cond
       ((eq phase 'focus)
        (when (org-clocking-p)
          (funcall org-learn-study-clock-out-function))
        (let ((done (1+ (plist-get state :done))))
          (if (>= done (plist-get state :cycles))
              (progn
                (org-learn-pomodoro--notify
                 (format "Focus %d/%d done -- run complete, nice work."
                         done (plist-get state :cycles)))
                (setq org-learn-pomodoro--state nil)
                (remove-hook 'org-timer-done-hook
                             #'org-learn-pomodoro--tick))
            (setq org-learn-pomodoro--state
                  (plist-put (plist-put state :phase 'break) :done done))
            (org-learn-pomodoro--notify
             (format "Focus %d/%d done -- break for %d minutes."
                     done (plist-get state :cycles)
                     (plist-get state :break)))
            (funcall org-learn-study-timer-function
                     (plist-get state :break)))))
       ((eq phase 'break)
        (setq org-learn-pomodoro--state
              (plist-put state :phase 'focus))
        (org-learn-pomodoro--notify
         (format "Break over -- focus block %d/%d."
                 (1+ (plist-get state :done))
                 (plist-get state :cycles)))
        (funcall org-learn-study-timer-function
                 (plist-get state :focus)))))))

;;;###autoload
(defun org-learn-pomodoro (&optional cycles)
  "Run CYCLES pomodoro focus/break cycles over the entry at point.
Each focus block clocks in, through `org-learn-study-clock-function',
and lasts `org-learn-pomodoro-focus' minutes; each break lasts
`org-learn-pomodoro-break'.  Phase changes notify through
`org-learn-study-notify-function'.  CYCLES defaults to 2.  Cancel
with `org-learn-pomodoro-cancel'."
  (interactive "P")
  (when org-learn-pomodoro--state
    (user-error "A pomodoro run is already going"))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer; org-learn-pomodoro needs one"))
  (unless (ignore-errors (org-back-to-heading t))
    (user-error "No heading at point; org-learn-pomodoro needs one"))
  (let ((n (if (and cycles (> cycles 0)) cycles 2)))
    (setq org-learn-pomodoro--state
          (list :phase 'focus :done 0 :cycles n
                :focus org-learn-pomodoro-focus
                :break org-learn-pomodoro-break))
    (add-hook 'org-timer-done-hook #'org-learn-pomodoro--tick)
    (funcall org-learn-study-clock-function)
    (funcall org-learn-study-timer-function org-learn-pomodoro-focus)
    (org-learn-pomodoro--notify
     (format "Focus 1/%d -- %d minutes, go."
             n org-learn-pomodoro-focus))))

;;;###autoload
(defun org-learn-pomodoro-cancel ()
  "Cancel the running pomodoro and clock out."
  (interactive)
  (unless org-learn-pomodoro--state
    (user-error "No pomodoro running"))
  (setq org-learn-pomodoro--state nil)
  (remove-hook 'org-timer-done-hook #'org-learn-pomodoro--tick)
  (when (org-clocking-p)
    (funcall org-learn-study-clock-out-function))
  (org-learn-pomodoro--notify "Run cancelled."))

(provide 'org-learn-study)
;;; org-learn-study.el ends here
