;;; org-learn-review.el --- SM-2 review scheduler for org-learn -*- lexical-binding: t; -*-

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

;; Spec 002 (specs/002-review-scheduler.org): spaced repetition over
;; the org-learn notes file.  Any heading can become a review item; a
;; review session walks the due items and schedules the next review.
;;
;; - `org-learn-sm2' is the pure SM-2 step: ease updates first, then
;;   the next interval is computed from the updated ease.  Quality < 3
;;   is a lapse (reps reset, interval 1, ease drops by 0.20 with a
;;   1.3 floor).
;; - `org-learn-review--process-entry' grades the entry at point and
;;   writes the LEARN_* properties, SCHEDULED and a LOGBOOK note, all
;;   through the Org API (org-entry-put, org-log-beginning) -- no
;;   regex surgery on drawers.
;; - `org-learn-review' runs an interactive session per
;;   `org-learn-review-scope'; `org-learn-agenda-view' lists due and
;;   new items in an agenda buffer.
;;
;; Day granularity only, no timers; org-roadmap stays at the boundary
;; (never required here, AGENTS.md).

;;; Code:

(require 'org)
(require 'calendar)
(require 'seq)
(require 'subr-x)
(require 'org-learn)

(defgroup org-learn-review nil
  "SM-2 review scheduling for org-learn."
  :group 'org-learn
  :prefix "org-learn-")

(defcustom org-learn-review-scope 'file
  "Which files a `org-learn-review' session walks over.
`file' uses `org-learn-file' only, `agenda' uses
`org-learn-agenda-files', and `directory' uses every Org file under
the directory of `org-learn-file'."
  :type '(choice (const :tag "Notes file" file)
                 (const :tag "Agenda files" agenda)
                 (const :tag "Directory tree" directory))
  :group 'org-learn-review)

(defcustom org-learn-agenda-files nil
  "Org files scanned by `org-learn-agenda-view' and the agenda scope."
  :type '(repeat file)
  :group 'org-learn-review)

(defconst org-learn-review--min-ease 1.3
  "Lower bound for the SM-2 ease factor.")

;;; SM-2 (pure math, spec 002).

(defun org-learn-sm2 (ease interval reps quality)
  "Return the next SM-2 state for the given arguments as a plist.
EASE is the current ease factor, INTERVAL the current interval in
days, REPS the current successful repetition count and QUALITY the
recalled quality 0-5.  The result is a plist with keys :ease,
:interval and :reps.

Law (specs/002-review-scheduler.org): quality < 3 is a lapse (reps
0, interval 1, ease drops by 0.20 with floor 1.3); otherwise reps
grows by one and the interval is 1 day for reps 0, 6 days for reps
1, else the old interval times the UPDATED ease, rounded.  Pure:
no buffer, file or state is touched."
  (unless (and (integerp quality) (<= 0 quality 5))
    (error "SM-2 quality must be an integer 0-5, got %S" quality))
  (let ((ease (max org-learn-review--min-ease (float ease)))
        (interval (max 1 (floor interval)))
        (reps (max 0 (floor reps))))
    (if (< quality 3)
        (list :ease (max org-learn-review--min-ease (- ease 0.20))
              :interval 1
              :reps 0)
      (let* ((delta (- 0.1 (* (- 5 quality)
                              (+ 0.08 (* (- 5 quality) 0.02)))))
             (ease (max org-learn-review--min-ease (+ ease delta)))
             (interval (pcase reps
                         (0 1)
                         (1 6)
                         (_ (round (* interval ease))))))
        (list :ease ease :interval interval :reps (1+ reps))))))

;;; Entry plumbing (Org API only, no drawer regex).

(defun org-learn-review--org-date (days)
  "Return an Org timestamp string DAYS days from today."
  (let ((greg (calendar-gregorian-from-absolute (+ (org-today) days))))
    (format-time-string "<%Y-%m-%d %a>"
                        (encode-time 0 0 0
                                     (nth 1 greg) (nth 0 greg) (nth 2 greg)))))

(defun org-learn-review--entry-property-number (property default)
  "Return the LEARN PROPERTY of the entry at point as a number.
An absent or empty property yields DEFAULT."
  (let ((value (org-entry-get (point) property)))
    (if (and value (not (string= value "")))
        (string-to-number value)
      default)))

(defun org-learn-review--log-note (text)
  "Add a LOGBOOK note with TEXT to the entry at point."
  (let ((org-log-into-drawer t))
    (save-excursion
      (goto-char (org-log-beginning t))
      (insert (format "- Note taken on [%s] \\\n  %s\n"
                      (format-time-string "%Y-%m-%d %a %H:%M") text)))))

(defun org-learn-review--process-entry (quality)
  "Grade the entry at point with QUALITY and schedule the next review.
Applies `org-learn-sm2', writes the :LEARN_EASE:, :LEARN_INTERVAL:,
:LEARN_REPS: and :LEARN_LAPSES: properties plus SCHEDULED via
`org-entry-put', and appends a LOGBOOK note.  A quality below 3
counts as a lapse.  Returns the SM-2 result plist.  Point stays on
the heading."
  (org-back-to-heading t)
  (let* ((ease (org-learn-review--entry-property-number "LEARN_EASE" 2.5))
         (interval (org-learn-review--entry-property-number "LEARN_INTERVAL" 1))
         (reps (org-learn-review--entry-property-number "LEARN_REPS" 0))
         (lapses (org-learn-review--entry-property-number "LEARN_LAPSES" 0))
         (result (org-learn-sm2 ease interval reps quality))
         (new-ease (plist-get result :ease))
         (new-interval (plist-get result :interval))
         (new-reps (plist-get result :reps))
         (new-lapses (if (< quality 3) (1+ lapses) lapses)))
    (org-entry-put (point) "LEARN_EASE" (number-to-string new-ease))
    (org-entry-put (point) "LEARN_INTERVAL" (number-to-string new-interval))
    (org-entry-put (point) "LEARN_REPS" (number-to-string new-reps))
    (org-entry-put (point) "LEARN_LAPSES" (number-to-string new-lapses))
    (org-entry-put (point) "SCHEDULED" (org-learn-review--org-date new-interval))
    (org-learn-review--log-note
     (format "Reviewed with quality %d; next review in %d day%s."
             quality new-interval (if (= new-interval 1) "" "s")))
    result))

;;; Entry content display.

(defun org-learn-review--entry-content ()
  "Return the heading and body of the entry at point as a string.
Property drawers, planning lines and subheadings are excluded;
the result is \"TITLE\" alone when the entry has no body."
  (save-excursion
    (org-back-to-heading t)
    (let ((title (org-get-heading t t t t))
          (end (org-entry-end-position))
          (in-drawer nil)
          lines)
      (forward-line 1)
      (while (< (point) end)
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match-p "\\`[ \t]*:[A-Za-z][A-Za-z0-9_-]*:[ \t]*\\'" line)
            (setq in-drawer (not in-drawer)))
           ((and (not in-drawer) (looking-at-p "\\*"))
            (setq end (point)))   ; a subheading: body ends here
           ((or in-drawer
                (string-match-p "\\`\\(SCHEDULED\\|DEADLINE\\|CLOSED\\|CLOCK\\):"
                                line)))
           (t (push line lines))))
        (unless (= (point) end)
          (forward-line 1)))
      (setq lines (nreverse lines))
      (while (and lines (string-blank-p (car lines)))
        (setq lines (cdr lines)))
      (setq lines (nreverse lines))
      (while (and lines (string-blank-p (car lines)))
        (setq lines (cdr lines)))
      (if (null lines)
          title
        (concat title "\n" (string-join (nreverse lines) "\n"))))))

(defun org-learn-review--has-body-p ()
  "Return non-nil when the entry at point has text body content."
  (not (string= (org-learn-review--entry-content)
                (org-get-heading t t t t))))

;;; Due selection.

(defun org-learn-review--collect-items (buffer)
  "Return the review items in BUFFER as a list of markers.
Due items (SCHEDULED date <= today) come first, oldest SCHEDULED
first; new items (unscheduled, with body) follow in file order.
Items scheduled in the future are not selected."
  (with-current-buffer buffer
    (org-with-point-at (point-min)
      (let (due new)
        (org-map-entries
         (lambda ()
           (let* ((scheduled (org-entry-get (point) "SCHEDULED"))
                  (due-time (and scheduled (org-time-string-to-time scheduled))))
             (cond
              ((and due-time (<= (time-to-days due-time) (org-today)))
               (push (list (copy-marker (point)) due-time) due))
              ((and (not due-time) (org-learn-review--has-body-p))
               (push (list (copy-marker (point)) nil) new)))))
         nil 'file)
        (setq due (sort due
                        (lambda (a b)
                          (let ((ta (nth 1 a)) (tb (nth 1 b)))
                            (if (time-equal-p ta tb)
                                (< (marker-position (nth 0 a))
                                   (marker-position (nth 0 b)))
                              (time-less-p ta tb))))))
        (append due (nreverse new))))))

;;; Agenda view.

(defun org-learn-review--agenda-items (buffer)
  "Return the agenda view items in BUFFER as a list of plists.
Due items (LEARN_REPS set and SCHEDULED <= today) come first,
oldest SCHEDULED first; new items (LEARN_REPS missing or 0, never
scheduled in the future) follow in file order.  Each element is a
plist with :marker, :title, :reps and :scheduled."
  (with-current-buffer buffer
    (org-with-point-at (point-min)
      (let (due new)
        (org-map-entries
         (lambda ()
           (let* ((scheduled (org-entry-get (point) "SCHEDULED"))
                  (due-time (and scheduled
                                 (org-time-string-to-time scheduled)))
                  (due-p (and due-time
                              (<= (time-to-days due-time) (org-today))))
                  (reps (org-learn-review--entry-property-number
                         "LEARN_REPS" 0))
                  (title (org-get-heading t t t t))
                  (marker (copy-marker (point))))
             (cond
              ((and (> reps 0) due-p)
               (push (list :marker marker :title title
                           :reps reps :scheduled scheduled) due))
              ((and (= reps 0) (or (not due-time) due-p))
               (push (list :marker marker :title title
                           :reps reps :scheduled scheduled) new)))))
         nil 'file)
        (setq due (sort due
                        (lambda (a b)
                          (time-less-p
                           (org-time-string-to-time (plist-get a :scheduled))
                           (org-time-string-to-time (plist-get b :scheduled))))))
        (append due (nreverse new))))))

(defun org-learn-review--agenda-line (item)
  "Return one display line for agenda ITEM, a plist."
  (let* ((marker (plist-get item :marker))
         (buffer (and (markerp marker) (marker-buffer marker)))
         (file (and buffer (buffer-file-name buffer)))
         (line (and buffer
                    (with-current-buffer buffer
                      (count-lines (point-min) (marker-position marker)))))
         (link (org-link-make-string (format "file:%s::%d" file line)
                                     (plist-get item :title))))
    (if (plist-get item :scheduled)
        (format "- due %s, reps %d: %s"
                (plist-get item :scheduled) (plist-get item :reps) link)
      (format "- new: %s" link))))

;;;###autoload
(defun org-learn-agenda-view (&optional _match)
  "Show the org-learn review agenda.
Lists due items (LEARN_REPS set and SCHEDULED <= today, oldest
first) followed by new items (LEARN_REPS missing or 0) over
`org-learn-agenda-files'.  Works as a custom agenda command and as
a standalone command.  MATCH is the agenda match string and is
ignored."
  (interactive)
  (let* ((files (seq-filter #'file-exists-p
                            (mapcar #'expand-file-name
                                    org-learn-agenda-files)))
         (pre-existing (mapcar #'get-file-buffer files))
         (buffers (mapcar #'find-file-noselect files))
         items)
    (unwind-protect
        (progn
          (dolist (buffer buffers)
            (setq items (append items (org-learn-review--agenda-items buffer))))
          (with-current-buffer (get-buffer-create "*Org Learn Agenda*")
            (let ((inhibit-read-only t)
                  new-header-written)
              (erase-buffer)
              (org-mode)
              (insert "#+TITLE: org-learn review agenda\n\n* Due\n")
              (if (null items)
                  (insert "(nothing due)\n")
                (dolist (item items)
                  (unless (or (plist-get item :scheduled) new-header-written)
                    (insert "* New\n")
                    (setq new-header-written t))
                  (insert (org-learn-review--agenda-line item) "\n"))))
            (goto-char (point-min))
            (pop-to-buffer (current-buffer))))
      (dolist (item items)
        (let ((marker (plist-get item :marker)))
          (when (and (markerp marker) (marker-buffer marker))
            (set-marker marker nil))))
      (dolist (buffer buffers)
        (unless (memq buffer pre-existing)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

;;; Review session.

(defun org-learn-review--scoped-files ()
  "Return the list of Org files selected by `org-learn-review-scope'."
  (pcase org-learn-review-scope
    (`file (list (expand-file-name org-learn-file)))
    (`agenda (mapcar #'expand-file-name org-learn-agenda-files))
    (`directory
     (let ((dir (or (file-name-directory (expand-file-name org-learn-file))
                    default-directory)))
       (directory-files-recursively dir "\\.org\\'")))
    (_ (error "Unknown org-learn-review-scope value: %S"
              org-learn-review-scope))))

(defun org-learn-review--prompt-quality ()
  "Prompt for a review quality and return it.
Accepts the digits 0-5 (returned as an integer), the letter s
\(returned as `skip') and the letter q (returned as `quit').
Any other key re-prompts."
  (let (char result done)
    (while (not done)
      (setq char (read-char "Quality 0-5, s=skip, q=quit: ")
            done t
            result (cond
                    ((and (<= ?0 char) (<= char ?5)) (- char ?0))
                    ((eq char ?s) 'skip)
                    ((eq char ?q) 'quit)
                    (t (message "Please answer 0-5, s or q.")
                       (sit-for 1)
                       (setq done nil)))))
    result))

;;;###autoload
(defun org-learn-review ()
  "Run a review session over the due items per `org-learn-review-scope'.
For each due item the heading and body are shown (subheadings
hidden) and a quality 0-5 is prompted for; s skips the item, q
quits the session.  Each grade schedules the next review via
`org-learn-review--process-entry' and saves the file.  Helper
buffers opened by the session are killed afterwards."
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
          (catch 'org-learn-review-quit
            (dolist (item items)
              (let* ((marker (nth 0 item))
                     (content (with-current-buffer (marker-buffer marker)
                                (goto-char marker)
                                (org-learn-review--entry-content)))
                     (grade (progn
                              (with-output-to-temp-buffer "*Org Learn Review*"
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
                    (save-buffer))))))))
      (dolist (item items)
        (let ((marker (nth 0 item)))
          (when (and (markerp marker) (marker-buffer marker))
            (set-marker marker nil))))
      (let ((review (get-buffer "*Org Learn Review*")))
        (when review
          (kill-buffer review)))
      (dolist (buffer buffers)
        (unless (memq buffer pre-existing)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

;; Defined in org-agenda.el; declared to keep the compiler quiet --
;; org-agenda is never loaded eagerly from here.
(defvar org-agenda-custom-commands)

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-custom-commands
               '("L" "org-learn review agenda" org-learn-agenda-view)))

(provide 'org-learn-review)
;;; org-learn-review.el ends here
