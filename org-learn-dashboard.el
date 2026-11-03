;;; org-learn-dashboard.el --- Study sessions and dashboard for org-learn -*- lexical-binding: t; -*-

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

;; Spec 005 (specs/005-dashboard.org): study sessions, stats and the
;; org-learn cockpit -- what is due today, how am I doing per topic,
;; where do I lose time.
;;
;; - `org-learn-study' clocks in on the heading at point and, when
;;   `org-learn-study-timer-minutes' is non-nil, starts an org-timer
;;   pomodoro.  The timer callback clocks out when it fires (one-shot
;;   hook entry, no advice, no permanent global hook); the LOGBOOK
;;   carries the evidence.
;; - `org-learn-dashboard' rebuilds the read-only *org-learn-dashboard*
;;   buffer from scratch each run: Today (due/new/done per
;;   `org-learn-review-scope'), the 10 oldest due items as org links,
;;   leeches with lapse counts, per-topic stats from the spec 002
;;   LEARN_* properties, and a commands legend.
;; - `org-learn-dashboard--data' is the pure, batch-safe compute path:
;;   it maps a file list to the dashboard data plist, and
;;   `org-learn-dashboard-retention' is the pure retention step
;;   (nil when nothing was reviewed yet).  Nothing new is stored.
;; - The Roadmap line (spec 011): when the roadmap bridge is loaded
;;   AND org-roadmap is present (both checked with `featurep' -- the
;;   dashboard itself never loads either), the data carries the
;;   spec 011 rollup of the scoped files' TODO trees and the render
;;   shows one "Roadmap: X/Y milestones done (Z%)" line (plus the
;;   next-review clause when the files schedule future reviews).
;;   Without roadmap nodes the line is silent.
;;
;; org-roadmap stays at the boundary: it is never required here
;; (AGENTS.md); the spec 011 Roadmap line only ever runs through
;; `featurep' guards into the already-loaded roadmap bridge.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'org-timer)
(require 'seq)
(require 'subr-x)
(require 'org-learn)
(require 'org-learn-review)

;; The Roadmap line (spec 011) is computed by the roadmap bridge, and
;; only when it is already loaded (see `org-learn-dashboard--data'):
;; no require happens here, the declarations keep the compiler quiet.
(declare-function org-learn-roadmap-rollup-files "org-learn-roadmap" (files))
(declare-function org-learn-roadmap-rollup-line
                  "org-learn-roadmap" (rollup &optional next-review-days))

(defgroup org-learn-dashboard nil
  "Study sessions and dashboard for org-learn."
  :group 'org-learn
  :prefix "org-learn-")

(defcustom org-learn-study-timer-minutes 25
  "Pomodoro length in minutes for `org-learn-study', or nil for no timer.
When the timer fires it clocks out the running study clock."
  :type '(choice (const :tag "No timer" nil) (natnum :tag "Minutes"))
  :group 'org-learn-dashboard)

(defconst org-learn-dashboard--buffer "*org-learn-dashboard*"
  "Name of the dashboard buffer.")

;;; Study sessions (spec 005: clocking + pomodoro).

(defun org-learn-study--timer-fired ()
  "One-shot timer callback: clock out, then remove this callback.
No permanent global hook is kept and nothing is advised."
  (remove-hook 'org-timer-done-hook #'org-learn-study--timer-fired)
  (when (org-clocking-p)
    (org-clock-out)))

;;;###autoload
(defun org-learn-study ()
  "Start a study session at the heading at point.
Clocks in on the entry and, when `org-learn-study-timer-minutes'
is non-nil, starts an org-timer pomodoro whose callback clocks out
when it fires.  The LOGBOOK carries the evidence."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer; org-learn-study needs one"))
  (unless (ignore-errors (org-back-to-heading t))
    (user-error "No heading at point; org-learn-study needs one"))
  (org-clock-in)
  (when org-learn-study-timer-minutes
    (add-hook 'org-timer-done-hook #'org-learn-study--timer-fired)
    (org-timer-set-timer org-learn-study-timer-minutes)))

;;; Pure compute path (spec 005: batch-safe, nothing stored).

(defun org-learn-dashboard-retention (lapses reps)
  "Return the retention percent for LAPSES and REPS.
Retention is 1 - lapses/reps as a rounded percent; nil when REPS
is zero (nothing was ever reviewed).  Pure."
  (when (> reps 0)
    (round (* 100 (- 1 (/ (float lapses) reps))))))

(defun org-learn-dashboard--last-review-time (pos)
  "Return the time of the entry's last LOGBOOK review note, or nil.
POS points at the entry heading in the current buffer."
  (save-match-data
    (org-with-point-at pos
      (let ((end (org-entry-end-position))
            (last nil))
        (save-excursion
          (while (re-search-forward
                  "- Note taken on \\(\\[[^]]+\\]\\)" end t)
            (setq last (match-string 1))))
        (when last
          (org-time-string-to-time last))))))

(defun org-learn-dashboard--scan-file (file)
  "Return the entry facts of FILE as a list of plists.
Each plist carries :title :level :pos :file :line :scheduled :reps
:lapses :ease :leech :has-body :last-review :reviewed-today.  Pure
data for `org-learn-dashboard--data'; no display happens here."
  (let ((buffer (find-file-noselect file))
        (today (format-time-string "%Y-%m-%d"))
        entries)
    (with-current-buffer buffer
      (org-with-point-at (point-min)
        (org-map-entries
         (lambda ()
           (let* ((scheduled (org-entry-get (point) "SCHEDULED"))
                  (reps (string-to-number
                         (or (org-entry-get (point) "LEARN_REPS") "0")))
                  (lapses (string-to-number
                           (or (org-entry-get (point) "LEARN_LAPSES") "0")))
                  (ease-string (org-entry-get (point) "LEARN_EASE"))
                  (last (org-learn-dashboard--last-review-time (point))))
             (push (list :title (org-get-heading t t t t)
                         :level (org-current-level)
                         :pos (point)
                         :file file
                         :line (line-number-at-pos (point))
                         :scheduled scheduled
                         :reps reps
                         :lapses lapses
                         :ease (and ease-string
                                    (string-to-number ease-string))
                         :leech (member "leech" (org-get-tags (point) t))
                         :has-body (org-learn-review--has-body-p)
                         :last-review last
                         :reviewed-today
                         (and last
                              (equal (format-time-string "%Y-%m-%d" last)
                                     today)))
                   entries)))
         nil 'file)))
    (nreverse entries)))

(defun org-learn-dashboard--due-p (entry)
  "Return non-nil when ENTRY is due (SCHEDULED <= today)."
  (and (plist-get entry :scheduled)
       (<= (time-to-days
            (org-time-string-to-time (plist-get entry :scheduled)))
           (org-today))))

(defun org-learn-dashboard--queue-item (entry)
  "Return the review-queue plist for a due ENTRY."
  (list :title (plist-get entry :title)
        :file (plist-get entry :file)
        :line (plist-get entry :line)
        :scheduled (plist-get entry :scheduled)))

(defun org-learn-dashboard--subtree-end (entry)
  "Return the end position of ENTRY's subtree in its own file.
`org-end-of-subtree' must run with the entry's buffer selected:
the scan records plain positions, so selecting the file's buffer
here keeps the computation independent of whatever buffer is
current when the stats are requested.  A hand-rolled ENTRY without
:file falls back to the current buffer (legacy behavior)."
  (let ((file (plist-get entry :file)))
    (if (null file)
        (org-with-point-at (plist-get entry :pos)
          (org-end-of-subtree))
      (with-current-buffer (find-file-noselect file)
        (org-with-point-at (plist-get entry :pos)
          (org-end-of-subtree))))))

(defun org-learn-dashboard--topic-stats (entries)
  "Return per-topic stat plists for the level-1 ENTRIES of one file.
Items are the in-system descendants of each topic (SCHEDULED or
LEARN_REPS present); ease averages the LEARN_EASE values, retention
aggregates lapses over reps and the last review is the newest
LOGBOOK note date.  Position comparisons stay within one file."
  (let ((topics (seq-filter (lambda (entry)
                              (= (plist-get entry :level) 1))
                            entries)))
    (mapcar
     (lambda (topic)
       (let* ((pos (plist-get topic :pos))
              (end (org-learn-dashboard--subtree-end topic))
              (items (seq-filter
                      (lambda (entry)
                        (and (> (plist-get entry :level) 1)
                             (>= (plist-get entry :pos) pos)
                             (< (plist-get entry :pos) end)
                             (or (plist-get entry :scheduled)
                                 (> (plist-get entry :reps) 0))))
                      entries))
              (eases (seq-map (lambda (entry) (plist-get entry :ease))
                              (seq-filter (lambda (entry)
                                            (plist-get entry :ease))
                                          items)))
              (reps (seq-reduce (lambda (sum entry)
                                  (+ sum (plist-get entry :reps)))
                                items 0))
              (lapses (seq-reduce (lambda (sum entry)
                                    (+ sum (plist-get entry :lapses)))
                                  items 0))
              (last (seq-reduce (lambda (newest entry)
                                  (let ((time (plist-get entry :last-review)))
                                    (cond
                                     ((not time) newest)
                                     ((or (not newest)
                                          (time-less-p newest time)) time)
                                     (t newest))))
                                items nil)))
         (list :topic (plist-get topic :title)
               :items (length items)
               :due (seq-count #'org-learn-dashboard--due-p items)
               :ease (and eases
                          (/ (apply #'+ eases) (float (length eases))))
               :retention (org-learn-dashboard-retention lapses reps)
               :last (and last (format-time-string "%Y-%m-%d" last)))))
     topics)))

(defun org-learn-dashboard--next-review-days (entries)
  "Return the days until the next future-scheduled ENTRY, or nil.
Pure over the scanned ENTRIES: the smallest SCHEDULED date strictly
after today, in days (the spec 011 `next review in Nd' clause)."
  (let ((today (org-today))
        (next nil))
    (dolist (entry entries)
      (let ((scheduled (plist-get entry :scheduled)))
        (when scheduled
          (let ((days (- (time-to-days
                          (org-time-string-to-time scheduled))
                         today)))
            (when (and (> days 0) (or (not next) (< days next)))
              (setq next days))))))
    next))

(defun org-learn-dashboard--data (files)
  "Return the dashboard data plist for the Org FILES.
Keys: :due :new :done-today :queue :leeches :stats :next-review
:roadmap.  Pure over the file list (no display, no interactive
APIs), so it is batch-safe and testable.  :roadmap carries the
spec 011 rollup of the files' TODO trees, computed through the
roadmap bridge ONLY when it and org-roadmap are already loaded
\(`featurep' guards, no require -- boundary law); nil otherwise,
which keeps the Roadmap line silent."
  (let* ((file-entries (mapcar (lambda (file)
                                 (cons file
                                       (org-learn-dashboard--scan-file file)))
                               files))
         (entries (apply #'append (mapcar #'cdr file-entries)))
         (due (seq-filter #'org-learn-dashboard--due-p entries))
         (due (sort due
                    (lambda (a b)
                      (time-less-p
                       (org-time-string-to-time (plist-get a :scheduled))
                       (org-time-string-to-time (plist-get b :scheduled))))))
         (new (seq-filter (lambda (entry)
                            (and (not (plist-get entry :scheduled))
                                 (plist-get entry :has-body)))
                          entries))
         (leeches (mapcar (lambda (entry)
                            (cons (plist-get entry :title)
                                  (plist-get entry :lapses)))
                          (seq-filter (lambda (entry)
                                        (plist-get entry :leech))
                                      entries)))
         (stats (apply #'append
                       (mapcar (lambda (file-entry)
                                 (org-learn-dashboard--topic-stats
                                  (cdr file-entry)))
                               file-entries)))
         (next-review (org-learn-dashboard--next-review-days entries))
         (roadmap (when (featurep 'org-learn-roadmap)
                    (org-learn-roadmap-rollup-files files))))
    (list :due (length due)
          :new (length new)
          :done-today (seq-count (lambda (entry)
                                   (plist-get entry :reviewed-today))
                                 entries)
          :queue (mapcar #'org-learn-dashboard--queue-item
                         (seq-take due 10))
          :leeches leeches
          :stats stats
          :next-review next-review
          :roadmap roadmap)))

;;; Presentation (read-only, rebuilt from scratch).

(defun org-learn-dashboard--render (data)
  "Insert the dashboard sections for DATA into the current buffer."
  (insert "#+TITLE: org-learn dashboard\n\n")
  ;; 1. Today
  (insert "* Today\n\n")
  (insert (format "- Due now: %d\n" (plist-get data :due)))
  (insert (format "- New: %d\n" (plist-get data :new)))
  (insert (format "- Done today: %d\n" (plist-get data :done-today)))
  ;; Roadmap (spec 011): one line, silent without roadmap nodes.
  (let ((rollup (plist-get data :roadmap)))
    (when rollup
      (insert "- " (org-learn-roadmap-rollup-line
                    rollup (plist-get data :next-review))
              "\n")))
  (insert "\n")
  ;; 2. Review queue
  (insert "* Review queue\n\n")
  (let ((queue (plist-get data :queue)))
    (if (null queue)
        (insert "(nothing due)\n\n")
      (dolist (item queue)
        (insert (format "- due %s: %s\n"
                        (plist-get item :scheduled)
                        (org-link-make-string
                         (format "file:%s::%d"
                                 (plist-get item :file)
                                 (plist-get item :line))
                         (plist-get item :title)))))
      (insert "\n")))
  ;; 3. Leeches
  (insert "* Leeches\n\n")
  (let ((leeches (plist-get data :leeches)))
    (if (null leeches)
        (insert "(no leeches)\n\n")
      (dolist (leech leeches)
        (insert (format "- %s: %d lapses\n" (car leech) (cdr leech))))
      (insert "\n")))
  ;; 4. Stats
  (insert "* Stats\n\n")
  (let ((stats (plist-get data :stats)))
    (if (null stats)
        (insert "(no topics yet)\n\n")
      (insert "| Topic | Items | Due | Avg ease | Retention | Last review |\n")
      (insert "|-------+-------+-----+----------+-----------+-------------|\n")
      (dolist (stat stats)
        (insert
         (format "| %s | %d | %d | %s | %s | %s |\n"
                 (plist-get stat :topic)
                 (plist-get stat :items)
                 (plist-get stat :due)
                 (or (and (plist-get stat :ease)
                          (format "%.2f" (plist-get stat :ease)))
                     "-")
                 (or (and (plist-get stat :retention)
                          (format "%d%%" (plist-get stat :retention)))
                     "-")
                 (or (plist-get stat :last) "-"))))
      (insert "\n")))
  ;; 5. Commands legend
  (insert "* Commands\n\n")
  (insert "- M-x org-learn-review — grade the due items (SM-2)\n")
  (insert "- M-x org-learn-drill — drill the flashcards\n")
  (insert "- M-x org-learn-study — clock a study session\n")
  (insert "- M-x org-learn-ai-ask — ask the AI tutor about a subtree\n"))

;;;###autoload
(defun org-learn-dashboard ()
  "Build and show the org-learn dashboard.
The read-only *org-learn-dashboard* buffer is rebuilt from scratch
each run: Today (due/new/done per `org-learn-review-scope'), the
review queue (10 oldest due items as org links), leeches, per-topic
stats and a commands legend.  Empty states render friendly lines."
  (interactive)
  (let* ((files (seq-filter #'file-exists-p
                            (org-learn-review--scoped-files)))
         (data (org-learn-dashboard--data files)))
    (with-current-buffer (get-buffer-create org-learn-dashboard--buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (org-learn-dashboard--render data))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'org-learn-dashboard)
;;; org-learn-dashboard.el ends here
