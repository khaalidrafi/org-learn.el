;;; org-learn-discipline.el --- Discipline engine for org-learn -*- lexical-binding: t; -*-

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

;; Spec 017 (specs/017-discipline.org): the discipline layer -- daily
;; goals, streaks, the don't-break-the-chain strip, a due forecast,
;; and the sibling-package integrations.  The motivation research is
;; cited honestly: streaks and chain calendars are habit tools (the
;; "don't break the chain" popularized by Seinfeld; habit formation
;; via implementation intentions, Gollwitzer 1999), not learning
;; science per se -- the learning science is spec 016's techniques;
;; this module makes showing up every day visible.
;;
;; - `org-learn-discipline--review-days' scans the spec 002 LOGBOOK
;;   review notes into an alist of (DATE . COUNT), oldest first.
;; - `org-learn-discipline--streak-from' turns that into the streak
;;   facts: consecutive review days ending today, the best run, and
;;   whether today already counts.
;; - `org-learn-discipline--forecast' buckets future SCHEDULED dates
;;   into per-day due counts.
;; - `org-learn-discipline--minutes-today' sums today's CLOCK time
;;   over the scoped files -- `org-learn-study' and the practice
;;   sessions clock in, so the evidence is already in the LOGBOOKs.
;; - Integrations follow the Roadmap-line law: loophabits (streak
;;   and score of one habit, by default the \"org-learn\" habit) and
;;   org-activity (Emacs minutes today) contribute lines ONLY when
;;   their features are already loaded -- nothing is ever required
;;   here, and the lines stay silent otherwise.
;; - `org-learn-discipline-report' renders the report buffer; the
;;   dashboard's Today section gains the same lines through
;;   `org-learn-discipline-dashboard-lines', which dashboard.el calls
;;   featurep-guarded (the spec 011 pattern).
;;
;; Everything in the compute path is pure over a file list, so it is
;; batch-safe and testable.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org-learn)
(require 'org-learn-review)

;; Optional integrations, loaded lazily by their owners; the
;; declarations keep the compiler quiet without requiring anything.
(declare-function loophabits-find "loophabits" (name))
(declare-function loophabits-current-streak "loophabits" (habit))
(declare-function loophabits-score "loophabits" (habit))
(declare-function lh-completed-p "loophabits" (habit date))
(declare-function lh-today "loophabits" ())
(declare-function org-activity--today-total "org-activity" ())

(defgroup org-learn-discipline nil
  "Discipline engine for org-learn: goals, streaks, integrations."
  :group 'org-learn
  :prefix "org-learn-")

(defcustom org-learn-goal-reviews 10
  "Daily goal: reviews graded per day."
  :type 'natnum
  :group 'org-learn-discipline)

(defcustom org-learn-goal-minutes 25
  "Daily goal: minutes clocked on learning per day."
  :type 'natnum
  :group 'org-learn-discipline)

(defcustom org-learn-discipline-habit "org-learn"
  "loophabits habit name shown in the discipline lines.
Set to nil to hide the habit line even when loophabits is loaded."
  :type '(choice (const :tag "No habit line" nil)
                 (string :tag "Habit name"))
  :group 'org-learn-discipline)

(defconst org-learn-discipline--chain-days 28
  "Days shown in the don't-break-the-chain strip.")

;;; Review history (spec 002 LOGBOOK notes -> day counts).

(defun org-learn-discipline--review-days (files)
  "Return the review counts per day over FILES as an alist.
Each element is (DATE . COUNT) with DATE the YYYY-MM-DD of a spec
002 review note; the list is sorted by date, oldest first.  Pure
over the file list."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (file files)
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward
                  "- Note taken on \\[\\(20[0-9][0-9]-[01][0-9]-[0-3][0-9]\\)[^]]*\\] \\\\\n[ \t]+Reviewed with quality"
                  nil t)
            (let ((day (match-string 1)))
              (puthash day (1+ (gethash day counts 0)) counts))))))
    (let (out)
      (dolist (day (sort (hash-table-keys counts) #'string<))
        (push (cons day (gethash day counts)) out))
      (nreverse out))))

(defun org-learn-discipline--day-string (&optional offset)
  "Return the date OFFSET days before today as YYYY-MM-DD."
  (format-time-string
   "%Y-%m-%d" (time-subtract nil (* 86400 (or offset 0)))))

(defun org-learn-discipline--day-number (date)
  "Return DATE, a YYYY-MM-DD string, as an absolute day number.
Parsed by hand: `parse-iso8601-time-string' rejects date-only
strings on some builds (nil time fields), and streak math only
needs the day number anyway."
  (unless (string-match
           "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
           date)
    (error "org-learn: not a YYYY-MM-DD date: %s" date))
  (time-to-days
   (encode-time 0 0 0
                (string-to-number (match-string 3 date))
                (string-to-number (match-string 2 date))
                (string-to-number (match-string 1 date)))))

(defun org-learn-discipline--streak-from (days)
  "Return the streak plist for the review DAYS alist.
:streak counts consecutive review days ending today; when today has
no reviews yet the run may still be alive through yesterday -- the
day is not over -- so the count starts there instead.  :best is the
longest consecutive run in DAYS, :today today's review count and
:done-today non-nil when it is at least one."
  (let* ((today (org-learn-discipline--day-string))
         (streak 0)
         (best 0)
         (run 0)
         (prev-day-number nil))
    (dolist (pair days)
      (let ((day-number (org-learn-discipline--day-number (car pair))))
        (setq run (if (and prev-day-number
                           (= (- day-number prev-day-number) 1))
                      (1+ run)
                    1)
              prev-day-number day-number
              best (max best run))))
    (let ((offset (if (assoc today days) 0 1)))
      (while (assoc (org-learn-discipline--day-string offset) days)
        (setq streak (1+ streak)
              offset (1+ offset))))
    (list :streak streak
          :best best
          :today (cdr (assoc today days))
          :done-today (and (assoc today days) t))))

(defun org-learn-discipline--chain (days)
  "Return the last `org-learn-discipline--chain-days' chain cells.
Cells are plists of :date and :count, oldest first; today is last."
  (cl-loop for offset from (- org-learn-discipline--chain-days 1) downto 0
           collect (let ((date (org-learn-discipline--day-string offset)))
                     (list :date date
                           :count (or (cdr (assoc date days)) 0)))))

(defun org-learn-discipline--forecast (files &optional days)
  "Return the due forecast over FILES for the next DAYS days.
Each element is (OFFSET . COUNT) with OFFSET 1..DAYS; default 7.
Pure over the file list -- SCHEDULED dates only, no display."
  (let* ((n (or days 7))
         (buckets (make-vector n 0)))
    (dolist (file files)
      (when (file-exists-p file)
        (let ((buffer (find-file-noselect file)))
          (with-current-buffer buffer
            (org-with-point-at (point-min)
              (org-map-entries
               (lambda ()
                 (let ((scheduled (org-entry-get (point) "SCHEDULED")))
                   (when scheduled
                     (let ((offset
                            (- (time-to-days
                                (org-time-string-to-time scheduled))
                               (org-today))))
                       (when (and (> offset 0) (<= offset n))
                         (aset buckets (1- offset)
                               (1+ (aref buckets (1- offset)))))))))
               nil 'file))
            (kill-buffer)))))
    (cl-loop for i from 0 below n collect (cons (1+ i) (aref buckets i)))))

(defun org-learn-discipline--minutes-today (files)
  "Return the minutes clocked over FILES today.
`org-learn-study' and the practice sessions clock in, so the
LOGBOOK CLOCK lines are the evidence; the sum is `org-clock-sum's,
in minutes, over today's range."
  (let ((start (apply #'encode-time
                      (append '(0 0 0)
                              (nthcdr 3 (decode-time)))))
        (total 0))
    (dolist (file files)
      (when (file-exists-p file)
        (let ((buffer (find-file-noselect file)))
          (with-current-buffer buffer
            (org-clock-sum start)
            (setq total (+ total (or org-clock-file-total-minutes 0))))
          (kill-buffer buffer))))
    total))

;;; Integration lines (the Roadmap-line silence law).

(defun org-learn-discipline--habit-facts ()
  "Return the loophole habit facts, or nil when unavailable.
Keys: :name :streak :score :done.  Needs loophabits loaded AND a
habit named by `org-learn-discipline-habit' to exist; nil in every
other case, so the habit line stays silent instead of lying."
  (when (and (featurep 'loophabits)
             org-learn-discipline-habit)
    (let ((habit (loophabits-find org-learn-discipline-habit)))
      (when habit
        (list :name org-learn-discipline-habit
              :streak (loophabits-current-streak habit)
              :score (loophabits-score habit)
              :done (lh-completed-p habit (lh-today)))))))

(defun org-learn-discipline--activity-minutes ()
  "Return today's org-activity minutes, or nil when not running."
  (when (featurep 'org-activity)
    (round (/ (org-activity--today-total) 60.0))))

(defun org-learn-discipline--data (files)
  "Return the discipline plist for FILES.
Keys: :done-today :goal-reviews :streak :best :minutes-today
:goal-minutes :chain :forecast :habit :activity.  Pure over the
file list; the optional integrations contribute nil when their
packages are absent -- the silence law."
  (let* ((days (org-learn-discipline--review-days files))
         (streak (org-learn-discipline--streak-from days)))
    (list :done-today (or (plist-get streak :today) 0)
          :goal-reviews org-learn-goal-reviews
          :streak (plist-get streak :streak)
          :best (plist-get streak :best)
          :minutes-today (org-learn-discipline--minutes-today files)
          :goal-minutes org-learn-goal-minutes
          :chain (org-learn-discipline--chain days)
          :forecast (org-learn-discipline--forecast files)
          :habit (org-learn-discipline--habit-facts)
          :activity (org-learn-discipline--activity-minutes))))

(defun org-learn-discipline-dashboard-lines (data)
  "Return the Today-section lines for the discipline DATA plist.
Pure formatting: absent integration keys stay silent."
  (let ((lines nil))
    (push (format "- Reviews today: %d/%d"
                  (plist-get data :done-today)
                  (plist-get data :goal-reviews))
          lines)
    (push (format "- Streak: %d day%s (best %d)"
                  (plist-get data :streak)
                  (if (= (plist-get data :streak) 1) "" "s")
                  (plist-get data :best))
          lines)
    (push (format "- Study time today: %d/%d min"
                  (plist-get data :minutes-today)
                  (plist-get data :goal-minutes))
          lines)
    (let ((habit (plist-get data :habit)))
      (when habit
        (push (format "- Habit %s: %dd streak, %d%% score%s"
                      (plist-get habit :name)
                      (plist-get habit :streak)
                      (plist-get habit :score)
                      (if (plist-get habit :done) ", done today" ""))
              lines)))
    (let ((activity (plist-get data :activity)))
      (when activity
        (push (format "- Emacs time today (org-activity): %d min" activity)
              lines)))
    (nreverse lines)))

;;; Report buffer (read-only, rebuilt from scratch).

(defun org-learn-discipline--chain-cell (cell)
  "Return one chain-strip cell for CELL: a count digit or a dot."
  (if (zerop (plist-get cell :count))
      "."
    (number-to-string (min 9 (plist-get cell :count)))))

(defun org-learn-discipline--render (data)
  "Insert the discipline report for DATA into the current buffer."
  (insert "#+TITLE: org-learn discipline\n\n")
  (insert "* Today\n\n")
  (dolist (line (org-learn-discipline-dashboard-lines data))
    (insert line "\n"))
  (insert "\n* Don't break the chain\n\n")
  (insert "Last " (number-to-string org-learn-discipline--chain-days)
          " days, reviews per day (a dot is a day without reviews):\n\n")
  (insert "|")
  (dolist (cell (plist-get data :chain))
    (insert " " (org-learn-discipline--chain-cell cell) " |"))
  (insert "\n\n")
  (insert "* Due forecast\n\n")
  (dolist (bucket (plist-get data :forecast))
    (insert (format "- in %d day%s: %d due %s\n"
                    (car bucket)
                    (if (= (car bucket) 1) "" "s")
                    (cdr bucket)
                    (make-string (cdr bucket) ?#))))
  (insert "\n* Commands\n\n")
  (insert "- M-x org-learn-review-interleaved -- interleaved review session\n")
  (insert "- M-x org-learn-pomodoro -- focus/break cycles\n")
  (insert "- M-x org-learn-discipline-report -- this report\n"))

;;;###autoload
(defun org-learn-discipline-report ()
  "Build and show the org-learn discipline report.
Today's goals vs actual, the don't-break-the-chain strip of the
last 28 days, the 7-day due forecast, and the sibling-package lines
(loophabits habit, org-activity minutes -- silent when those are
not loaded)."
  (interactive)
  (let* ((files (seq-filter #'file-exists-p
                            (org-learn-review--scoped-files)))
         (data (org-learn-discipline--data files)))
    (with-current-buffer (get-buffer-create "*org-learn-discipline*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (org-learn-discipline--render data))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'org-learn-discipline)
;;; org-learn-discipline.el ends here
