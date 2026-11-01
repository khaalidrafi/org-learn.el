;;; tests/test-org-learn-review.el --- ERT suite for org-learn-review -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Khalid

;; Author: Khalid
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.0"))
;; Keywords: org, learning

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT suite for the spec 002 review scheduler
;; (specs/002-review-scheduler.org): the SM-2 math table, the
;; process-entry round trip on a temp Org file, the due selection
;; order, the scope custom and the agenda command.  Loaded by
;; tests/test-org-learn.el so the single CI entry runs everything.

;;; Code:

(require 'ert)
(require 'org-learn)
(require 'org-learn-review)

(defun org-learn-review-test--count-matches (regexp string)
  "Count non-overlapping REGEXP matches in STRING (line-anchored)."
  (let ((count 0))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq count (1+ count))))
    count))

(defun org-learn-review-test--org-date (days)
  "Return the Org timestamp for DAYS days from today, like <2026-11-02 Mon>."
  (let ((greg (calendar-gregorian-from-absolute (+ (org-today) days))))
    (format-time-string "<%Y-%m-%d %a>"
                        (encode-time 0 0 0
                                     (nth 1 greg) (nth 0 greg) (nth 2 greg)))))

(defun org-learn-review-test--titles (items)
  "Return the heading titles of review ITEMS, in order.
ITEMS are the (MARKER SCHEDULED-TIME) pairs returned by
`org-learn-review--collect-items'."
  (mapcar (lambda (item)
            (let ((marker (nth 0 item)))
              (with-current-buffer (marker-buffer marker)
                (goto-char marker)
                (org-get-heading t t t t))))
          items))

;;; SM-2 math table (spec 002 acceptance: exact values, 1e-9 slop).

;; Rows: EASE INTERVAL REPS QUALITY -> EASE' INTERVAL' REPS'.
;; The ease updates first (flat -0.20 drop for any q < 3, floor
;; 1.3); the interval for reps >= 2 multiplies the UPDATED ease
;; (see the rows with q=3, where the floored ease is the multiplier).
(defconst org-learn-review-test--sm2-table
  '(
    ;; q < 3: lapse -- reps 0, interval 1, ease - 0.20 (floor 1.3),
    ;; the same flat drop for every failing quality.
    (2.5  1 0 0  2.3  1 0)
    (2.5  1 0 1  2.3  1 0)
    (2.5  1 0 2  2.3  1 0)
    (1.35 6 2 0  1.3  1 0)
    (1.3  6 3 1  1.3  1 0)
    ;; q >= 3, reps 0 -> interval 1, reps 1.
    (2.5  1 0 3  2.36 1 1)
    (2.5  1 0 4  2.5  1 1)
    (2.5  1 0 5  2.6  1 1)
    ;; q >= 3, reps 1 -> interval 6, reps 2.
    (2.5  6 1 3  2.36 6 2)
    (2.5  6 1 4  2.5  6 2)
    (2.5  6 1 5  2.6  6 2)
    (1.3  6 1 3  1.3  6 2)
    ;; q >= 3, reps >= 2 -> round(interval * UPDATED ease).
    (2.36 14 2 3  2.22 31 3)
    (2.6  6  2 4  2.6  16 3)
    (2.6  20 2 5  2.7  54 3)
    (1.3  10 4 3 1.3  13 5)
    (1.3  10 5 4 1.3  13 6)
    (1.3  10 5 5 1.4  14 6))
  "Fixed SM-2 vectors: (EASE INTERVAL REPS QUALITY EASE' INTERVAL' REPS').")

(ert-deftest org-learn-review-sm2-math-table ()
  "Spec 002: the SM-2 vectors hold exactly (floats within 1e-9)."
  (pcase-dolist (`(,ease ,interval ,reps ,quality ,er ,ir ,rr)
                 org-learn-review-test--sm2-table)
    (let ((result (org-learn-sm2 ease interval reps quality)))
      (should (< (abs (- (plist-get result :ease) er)) 1e-9))
      (should (= (plist-get result :interval) ir))
      (should (= (plist-get result :reps) rr)))))

(ert-deftest org-learn-review-sm2-rejects-out-of-range-quality ()
  "Spec 002: quality outside 0-5 is an error; sm2 stays pure."
  (should-error (org-learn-sm2 2.5 1 0 6))
  (should-error (org-learn-sm2 2.5 1 0 -1))
  (should-error (org-learn-sm2 2.5 1 0 2.5)))

;;; process-entry round trip on a temp Org file.

(ert-deftest org-learn-review-process-entry-round-trip ()
  "Spec 002: grade 5 -> reps 1, interval 1, SCHEDULED tomorrow; \
grade 4 -> interval 6; grade 2 -> reps 0, lapses 1, ease dropped."
  (let* ((file (make-temp-file "org-learn-rev-" nil ".org"))
         (org-learn-file file)
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert "* TODO SM-2 item\n  body to remember\n")
            (goto-char (point-min))
            ;; First grade: fresh item -> reps 1, interval 1, tomorrow.
            (org-learn-review--process-entry 5)
            (should (equal (org-entry-get (point) "LEARN_REPS") "1"))
            (should (equal (org-entry-get (point) "LEARN_INTERVAL") "1"))
            (should (equal (org-entry-get (point) "LEARN_LAPSES") "0"))
            (should (< (abs (- (string-to-number
                                (org-entry-get (point) "LEARN_EASE"))
                               2.6))
                       1e-9))
            (should (equal (org-entry-get (point) "SCHEDULED")
                           (org-learn-review-test--org-date 1)))
            ;; Second grade: reps 1 -> interval 6.
            (org-learn-review--process-entry 4)
            (should (equal (org-entry-get (point) "LEARN_REPS") "2"))
            (should (equal (org-entry-get (point) "LEARN_INTERVAL") "6"))
            (should (< (abs (- (string-to-number
                                (org-entry-get (point) "LEARN_EASE"))
                               2.6))
                       1e-9))
            (should (equal (org-entry-get (point) "SCHEDULED")
                           (org-learn-review-test--org-date 6)))
            ;; Third grade: lapse -> reps 0, lapses 1, ease dropped.
            (org-learn-review--process-entry 2)
            (should (equal (org-entry-get (point) "LEARN_REPS") "0"))
            (should (equal (org-entry-get (point) "LEARN_INTERVAL") "1"))
            (should (equal (org-entry-get (point) "LEARN_LAPSES") "1"))
            (should (< (abs (- (string-to-number
                                (org-entry-get (point) "LEARN_EASE"))
                               2.4))
                       1e-9))
            (should (equal (org-entry-get (point) "SCHEDULED")
                           (org-learn-review-test--org-date 1)))
            ;; Three grades, three LOGBOOK notes, body intact.
            (should (= 3 (org-learn-review-test--count-matches
                          "^- Note taken on" (buffer-string))))
            (should (string-match-p "^  body to remember$" (buffer-string)))
            ;; Round-trip through disk.
            (save-buffer))
          (kill-buffer buffer)
          (setq buffer nil)
          (let ((content (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
            (should (string-match-p "^[ \t]*:LEARN_REPS: 0$" content))
            (should (string-match-p "^[ \t]*:LEARN_LAPSES: 1$" content))
            (should (string-match-p
                     (regexp-quote (concat "SCHEDULED: "
                                           (org-learn-review-test--org-date 1)))
                     content))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

;;; Due selection order.

(ert-deftest org-learn-review-due-selection-order ()
  "Spec 002: due items oldest SCHEDULED first, then new items; \
future items and unscheduled entries without body are excluded."
  (let* ((file (make-temp-file "org-learn-due-" nil ".org"))
         (today (format-time-string "<%Y-%m-%d %a>"))
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert (format "\
* TODO future
SCHEDULED: <2030-01-01 Tue>
  body

* TODO overdue-old
SCHEDULED: <2020-01-01 Wed>
  body

* TODO new-one
  has body, no schedule

* TODO due-today
SCHEDULED: %s
  body

* TODO overdue-recent
SCHEDULED: <2025-06-01 Sun>
  body

* TODO no-body

* TODO new-two
  another body
" today))
            (let ((items (org-learn-review--collect-items (current-buffer))))
              (should (equal (org-learn-review-test--titles items)
                             (list "overdue-old" "overdue-recent" "due-today"
                                   "new-one" "new-two"))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

;;; Agenda ordering.

(ert-deftest org-learn-review-agenda-ordering ()
  "Spec 002 agenda: due items with LEARN_REPS first (oldest first), \
then new items (LEARN_REPS missing or 0); future-scheduled excluded."
  (let* ((file (make-temp-file "org-learn-ag-" nil ".org"))
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert "\
* TODO done-item
SCHEDULED: <2020-01-01 Wed>
  :PROPERTIES:
  :LEARN_REPS: 2
  :END:
  body

* TODO fresh-item
  body, never reviewed

* TODO lapsed-item
SCHEDULED: <2021-01-01 Fri>
  :PROPERTIES:
  :LEARN_REPS: 0
  :END:
  body

* TODO future-item
SCHEDULED: <2030-01-01 Tue>
  :PROPERTIES:
  :LEARN_REPS: 3
  :END:
  body

* TODO due-recent
SCHEDULED: <2025-06-01 Sun>
  :PROPERTIES:
  :LEARN_REPS: 5
  :END:
  body
")
            (let ((items (org-learn-review--agenda-items (current-buffer))))
              (should (equal (mapcar (lambda (item)
                                       (plist-get item :title))
                                     items)
                             (list "done-item" "due-recent"
                                   "fresh-item" "lapsed-item"))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

;;; Scope and command law.

(ert-deftest org-learn-review-scope-defaults-to-file ()
  "Spec 002: the scope custom defaults to `file'; agenda files nil."
  (should (eq org-learn-review-scope 'file))
  (should (null org-learn-agenda-files)))

(ert-deftest org-learn-review-scope-is-honored ()
  "Spec 002: org-learn-review-scope selects the scanned files."
  (let* ((dir (make-temp-file "org-learn-scope-" t))
         (notes (expand-file-name "notes.org" dir))
         (f1 (expand-file-name "one.org" dir))
         (f2 (expand-file-name "sub/two.org" dir)))
    (unwind-protect
        (let ((org-learn-file notes)
              (org-learn-agenda-files (list f1 f2)))
          (make-directory (expand-file-name "sub" dir) t)
          (dolist (file (list notes f1 f2))
            (write-region "" nil file nil 'quiet))
          ;; file scope: only the notes file.
          (should (equal (org-learn-review--scoped-files) (list notes)))
          ;; agenda scope: the agenda files.
          (let ((org-learn-review-scope 'agenda))
            (should (equal (org-learn-review--scoped-files)
                           (list f1 f2))))
          ;; directory scope: every Org file under the notes directory.
          (let ((org-learn-review-scope 'directory))
            (should (equal
                     (sort (org-learn-review--scoped-files) #'string<)
                     (sort (list notes f1 f2) #'string<))))
          ;; unknown scopes error out.
          (let ((org-learn-review-scope 'bogus))
            (should-error (org-learn-review--scoped-files))))
      (delete-directory dir t))))

(ert-deftest org-learn-review-commands-are-commands ()
  "Spec 002: the review and agenda entry points are commands."
  (should (commandp 'org-learn-review))
  (should (commandp 'org-learn-agenda-view)))

(ert-deftest org-learn-review-keeps-the-org-roadmap-boundary ()
  "Spec 002: the review feature never loads org-roadmap either."
  (should-not (featurep 'org-roadmap)))

;;; Entry content display law.

(ert-deftest org-learn-review-entry-content-hides-subheadings ()
  "Spec 002: item content shows heading plus body only -- drawers, \
planning lines and subheadings are hidden."
  (let* ((file (make-temp-file "org-learn-cont-" nil ".org"))
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert "\
* TODO Parent
SCHEDULED: <2026-11-02 Mon>
  :PROPERTIES:
  :LEARN_REPS: 1
  :END:
  visible body line
** TODO Child
")
            (goto-char (point-min))
            (should (equal (org-learn-review--entry-content)
                           "Parent\n  visible body line"))
            ;; An entry without body content yields just the heading.
            (goto-char (point-min))
            (search-forward "** TODO Child")
            (beginning-of-line)
            (should (equal (org-learn-review--entry-content)
                           "Child"))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(provide 'test-org-learn-review)
;;; test-org-learn-review.el ends here
