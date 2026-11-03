;;; tests/test-org-learn-dashboard.el --- ERT suite for org-learn-dashboard -*- lexical-binding: t; -*-

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

;; ERT suite for the spec 005 dashboard
;; (specs/005-dashboard.org): the pure retention math, the batch-safe
;; `org-learn-dashboard--data' compute path on a seeded two-topic
;; temp Org file (counts, queue order, leeches, per-topic stats) and
;; the read-only dashboard buffer (section headers, commands legend,
;; friendly empty states, idempotent rebuild).  Loaded by
;; tests/test-org-learn.el so the single CI entry runs everything.

;;; Code:

(require 'ert)
(require 'org-learn)
(require 'org-learn-dashboard)

(defun org-learn-dashboard-test--count-matches (regexp string)
  "Count non-overlapping REGEXP matches in STRING (line-anchored)."
  (let ((count 0))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq count (1+ count))))
    count))

(defun org-learn-dashboard-test--kill-file-buffer (file)
  "Kill FILE's buffer, leaving no modified state behind."
  (let ((buffer (get-file-buffer file)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(defun org-learn-dashboard-test--kill-dashboard ()
  "Kill the dashboard buffer, leaving no litter behind."
  (let ((buffer (get-buffer org-learn-dashboard--buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer))))

;;; Retention math (pure, spec 005 acceptance rows).

(ert-deftest org-learn-dashboard-retention-math ()
  "Spec 005: retention = 1 - lapses/reps as a percent, nil at zero reps."
  (should (= 100 (org-learn-dashboard-retention 0 4)))
  (should (= 75 (org-learn-dashboard-retention 1 4)))
  (should (= 25 (org-learn-dashboard-retention 3 4)))
  (should (null (org-learn-dashboard-retention 0 0))))

;;; Compute path (spec 005: correct counts and queue order).

(ert-deftest org-learn-dashboard-data-counts-and-queue-order ()
  "Spec 005: dashboard--data returns correct counts and queue order \
on a seeded two-topic file."
  (let* ((file (make-temp-file "org-learn-dash-" nil ".org"))
         (today (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (today-date (format-time-string "%Y-%m-%d"))
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert (format "\
* Alpha

** due-old
SCHEDULED: <2020-01-01 Wed>
  :PROPERTIES:
  :LEARN_REPS: 2
  :LEARN_LAPSES: 1
  :LEARN_EASE: 2.3
  :END:
   old body

** due-recent
SCHEDULED: <2025-06-01 Sun>
  :PROPERTIES:
  :LEARN_REPS: 1
  :END:
   recent body

** future-item
SCHEDULED: <2030-01-01 Tue>
  :PROPERTIES:
  :LEARN_REPS: 0
  :END:
   future body

** new-item
   a brand new item body

** done-today
SCHEDULED: <2030-01-01 Tue>
  :PROPERTIES:
  :LEARN_REPS: 3
  :END:
  :LOGBOOK:
- Note taken on %s \\\\
  Reviewed with quality 5.
  :END:
   done body

* Beta

** leech-card :leech:
SCHEDULED: <2021-01-01 Fri>
  :PROPERTIES:
  :LEARN_REPS: 20
  :LEARN_LAPSES: 15
  :END:
   leech body

** plain-card
SCHEDULED: <2022-02-02 Wed>
  :PROPERTIES:
  :LEARN_REPS: 5
  :END:
   plain body
" today))
            (let ((data (org-learn-dashboard--data (list file))))
              ;; Today: 4 due (oldest first below), 1 new, 1 done.
              (should (= 4 (plist-get data :due)))
              (should (= 1 (plist-get data :new)))
              (should (= 1 (plist-get data :done-today)))
              ;; Queue: due items, oldest SCHEDULED first.
              (should (equal (mapcar (lambda (item)
                                       (plist-get item :title))
                                     (plist-get data :queue))
                             (list "due-old" "leech-card" "plain-card"
                                   "due-recent")))
              (should (equal (mapcar (lambda (item)
                                       (plist-get item :scheduled))
                                     (plist-get data :queue))
                             (list "<2020-01-01 Wed>" "<2021-01-01 Fri>"
                                   "<2022-02-02 Wed>" "<2025-06-01 Sun>")))
              ;; Leeches: title/lapse pairs for the leech-tagged item.
              (should (equal (plist-get data :leeches)
                             (list (cons "leech-card" 15))))
              ;; Stats: one entry per level-1 topic, file order.
              (let ((stats (plist-get data :stats)))
                (should (= 2 (length stats)))
                (let ((alpha (nth 0 stats))
                      (beta (nth 1 stats)))
                  (should (equal (plist-get alpha :topic) "Alpha"))
                  (should (= 4 (plist-get alpha :items)))
                  (should (= 2 (plist-get alpha :due)))
                  (should (< (abs (- (plist-get alpha :ease) 2.3)) 1e-9))
                  (should (= 83 (plist-get alpha :retention)))
                  (should (equal (plist-get alpha :last) today-date))
                  (should (equal (plist-get beta :topic) "Beta"))
                  (should (= 2 (plist-get beta :items)))
                  (should (= 2 (plist-get beta :due)))
                  (should-not (plist-get beta :ease))
                  (should (= 40 (plist-get beta :retention)))
                  (should-not (plist-get beta :last)))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

;;; Buffer independence (regression, 2026-11-03 CI red).

(ert-deftest org-learn-dashboard-topic-stats-reads-the-entry-buffer ()
  "Spec 005 regression: topic subtree extents are read in the \
entry's own file buffer, never in whatever buffer is current.
The scan captures plain positions, so `org-end-of-subtree' must
run with the file's buffer selected; running it in the current
buffer counts the wrong items and crashed two CI suites on
Emacs 29 (args-out-of-range on a killed buffer).  Seed two
topics, kill the file buffer, select a tiny unrelated Org
buffer, and demand the exact per-topic counts."
  (let* ((file (make-temp-file "org-learn-dashbufdep-" nil ".org"))
         (org-learn-file file)
         (org-learn-review-scope 'file))
    (unwind-protect
        (progn
          (write-region "\
* Alpha
** TODO Child one :drill:
SCHEDULED: <2020-01-01 Wed>
:PROPERTIES:
:LEARN_REPS: 4
:LEARN_LAPSES: 1
:LEARN_EASE: 2.3
:END:
** TODO Child two :drill:
SCHEDULED: <2020-01-02 Thu>
* Beta
** TODO Only child :drill:
SCHEDULED: <2020-01-03 Fri>
" nil file nil 'quiet)
          ;; Kill the file buffer and select a tiny unrelated Org
          ;; buffer: the stats must still read the FILE, not this one.
          (org-learn-dashboard-test--kill-file-buffer file)
          (with-temp-buffer
            (org-mode)
            (insert "* U\n")
            (let* ((data (org-learn-dashboard--data (list file)))
                   (stats (plist-get data :stats))
                   (alpha (nth 0 stats))
                   (beta (nth 1 stats)))
              (should (equal "Alpha" (plist-get alpha :topic)))
              (should (= 2 (plist-get alpha :items)))
              (should (= 2 (plist-get alpha :due)))
              (should (equal "Beta" (plist-get beta :topic)))
              (should (= 1 (plist-get beta :items))))))
      (org-learn-dashboard-test--kill-file-buffer file)
      (org-learn-dashboard-test--kill-dashboard)
      (when (file-exists-p file)
        (delete-file file)))))

;;; Presentation (spec 005: read-only buffer, headers, legend).

(ert-deftest org-learn-dashboard-builds-a-read-only-buffer ()
  "Spec 005: the dashboard buffer builds read-only with section \
headers, the commands legend and friendly empty states, rebuilt \
from scratch each run."
  (let* ((file (make-temp-file "org-learn-dashbuf-" nil ".org"))
         (org-learn-file file)
         (org-learn-review-scope 'file))
    (unwind-protect
        (progn
          ;; Empty file: sections exist, empty states are friendly.
          (write-region "" nil file nil 'quiet)
          (org-learn-dashboard)
          (with-current-buffer org-learn-dashboard--buffer
            (should buffer-read-only)
            (let ((text (buffer-string)))
              (should (string-match-p "^\\* Today" text))
              (should (string-match-p "^\\* Review queue" text))
              (should (string-match-p "^\\* Leeches" text))
              (should (string-match-p "^\\* Stats" text))
              (should (string-match-p "^\\* Commands" text))
              (should (string-match-p "(nothing due)" text))
              (should (string-match-p "(no leeches)" text))
              (should (string-match-p "(no topics yet)" text))
              (should (string-match-p "M-x org-learn-review" text))
              (should (string-match-p "M-x org-learn-drill" text))
              (should (string-match-p "M-x org-learn-study" text))
              (should (string-match-p "M-x org-learn-ai-ask" text))))
          ;; Second run over seeded content: rebuilt from scratch (one
          ;; Today header, counts computed), still read-only.
          (org-learn-dashboard-test--kill-file-buffer file)
          (write-region "\
* Topic

** due card
SCHEDULED: <2020-01-01 Wed>
   remember me
" nil file nil 'quiet)
          (org-learn-dashboard)
          (with-current-buffer org-learn-dashboard--buffer
            (should buffer-read-only)
            (let ((text (buffer-string)))
              (should (= 1 (org-learn-dashboard-test--count-matches
                            "^\\* Today$" text)))
              (should (string-match-p "^- Due now: 1$" text))
              (should (string-match-p "\\[\\[file:.*\\]\\[due card\\]\\]"
                                      text)))))
      (org-learn-dashboard-test--kill-dashboard)
      (org-learn-dashboard-test--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

;;; Command and boundary law.

(ert-deftest org-learn-dashboard-commands-are-commands ()
  "Spec 005: the dashboard and study entry points are commands."
  (should (commandp 'org-learn-dashboard))
  (should (commandp 'org-learn-study)))

(ert-deftest org-learn-dashboard-keeps-the-org-roadmap-boundary ()
  "Spec 005: the dashboard feature never loads org-roadmap either."
  (should-not (featurep 'org-roadmap)))

(provide 'test-org-learn-dashboard)
;;; test-org-learn-dashboard.el ends here
