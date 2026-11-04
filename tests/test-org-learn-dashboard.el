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

;;; Mastery math (pure, spec 012 acceptance rows).

(ert-deftest org-learn-dashboard-mastery-math ()
  "Spec 012: mastery = rounded weighted mean, renormalized over \
the present components; nil at zero evidence.  Pinned rows."
  ;; 1. Worked example (spec): retention 75, leech 80, quiz 67 -> 74.
  (should (= 74 (org-learn-dashboard--mastery
                 (list :retention 75 :items 10 :leeches 2
                       :quizzes 6 :quiz-misses 2))))
  ;; 2. Retention only (renormalized single component): 40 -> 40.
  (should (= 40 (org-learn-dashboard--mastery
                 (list :retention 40 :items 0 :leeches 0
                       :quizzes 0 :quiz-misses 0))))
  ;; 3. Leech only: 2 leeches of 10 items -> 80.
  (should (= 80 (org-learn-dashboard--mastery
                 (list :retention nil :items 10 :leeches 2
                       :quizzes 0 :quiz-misses 0))))
  ;; 4. Quiz only: 2 misses of 6 attempts -> 67.
  (should (= 67 (org-learn-dashboard--mastery
                 (list :retention nil :items 0 :leeches 0
                       :quizzes 6 :quiz-misses 2))))
  ;; 5. Leech + quiz renormalized (0.5/0.5): 80 and 60 -> 70.
  (should (= 70 (org-learn-dashboard--mastery
                 (list :retention nil :items 10 :leeches 2
                       :quizzes 5 :quiz-misses 2))))
  ;; 6. Zero-evidence topic -> nil (unstarted, never a growth area).
  (should (null (org-learn-dashboard--mastery
                 (list :retention nil :items 0 :leeches 0
                       :quizzes 0 :quiz-misses 0))))
  ;; 7. Leech-fraction rounding: 1 leech of 3 items -> 67.
  (should (= 67 (org-learn-dashboard--mastery
                 (list :retention nil :items 3 :leeches 1
                       :quizzes 0 :quiz-misses 0))))
  ;; 8. All components perfect: 100.
  (should (= 100 (org-learn-dashboard--mastery
                  (list :retention 100 :items 4 :leeches 0
                        :quizzes 9 :quiz-misses 0)))))

;;; Growth areas (pure, spec 012: strict threshold, worst-first, ties).

(ert-deftest org-learn-dashboard-growth-areas-order-and-threshold ()
  "Spec 012: growth areas are strictly below the threshold,
worst-first, ties broken by title; nil on empty stats."
  (let ((stats (list (list :topic "Zeta" :retention 10 :items 0 :leeches 0
                           :quizzes 0 :quiz-misses 0 :file "a.org" :line 1)
                     (list :topic "Alpha" :retention 90 :items 0 :leeches 0
                           :quizzes 0 :quiz-misses 0 :file "a.org" :line 2)
                     (list :topic "Mid" :retention 50 :items 0 :leeches 0
                           :quizzes 0 :quiz-misses 0 :file "a.org" :line 3)
                     (list :topic "Tie-a" :retention 30 :items 0 :leeches 0
                           :quizzes 0 :quiz-misses 0 :file "a.org" :line 4)
                     (list :topic "Tie-b" :retention 30 :items 0 :leeches 0
                           :quizzes 0 :quiz-misses 0 :file "a.org" :line 5)
                     (list :topic "None" :retention nil :items 0 :leeches 0
                           :quizzes 0 :quiz-misses 0 :file "a.org" :line 6))))
    (let ((growth (org-learn-dashboard-growth-areas stats)))
      ;; Default threshold 50, strict <: Zeta, Tie-a, Tie-b grow;
      ;; Alpha 90 and Mid 50 (the boundary row) stay out; None is nil.
      (should (equal (list "Zeta" "Tie-a" "Tie-b")
                     (mapcar (lambda (row) (plist-get row :topic)) growth)))
      ;; Worst-first: 10, 30, 30; the 30 tie orders by title.
      (should (equal (list 10 30 30)
                     (mapcar (lambda (row) (plist-get row :mastery)) growth)))
      ;; Rows carry the link coordinates of the topic heading.
      (should (equal 4 (plist-get (nth 1 growth) :line)))
      (should (equal "a.org" (plist-get (nth 2 growth) :file)))))
  ;; Empty stats -> nil (the Roadmap line's silence law).
  (should (null (org-learn-dashboard-growth-areas nil))))

;;; Compute path (spec 012: quiz scan keys, sums, :growth).

(ert-deftest org-learn-dashboard-data-growth-and-quiz-scan ()
  "Spec 012: the scan parses LEARN_QUIZZES / LEARN_QUIZ_MISSES \
(missing -> 0), topic-stats sums them per topic with :leeches and \
the topic's own :file/:line, and --data carries :growth."
  (let* ((file (make-temp-file "org-learn-dash-growth-" nil ".org"))
         (org-learn-file file)
         (org-learn-review-scope 'file)
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert "\
* Alpha
** card one :leech:
SCHEDULED: <2020-01-01 Wed>
:PROPERTIES:
:LEARN_REPS: 6
:LEARN_LAPSES: 4
:LEARN_QUIZZES: 3
:LEARN_QUIZ_MISSES: 2
:END:
   body one

** card two
SCHEDULED: <2020-01-02 Thu>
:PROPERTIES:
:LEARN_REPS: 2
:LEARN_LAPSES: 0
:LEARN_QUIZZES: 5
:LEARN_QUIZ_MISSES: 4
:END:
   body two

* Beta
** card three
SCHEDULED: <2020-01-03 Fri>
:PROPERTIES:
:LEARN_REPS: 1
:END:
   body three
"))
          (let ((data (org-learn-dashboard--data (list file))))
            (let ((stats (plist-get data :stats))
                  (alpha-stats nil))
              (should (= 2 (length stats)))
              (setq alpha-stats (nth 0 stats))
              ;; Alpha: quiz sums 3+5, misses 2+4, 1 leech of 2 items.
              (should (equal "Alpha" (plist-get alpha-stats :topic)))
              (should (= 2 (plist-get alpha-stats :items)))
              (should (= 1 (plist-get alpha-stats :leeches)))
              (should (= 8 (plist-get alpha-stats :quizzes)))
              (should (= 6 (plist-get alpha-stats :quiz-misses)))
              ;; The topic's own location rides the stat plist.
              (should (equal file (plist-get alpha-stats :file)))
              (should (= 1 (plist-get alpha-stats :line)))
              ;; Beta: no quiz properties anywhere -> 0 (missing -> 0).
              (let ((beta-stats (nth 1 stats)))
                (should (equal "Beta" (plist-get beta-stats :topic)))
                (should (= 0 (plist-get beta-stats :quizzes)))
                (should (= 0 (plist-get beta-stats :quiz-misses)))
                (should (= 21 (plist-get beta-stats :line))))
              ;; Alpha mastery: retention 50 (4 lapses of 8 reps),
              ;; leech 50 (1 of 2), quiz 25 (6 misses of 8) -> 44.
              ;; Beta mastery 100 (perfect retention + no leeches).
              (let ((growth (plist-get data :growth)))
                (should (= 1 (length growth)))
                (should (equal "Alpha" (plist-get (nth 0 growth) :topic)))
                (should (= 44 (plist-get (nth 0 growth) :mastery)))
                (should (equal file (plist-get (nth 0 growth) :file)))
                (should (= 1 (plist-get (nth 0 growth) :line)))))))
      (org-learn-dashboard-test--kill-file-buffer file)
      (org-learn-dashboard-test--kill-dashboard)
      (when (file-exists-p file)
        (delete-file file)))))

;;; Presentation (spec 012: the Growth areas bullet).

(ert-deftest org-learn-dashboard-renders-growth-areas ()
  "Spec 012: one Growth areas bullet after the Roadmap line, \
worst-first links with percents; silent when empty."
  (let* ((file (make-temp-file "org-learn-dashgrow-" nil ".org"))
         (org-learn-file file)
         (org-learn-review-scope 'file))
    (unwind-protect
        (progn
          ;; Seeded: Alpha weak (mastery 44), Beta perfect (100).
          (write-region "\
* Alpha
** card one :leech:
SCHEDULED: <2020-01-01 Wed>
:PROPERTIES:
:LEARN_REPS: 6
:LEARN_LAPSES: 4
:LEARN_QUIZZES: 3
:LEARN_QUIZ_MISSES: 2
:END:
   body one

** card two
SCHEDULED: <2020-01-02 Thu>
:PROPERTIES:
:LEARN_REPS: 2
:LEARN_LAPSES: 0
:LEARN_QUIZZES: 5
:LEARN_QUIZ_MISSES: 4
:END:
   body two

* Beta
** card three
SCHEDULED: <2020-01-03 Fri>
:PROPERTIES:
:LEARN_REPS: 1
:END:
   body three
" nil file nil 'quiet)
          (org-learn-dashboard)
          (with-current-buffer org-learn-dashboard--buffer
            (let ((text (buffer-string)))
              (should buffer-read-only)
              ;; The bullet carries the link and the mastery percent.
              (should (string-match-p
                       "^- Growth areas: \\[\\[file:.*::1\\]\\[Alpha\\]\\] 44%$"
                       text))
              ;; It sits in Today, after the counters, before the queue.
              (should (< (string-match "- Done today" text)
                         (string-match "- Growth areas" text)
                         (string-match "^\\* Review queue" text)))))
          ;; Empty file -> no Growth areas line at all (silence law).
          (org-learn-dashboard-test--kill-file-buffer file)
          (write-region "" nil file nil 'quiet)
          (org-learn-dashboard)
          (with-current-buffer org-learn-dashboard--buffer
            (should-not (string-match-p "Growth areas"
                                        (buffer-string)))))
      (org-learn-dashboard-test--kill-dashboard)
      (org-learn-dashboard-test--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

(provide 'test-org-learn-dashboard)
;;; test-org-learn-dashboard.el ends here
