;;; tests/test-org-learn-discipline.el --- ERT suite for org-learn-discipline -*- lexical-binding: t; -*-

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

;; Spec 017 acceptance: the compute path is pure over file lists and
;; every fact is derived from the spec 002 LOGBOOK evidence.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-learn)
(require 'org-learn-review)
(require 'org-learn-discipline)

(defun org-learn-discipline-test--temp-file (text)
  "Write TEXT to a fresh Org temp file and return its name."
  (let ((file (make-temp-file "ol-disc-" nil ".org")))
    (with-temp-buffer
      (insert text)
      (write-region (point-min) (point-max) file nil 0))
    file))

(defun org-learn-discipline-test--entry-with-notes ()
  "Return Org text with two review notes logged today."
  (let ((today (format-time-string "%Y-%m-%d %a")))
    (format "* TODO Item one
  body
  :LOGBOOK:
  - Note taken on [%s 10:00] \\\n    Reviewed with quality 4; next review in 3 days.
  - Note taken on [%s 11:00] \\\n    Reviewed with quality 2; next review in 1 day.
  :END:
" today today)))

(ert-deftest org-learn-discipline-review-days-parses-logbook ()
  "Two of today's review notes count as one day with two reviews;
a non-review note never counts."
  (let* ((file (org-learn-discipline-test--temp-file
                (concat (org-learn-discipline-test--entry-with-notes)
                        "* TODO Other
  :LOGBOOK:
  - Note taken on [" (format-time-string "%Y-%m-%d %a") " 12:00] \\\n    State changed.
  :END:
")))
         (days (org-learn-discipline--review-days (list file))))
    (should (equal days
                   (list (cons (format-time-string "%Y-%m-%d") 2))))))

(ert-deftest org-learn-discipline-streak-live ()
  "Reviews yesterday and today: streak 2, done today."
  (let ((days (list (cons (org-learn-discipline--day-string 1) 3)
                    (cons (org-learn-discipline--day-string 0) 2))))
    (should (equal (org-learn-discipline--streak-from days)
                   (list :streak 2 :best 2 :today 2 :done-today t)))))

(ert-deftest org-learn-discipline-streak-alive-through-yesterday ()
  "Today empty so far: the run stays alive through yesterday."
  (let* ((days (list (cons (org-learn-discipline--day-string 3) 1)
                     (cons (org-learn-discipline--day-string 2) 1)
                     (cons (org-learn-discipline--day-string 1) 1)))
         (facts (org-learn-discipline--streak-from days)))
    (should (= (plist-get facts :streak) 3))
    (should (null (plist-get facts :done-today)))
    (should (null (plist-get facts :today)))))

(ert-deftest org-learn-discipline-streak-best-with-gap ()
  "The best run survives a gap the live streak refuses to cross."
  (let* ((days (list (cons (org-learn-discipline--day-string 10) 1)
                     (cons (org-learn-discipline--day-string 6) 1)
                     (cons (org-learn-discipline--day-string 5) 1)))
         (facts (org-learn-discipline--streak-from days)))
    (should (= (plist-get facts :best) 2))
    (should (= (plist-get facts :streak) 0))))

(ert-deftest org-learn-discipline-chain-shape ()
  "The chain covers 28 days ending today, zero cells included."
  (let* ((days (list (cons (org-learn-discipline--day-string 0) 2)
                     (cons (org-learn-discipline--day-string 1) 1)))
         (chain (org-learn-discipline--chain days)))
    (should (= (length chain) 28))
    (should (equal (plist-get (car (last chain)) :date)
                   (format-time-string "%Y-%m-%d")))
    (should (equal (plist-get (car (last chain)) :count) 2))
    (should (zerop (plist-get (car chain) :count))))

  (should (string= (org-learn-discipline--chain-cell
                    (list :date "x" :count 0))
                   "."))
  (should (string= (org-learn-discipline--chain-cell
                    (list :date "x" :count 12))
                   "9")))

(ert-deftest org-learn-discipline-forecast-buckets ()
  "Future SCHEDULED dates land in their day buckets; today never
counts as future."
  (let* ((file (org-learn-discipline-test--temp-file
                (format "* TODO Soon
SCHEDULED: %s
* TODO Later
SCHEDULED: %s
* TODO Today
SCHEDULED: %s
"
                        (org-learn-review--org-date 3)
                        (org-learn-review--org-date 3)
                        (org-learn-review--org-date 0))))
         (forecast (org-learn-discipline--forecast (list file))))
    (should (equal (cdr (assoc 3 forecast)) 2))
    (should (zerop (cdr (assoc 1 forecast))))
    (should (= (length forecast) 7))))

(ert-deftest org-learn-discipline-minutes-today ()
  "Today's CLOCK lines sum in minutes; yesterday's never leak in."
  (let* ((today (format-time-string "%Y-%m-%d %a"))
         (yesterday (org-learn-discipline--day-string 1))
         (file (org-learn-discipline-test--temp-file
                (format "* TODO Studied
  :LOGBOOK:
  CLOCK: [%s 12:00]--[%s 12:25]
  CLOCK: [%s 12:30]--[%s 12:40]
  :END:
" today today yesterday yesterday))))
    (should (= (org-learn-discipline--minutes-today (list file)) 25))))

(ert-deftest org-learn-discipline-dashboard-lines ()
  "Goals, streak and time always render; integrations only with
their facts present -- the silence law."
  (let ((lines (org-learn-discipline-dashboard-lines
                (list :done-today 4 :goal-reviews 10
                      :streak 3 :best 7
                      :minutes-today 12 :goal-minutes 25))))
    (should (member "- Reviews today: 4/10" lines))
    (should (member "- Streak: 3 days (best 7)" lines))
    (should (member "- Study time today: 12/25 min" lines))
    (should (null (seq-filter
                   (lambda (line) (string-match-p "Habit" line))
                   lines)))
    (should (null (seq-filter
                   (lambda (line) (string-match-p "org-activity" line))
                   lines))))
  (let ((lines (org-learn-discipline-dashboard-lines
                (list :done-today 4 :goal-reviews 10
                      :streak 1 :best 1
                      :minutes-today 12 :goal-minutes 25
                      :habit (list :name "org-learn" :streak 9
                                   :score 77 :done t)
                      :activity 90))))
    (should (member "- Streak: 1 day (best 1)" lines))
    (should (member
             "- Habit org-learn: 9d streak, 77% score, done today" lines))
    (should (member "- Emacs time today (org-activity): 90 min" lines))))

(ert-deftest org-learn-discipline-data-smoke ()
  "The data plist derives from one file's evidence; integrations
stay nil without their packages."
  (let* ((file (org-learn-discipline-test--temp-file
                (org-learn-discipline-test--entry-with-notes)))
         (data (org-learn-discipline--data (list file))))
    (should (= (plist-get data :done-today) 2))
    (should (= (plist-get data :streak) 1))
    (should (null (plist-get data :habit)))
    (should (null (plist-get data :activity)))
    (should (= (length (plist-get data :chain)) 28))))

(ert-deftest org-learn-discipline-report-renders ()
  "The report buffer carries the chain and forecast sections."
  (let* ((file (org-learn-discipline-test--temp-file
                (org-learn-discipline-test--entry-with-notes)))
         (org-learn-file file)
         (org-learn-review-scope 'file))
    (cl-letf (((symbol-function 'pop-to-buffer) #'identity))
      (let ((buffer (org-learn-discipline-report)))
        (unwind-protect
            (with-current-buffer buffer
              (should (string-match-p "Don't break the chain"
                                      (buffer-string)))
              (should (string-match-p "Due forecast" (buffer-string)))
              (should (string-match-p "Reviews today: 2/10"
                                      (buffer-string))))
          (kill-buffer buffer))))))


(provide 'test-org-learn-discipline)
;;; test-org-learn-discipline.el ends here
