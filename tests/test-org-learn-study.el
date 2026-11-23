;;; tests/test-org-learn-study.el --- ERT suite for org-learn-study -*- lexical-binding: t; -*-

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

;; Spec 016 acceptance: the interleaving math is pure and
;; deterministic; the Feynman, practice and pomodoro flows run
;; against seams only -- no live clock, timer or daemon.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-learn)
(require 'org-learn-study)

(defun org-learn-study-test--interleave-pairs ()
  "Return 8 test items: 4 math, 4 history, as (id . topic)."
  (append (mapcar (lambda (i) (cons i 'math)) '(1 2 3 4))
          (mapcar (lambda (i) (cons i 'hist)) '(5 6 7 8))))

(ert-deftest org-learn-study-shuffle-deterministic ()
  "Same seed, same order: sessions are reproducible per day."
  (should (equal (org-learn-study--shuffle '(a b c d e f) 42)
                 (org-learn-study--shuffle '(a b c d e f) 42)))
  (should-not (equal (org-learn-study--shuffle '(a b c d e f) 42)
                     (org-learn-study--shuffle '(a b c d e f) 43))))

(ert-deftest org-learn-study-shuffle-permutes ()
  "A shuffle is a permutation of its input, whatever the seed."
  (dolist (seed '(1 2 3 7 99 20261123))
    (should (equal (sort (org-learn-study--shuffle
                          '(1 2 3 4 5 6 7 8) seed)
                         #'<)
                   '(1 2 3 4 5 6 7 8)))))

(ert-deftest org-learn-study-interleave-preserves-and-spreads ()
  "Interleaving keeps every item exactly once and, for a balanced
split, no two neighbors ever share a topic."
  (dolist (seed '(1 5 17 99 20261123))
    (let* ((items (org-learn-study-test--interleave-pairs))
           (order (org-learn-study--interleave
                   items (lambda (item) (symbol-name (cdr item))) seed)))
      (should (= (length order) 8))
      (should (equal (sort (mapcar #'car order) #'<)
                     '(1 2 3 4 5 6 7 8)))
      (cl-loop for (a b) on order
               do (should-not (eq (cdr a) (cdr b)))))))

(ert-deftest org-learn-study-feynman-files-child-note ()
  "The Feynman note lands as a dated child heading, body indented."
  (cl-letf (((symbol-function 'org-learn-study--read-multiline)
             (lambda (_prompt)
               "It is like a pipeline of tiny steps.")))
    (with-temp-buffer
      (insert "* TODO Item one\n  some body\n")
      (org-mode)
      (goto-char (point-min))
      (org-learn-feynman)
      (should (string-match-p "\\*\\* Feynman 20[0-9][0-9]-" (buffer-string)))
      (should (string-match-p "pipeline of tiny steps" (buffer-string))
              ))))

(ert-deftest org-learn-study-practice-loop ()
  "Practice stores the goal, clocks through the seams, and files the
reflection as a dated child note."
  (let* ((events nil)
        (org-learn-study-clock-function
         (lambda () (push 'clocked-in events)))
        (org-learn-study-clock-out-function
         (lambda () (push 'clocked-out events))))
    (with-temp-buffer
      (insert "* TODO Skill\n")
      (org-mode)
      (goto-char (point-min))
      (org-learn-practice "land the flip")
      (should (equal (org-entry-get (point) "LEARN_GOAL") "land the flip"))
      (should (memq 'clocked-in events))
      (org-learn-practice-end "flips landed 3 of 5")
      (should (memq 'clocked-out events))
      (should (string-match-p "Goal: land the flip" (buffer-string)))
      (should (string-match-p "Reflection: flips landed 3 of 5"
                              (buffer-string))))))

(ert-deftest org-learn-study-practice-rejects-empty-goal ()
  "Deliberate practice without a goal is refused."
  (with-temp-buffer
    (insert "* TODO Skill\n")
    (org-mode)
    (goto-char (point-min))
    (should-error (org-learn-practice "  ") :type 'user-error)))

(ert-deftest org-learn-study-pomodoro-machine ()
  "Two cycles: focus -> break -> focus -> complete; every phase
change notifies and the timer seam gets the right minutes."
  (let* ((notes nil)
        (timers nil)
        (org-learn-study-notify-function
         (lambda (_title body) (push body notes)))
        (org-learn-study-timer-function
         (lambda (minutes) (push minutes timers)))
        (org-learn-study-clock-function (lambda () nil))
        (org-learn-study-clock-out-function (lambda () nil))
        (org-learn-pomodoro--state nil))
    (setq org-learn-pomodoro--state
          (list :phase 'focus :done 0 :cycles 2 :focus 25 :break 5))
    (org-learn-pomodoro--tick)
    (should (eq (plist-get org-learn-pomodoro--state :phase) 'break))
    (should (equal (car timers) 5))
    (org-learn-pomodoro--tick)
    (should (eq (plist-get org-learn-pomodoro--state :phase) 'focus))
    (should (equal (car timers) 25))
    (org-learn-pomodoro--tick)
    (should (null org-learn-pomodoro--state))
    (should (string-match-p "complete" (car notes)))
    (should (= (length timers) 2))))

(provide 'test-org-learn-study)
;;; test-org-learn-study.el ends here
