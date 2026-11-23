;;; tests/test-org-learn-notify.el --- ERT suite for org-learn-notify -*- lexical-binding: t; -*-

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

;; Spec 018 acceptance: the notification entry point is Android-first
;; with seams for both targets, the timer check announces due counts
;; and nudges once per day, and the widget tree and events are pure
;; over stubbed data.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-learn)
(require 'org-learn-notify)

(ert-deftest org-learn-notify-android-first ()
  "When the Android function is defined it wins, with the manual's
stable :urgency and :group pair."
  (let ((sent nil))
    (cl-letf (((symbol-function org-learn-notify-android-function)
               (lambda (&rest args) (push args sent))))
      (org-learn-notify "Title" "Body")
      (should (equal (plist-get (car sent) :title) "Title"))
      (should (equal (plist-get (car sent) :body) "Body"))
      (should (equal (plist-get (car sent) :group) "org-learn"))
      (should (eq (plist-get (car sent) :urgency) 'normal)))))

(ert-deftest org-learn-notify-desktop-fallback ()
  "Without the Android function the D-Bus desktop seam is used."
  (let ((sent nil))
    (cl-letf (((symbol-function org-learn-notify-desktop-function)
               (lambda (&rest args) (push args sent))))
      (org-learn-notify "Title" "Body")
      (should (equal (plist-get (car sent) :title) "Title")))))

(ert-deftest org-learn-notify-check-announces-due ()
  "A due count at or above the threshold announces once per count."
  (let ((notes nil))
    (cl-letf* (((symbol-function org-learn-notify-desktop-function)
                (lambda (&rest args)
                  (push (plist-get args :body) notes)))
               ((symbol-function 'org-learn-notify--due-count)
                (lambda () 5))
               ((symbol-function 'org-learn-discipline--data)
                (lambda (_files)
                  (list :done-today 1 :goal-reviews 10)))
               (org-learn-notify-evening-hour 24)
               (org-learn-notify--last-due-count nil)
               (org-learn-notify--nudge-date nil)
               (org-learn-notify-hours '(0 . 23)))
      (org-learn-notify--check)
      (should (string-match-p "5 reviews due" (car notes)))
      (org-learn-notify--check)
      (should (= (length notes) 1)))))

(ert-deftest org-learn-notify-check-nudge-once-per-day ()
  "The evening nudge fires once while the goal is unmet, then goes
quiet for the rest of the day."
  (let ((notes nil))
    (cl-letf* (((symbol-function org-learn-notify-desktop-function)
                (lambda (&rest args)
                  (push (plist-get args :body) notes)))
               ((symbol-function 'org-learn-notify--due-count)
                (lambda () 0))
               ((symbol-function 'org-learn-discipline--data)
                (lambda (_files)
                  (list :done-today 2 :goal-reviews 10)))
               (org-learn-notify-evening-hour 0)
               (org-learn-notify-due-threshold 3)
               (org-learn-notify--last-due-count nil)
               (org-learn-notify--nudge-date nil)
               (org-learn-notify-hours '(0 . 23)))
      (org-learn-notify--check)
      (should (string-match-p "Goal unmet" (car notes)))
      (org-learn-notify--check)
      (should (= (length notes) 1)))))

(ert-deftest org-learn-notify-tree-shape ()
  "The widget tree carries due, the goal bar, the streak, buttons."
  (cl-letf* (((symbol-function 'org-learn-discipline--data)
              (lambda (_files)
                (list :done-today 4 :goal-reviews 10 :streak 3)))
             ((symbol-function 'org-learn-dashboard--data)
              (lambda (_files) (list :due 2 :new 1)))
             (org-learn-review-scope 'file)
             (org-learn-file "/nonexistent/learn.org"))
    (let* ((tree (org-learn-notify--tree))
           (flat (format "%S" tree)))
      (should (eq (car (car tree)) :row))
      (should (string-match-p "2 due" flat))
      (should (string-match-p "4/10" flat))
      (should (string-match-p "3-day streak" flat))
      (should (string-match-p ":action \"review\"" flat))
      (should (string-match-p ":action \"drill\"" flat))
      (should (string-match-p ":action \"report\"" flat)))))

(ert-deftest org-learn-notify-on-event-dispatches ()
  "Widget taps run the matching org-learn commands."
  (let ((ran nil))
    (cl-letf (((symbol-function 'org-learn-review-interleaved)
               (lambda () (push 'review ran)))
              ((symbol-function 'org-learn-drill)
               (lambda () (push 'drill ran)))
              ((symbol-function 'org-learn-discipline-report)
               (lambda () (push 'report ran))))
      (should (org-learn-notify--on-event "review" nil))
      (should (org-learn-notify--on-event "drill" nil))
      (should (org-learn-notify--on-event "report" nil))
      (should (null (org-learn-notify--on-event "nope" nil)))
      (should (equal '(report drill review) ran)))))

(provide 'test-org-learn-notify)
;;; test-org-learn-notify.el ends here
