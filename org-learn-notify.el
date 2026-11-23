;;; org-learn-notify.el --- Notifications and widgets for org-learn -*- lexical-binding: t; -*-

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

;; Spec 018 (specs/018-notify-widgets.org): the phone surface --
;; Android-first notifications and home-screen widgets.
;;
;; - `org-learn-notify' is the one notification entry point, written
;;   against the GNU Emacs Lisp Reference Manual, \"Desktop
;;   Notifications\": an Emacs built as an Android application
;;   displays notifications through `android-notifications-notify'
;;   (no callbacks, at most three actions, a stable :urgency and
;;   :group pair per kind, silently disregarded without the Android
;;   13+ notification permission); desktops with D-Bus use
;;   `notifications-notify'; anything else falls back to `message'.
;;   A notification never raises.
;; - `org-learn-notify-mode' is a global timer mode: while active it
;;   checks the due queue every `org-learn-notify-interval' minutes
;;   inside `org-learn-notify-hours', announces when reviews come
;;   due, and fires one evening nudge per day while the review goal
;;   is unmet.
;; - `org-learn-register-widgets' defines the elwidget home-screen
;;   widgets (soft dependency, the loophabits pattern): \"org-learn\"
;;   4x2 -- due and new counts, the goal progress bar, the streak,
;;   and Review / Drill / Report buttons whose taps run the real
;;   commands in Emacs.  `M-x elwidget-refresh' picks them up on the
;;   Android side.

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)
(require 'org-learn)
(require 'org-learn-review)
(require 'org-learn-dashboard)
(require 'org-learn-discipline)

;; Soft dependency: elwidget ships in emacshell and installs
;; anywhere else as an ordinary package; nothing here loads it.
(declare-function elwidget-define "elwidget" (name &rest props))
;; org-learn-drill lives in its own module; the tap handler calls
;; it lazily, so the declaration keeps the compiler quiet.
(declare-function org-learn-drill "org-learn-drill" (&optional scope))
;; Same for the interleaved review (org-learn-study.el).
(declare-function org-learn-review-interleaved "org-learn-study" ())

(defgroup org-learn-notify nil
  "Notifications and widgets for org-learn."
  :group 'org-learn
  :prefix "org-learn-notify-")

(defcustom org-learn-notify-hours '(8 . 21)
  "Active notification hours: (FIRST . LAST), 0-23 each.
Outside this window the due check stays silent."
  :type '(cons (natnum :tag "First hour")
               (natnum :tag "Last hour"))
  :group 'org-learn-notify)

(defcustom org-learn-notify-interval 60
  "Minutes between due checks in `org-learn-notify-mode'."
  :type 'natnum
  :group 'org-learn-notify)

(defcustom org-learn-notify-due-threshold 3
  "Announce when at least this many items are due."
  :type 'natnum
  :group 'org-learn-notify)

(defcustom org-learn-notify-evening-hour 20
  "Hour of the one-per-day goal nudge, 0-23."
  :type 'natnum
  :group 'org-learn-notify)

(defcustom org-learn-notify-android-function
  'android-notifications-notify
  "Android notify seam: used when that function is defined.
Tests bind a recording stub."
  :type 'function
  :group 'org-learn-notify)

(defcustom org-learn-notify-desktop-function
  'notifications-notify
  "Desktop notify seam: used when Android's is absent.
Tests bind a recording stub."
  :type 'function
  :group 'org-learn-notify)

;;; The one entry point (Elisp manual, Desktop Notifications).

(defun org-learn-notify (title body &rest _args)
  "Notify TITLE and BODY, Android-first; never raises.
On the Emacs Android port this calls `android-notifications-notify'
with a stable :urgency and :group pair (the manual's advice: the
system may disregard urgency that does not match the group's
history) and the standard alert icon; on D-Bus desktops it calls
`notifications-notify'; otherwise it degrades to `message'.  ARGS
is accepted and ignored so callers can pass either signature's
extras without branching."
  (cond
   ((fboundp org-learn-notify-android-function)
    (funcall org-learn-notify-android-function
             :title title :body body
             :urgency 'normal :group "org-learn"
             :icon "ic_dialog_info"))
   ((fboundp org-learn-notify-desktop-function)
    (funcall org-learn-notify-desktop-function
             :title title :body body))
   (t (message "org-learn: %s -- %s" title body))))

;;; The timer mode (due announcements + one evening nudge per day).

(defvar org-learn-notify--timer nil
  "The running due-check timer, or nil.")

(defvar org-learn-notify--last-due-count nil
  "The due count last announced, to avoid repeats.")

(defvar org-learn-notify--nudge-date nil
  "The date the evening nudge already fired, or nil.")

(defun org-learn-notify--in-hours-p ()
  "Return non-nil inside `org-learn-notify-hours'."
  (let* ((hour (nth 2 (decode-time)))
         (first (car org-learn-notify-hours))
         (last (cdr org-learn-notify-hours)))
    (and (>= hour first) (<= hour last))))

(defun org-learn-notify--check ()
  "One due/goal check; silent outside the hours.
Announces a due queue at or above `org-learn-notify-due-threshold'
when the count changed, and fires the evening nudge once per day
while the review goal is unmet."
  (when (org-learn-notify--in-hours-p)
    (let* ((files (seq-filter #'file-exists-p
                              (org-learn-review--scoped-files)))
           (data (org-learn-discipline--data files))
           (due (org-learn-notify--due-count))
           (done (plist-get data :done-today))
           (goal (plist-get data :goal-reviews))
           (today (format-time-string "%Y-%m-%d"))
           (hour (nth 2 (decode-time))))
      (when (and (>= due org-learn-notify-due-threshold)
                 (not (equal due org-learn-notify--last-due-count)))
        (org-learn-notify
         "org-learn"
         (format "%d review%s due -- a session is waiting"
                 due (if (= due 1) "" "s"))))
      (setq org-learn-notify--last-due-count due)
      (when (and (>= hour org-learn-notify-evening-hour)
                 (< done goal)
                 (not (equal org-learn-notify--nudge-date today)))
        (setq org-learn-notify--nudge-date today)
        (org-learn-notify
         "org-learn"
         (format "Goal unmet: %d/%d reviews -- %d left to save the streak"
                 done goal (- goal done)))))))

(defun org-learn-notify--due-count ()
  "Return the due item count over the scoped files."
  (let* ((files (seq-filter #'file-exists-p
                            (org-learn-review--scoped-files)))
         (count 0))
    (dolist (file files count)
      (let ((buffer (find-file-noselect file)))
        (with-current-buffer buffer
          (setq count (+ count
                         (length (org-learn-review--collect-items
                                  buffer)))))
        (kill-buffer buffer)))))

;;;###autoload
(define-minor-mode org-learn-notify-mode
  "Timer mode: announce due reviews and the unmet goal.
Checks every `org-learn-notify-interval' minutes inside
`org-learn-notify-hours'; the evening nudge fires once per day at
or after `org-learn-notify-evening-hour' while the review goal is
unmet.  Notifications go through `org-learn-notify'."
  :init-value nil
  :lighter " Learn!"
  :global t
  :group 'org-learn-notify
  (when org-learn-notify--timer
    (cancel-timer org-learn-notify--timer)
    (setq org-learn-notify--timer nil))
  (when org-learn-notify-mode
    (org-learn-notify--check)
    (setq org-learn-notify--timer
          (run-at-time t (* org-learn-notify-interval 60)
                       #'org-learn-notify--check))))

;;; elwidget home-screen widgets (the loophabits pattern).

(defun org-learn-notify--on-event (action _arg)
  "Handle a widget tap: ACTION runs the matching org-learn command."
  (cond
   ((equal action "review")
    (org-learn-review-interleaved)
    t)
   ((equal action "drill")
    (org-learn-drill)
    t)
   ((equal action "dashboard")
    (org-learn-dashboard)
    t)
   ((equal action "report")
    (org-learn-discipline-report)
    t)))

(defun org-learn-notify--tree ()
  "The org-learn widget tree: due, goal bar, streak, buttons."
  (let* ((files (seq-filter #'file-exists-p
                            (org-learn-review--scoped-files)))
         (data (org-learn-discipline--data files))
         (dash (org-learn-dashboard--data files))
         (due (plist-get dash :due))
         (new (plist-get dash :new))
         (done (plist-get data :done-today))
         (goal (plist-get data :goal-reviews))
         (streak (plist-get data :streak)))
    (append
     (list
      `(:row (:text "Learn" :size 16 :bold t :color "#89b4fa")
             (:text ,(format "%d due" due) :size 13 :align right
                    :color ,(if (zerop due) "#a6e3a1" "#f38ba8")))
      `(:progress :value ,(min done goal) :max ,(max 1 goal)
                  :label ,(format "%d/%d" done goal)
                  :color "#a6e3a1")
      `(:text ,(format "%d new, %d-day streak" new streak)
              :size 12 :color "#a6adc8")
      `(:row (:button "Review" :action "review" :color "#89b4fa")
             (:button "Drill" :action "drill" :color "#cba6f7")
             (:button "Report" :action "report" :color "#94e2d5"))))))

;;;###autoload
(defun org-learn-register-widgets ()
  "Define the org-learn home-screen widget (requires elwidget).

One spec, 4x2: the due and new counts, the daily goal progress
bar, the streak, and Review / Drill / Report buttons whose taps
run the real commands in Emacs.  Call it once from your init
(emacshell: after loading org-learn), then `M-x elwidget-refresh'
and pick the spec in the Android widget picker."
  (interactive)
  (unless (require 'elwidget nil t)
    (user-error "The elwidget package is required for the widgets"))
  (elwidget-define "org-learn"
    :title "org-learn (tap to review)"
    :size [4 2]
    :render #'org-learn-notify--tree
    :on-event #'org-learn-notify--on-event))

(provide 'org-learn-notify)
;;; org-learn-notify.el ends here
