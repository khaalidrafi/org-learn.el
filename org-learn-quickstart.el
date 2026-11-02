;;; org-learn-quickstart.el --- Zero-to-loop quickstart wizard -*- lexical-binding: t; -*-

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

;; Spec 010 (specs/010-quickstart.org): `org-learn-quickstart' walks a
;; brand-new user from zero to the full learning loop in one command:
;; learning file, first capture, first review, keymap tour.  Every
;; step announces itself in the echo area and is skippable with
;; `y-or-n-p'; a "no" skips the step without aborting the wizard.
;;
;; The wizard never destroys data: the create path refuses to touch an
;; existing file, so a learning file can never be clobbered and the
;; template can never be merged into existing notes.  The keymap tour
;; renders the live `C-c l' prefix map at runtime, never a hardcoded
;; key list.  org-roadmap is never required here; roadmap commands are
;; README next-steps only.

;;; Code:

;; The tour renders the live keymap of spec 008; the keymap module is
;; loaded lazily inside the tour, never eagerly.
(defvar org-learn-mode-keymap-prefix)

;; Sibling commands offered by the wizard; org-learn is always loaded
;; by the time this module runs (its autoload points here).
(declare-function org-learn-capture "org-learn" (&optional title))
(declare-function org-learn-review "org-learn-review")

(defgroup org-learn-quickstart nil
  "Quickstart wizard for org-learn."
  :group 'org-learn
  :prefix "org-learn-quickstart-")

(defcustom org-learn-quickstart-file
  (expand-file-name "learn.org" user-emacs-directory)
  "Learning file the quickstart wizard sets up and opens.
The wizard's own default; a user's `org-learn-file' override is
not forced."
  :type 'file
  :group 'org-learn-quickstart)

(defconst org-learn-quickstart--template
  "#+TITLE: My Learning
#+FILETAGS: :learning:

* TODO An example topic                                   :drill:
  :PROPERTIES:
  :LEARN_EASE: 2.5
  :LEARN_INTERVAL: 1
  :LEARN_REPS: 0
  :LEARN_LAPSES: 0
  :END:
  Replace this topic with something you want to learn today.
  Capture more topics with M-x org-learn-capture (C-c l c), then
  review what is due with M-x org-learn-review (C-c l r).
"
  "Template written when the quickstart creates a fresh learning file.")

(defun org-learn-quickstart--announce (step)
  "Announce quickstart STEP in the echo area."
  (message "org-learn quickstart -- step %s" step))

(defun org-learn-quickstart--create-file (file)
  "Create the learning FILE from the quickstart template.
The create path refuses to touch an existing FILE: it writes
nothing and leaves every byte alone, so it never clobbers user
data and running it twice cannot duplicate template content.
Return non-nil when the template was written."
  (if (file-exists-p file)
      (progn
        (message "org-learn quickstart -- refusing to touch existing %s"
                 file)
        nil)
    (let ((dir (file-name-directory file)))
      (when dir (make-directory dir t)))
    (with-temp-buffer
      (insert org-learn-quickstart--template)
      (write-region (point-min) (point-max) file nil 'quiet))
    t))

(defun org-learn-quickstart--step-file ()
  "Quickstart step 1: set up the learning file and open it.
A missing `org-learn-quickstart-file' is offered creation from the
template; an existing one is offered to be opened.  Either way the
create path refuses to touch an existing file."
  (let ((file org-learn-quickstart-file))
    (if (file-exists-p file)
        (when (y-or-n-p (format "Open your learning file (%s)? " file))
          (find-file file))
      (when (y-or-n-p (format "Create your learning file (%s)? " file))
        (org-learn-quickstart--create-file file)
        (find-file file)))))

(defun org-learn-quickstart--tour-line (key command)
  "Return one tour line for KEY bound to COMMAND."
  (format "%-3s %-34s %s"
          (single-key-description key)
          command
          (or (ignore-errors
                (car (split-string (documentation command t) "\n" t)))
              "")))

(defun org-learn-quickstart--tour-lines ()
  "Return the tour lines rendered from the live prefix keymap.
This is `org-learn-mode-keymap-prefix', read at runtime."
  (let (lines)
    (map-keymap
     (lambda (key command)
       (when (symbolp command)
         (push (org-learn-quickstart--tour-line key command) lines)))
     org-learn-mode-keymap-prefix)
    (sort lines #'string<)))

(defun org-learn-quickstart--keymap-tour ()
  "Pop up a help window touring the live learning-loop prefix map.
Every line comes from `org-learn-mode-keymap-prefix' at runtime,
so the tour always shows what the keymap really binds."
  (unless (boundp 'org-learn-mode-keymap-prefix)
    (require 'org-learn-mode nil t))
  (with-help-window "*org-learn quickstart tour*"
    (princ "The whole learning loop lives under the C-c l prefix.
Every key below is read from your live keymap right now:\n\n")
    (if (boundp 'org-learn-mode-keymap-prefix)
        (dolist (line (org-learn-quickstart--tour-lines))
          (princ line)
          (princ "\n"))
      (princ "org-learn-mode is not loaded; try M-x org-learn-mode-help.\n"))
    (princ "\nPress C-c l ? to see this list any time.\n")))

;;;###autoload
(defun org-learn-quickstart ()
  "Walk a new user from zero to the full org-learn learning loop.
The four steps -- learning file, first capture, first review,
keymap tour -- each announce themselves in the echo area and are
offered with `y-or-n-p'; answering \"no\" skips a step without
aborting the wizard.  The learning file is created from a small
template when missing and never clobbered when present.  The
wizard finishes by pointing at \\<org-learn-mode-map>\\[org-learn-mode-help]."
  (interactive)
  (org-learn-quickstart--announce "1/4: learning file")
  (org-learn-quickstart--step-file)
  (org-learn-quickstart--announce "2/4: first capture")
  (when (y-or-n-p "Capture your first learning note now? ")
    (call-interactively #'org-learn-capture))
  (org-learn-quickstart--announce "3/4: first review")
  (when (y-or-n-p "Review what is due now? ")
    (org-learn-review))
  (org-learn-quickstart--announce "4/4: keymap tour")
  (when (y-or-n-p "See the tour of the C-c l keymap now? ")
    (org-learn-quickstart--keymap-tour))
  (message "org-learn quickstart done -- C-c l ? lists every key, any time."))

(provide 'org-learn-quickstart)
;;; org-learn-quickstart.el ends here
