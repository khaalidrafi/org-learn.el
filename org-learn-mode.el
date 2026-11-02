;;; org-learn-mode.el --- One keymap for the whole learning loop -*- lexical-binding: t; -*-

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

;; Spec 008 (specs/008-learn-mode.org): one entry point for the whole
;; learning loop -- "I need to get started learning FAST" (Owner order
;; 2026-11-01 18:28Z #hq).
;;
;; `org-learn-mode' is a buffer-local minor mode and
;; `global-org-learn-mode-everywhere' turns it on in every Org buffer
;; (it is meaningless elsewhere).  Every org-learn command lives one
;; keystroke under `org-learn-mode-keymap-prefix', and
;; \\[org-learn-mode-help] pops up a temporary window listing every
;; key with its command, so it is always clear which button does what.
;;
;; The binds are lazy (autoload-style): a key stays bound whether or
;; not its optional module is loaded, and invoking it loads the module
;; or surfaces the module's own friendly `user-error'.  No sibling
;; module is required here, so this file loads standalone.
;;
;; An easy-menu \"Learn\" entry keeps the loop discoverable from the
;; menu bar.

;;; Code:

(require 'org)

(defgroup org-learn-mode nil
  "One keymap for the whole org-learn learning loop."
  :group 'org
  :prefix "org-learn-mode-")

(defun org-learn-mode--set-keymap-prefix (symbol value)
  "Set SYMBOL to VALUE and rebind it in `org-learn-mode-map'."
  (set-default symbol value)
  (when (boundp 'org-learn-mode-map)
    (define-key org-learn-mode-map (kbd "C-c l") value)))

(defcustom org-learn-mode-keymap-prefix
  (let ((map (make-sparse-keymap)))
    ;; Every bind is lazy (autoload-style): a missing optional module
    ;; keeps its key, and its command reports the problem itself.
    (define-key map (kbd "c") 'org-learn-capture)
    (define-key map (kbd "o") 'org-learn-open)
    (define-key map (kbd "r") 'org-learn-review)
    (define-key map (kbd "d") 'org-learn-drill)
    (define-key map (kbd "a") 'org-learn-ai-ask)
    (define-key map (kbd "f") 'org-learn-ai-generate-flashcards)
    (define-key map (kbd "Q") 'org-learn-ai-quiz)
    (define-key map (kbd "D") 'org-learn-dashboard)
    (define-key map (kbd "p") 'org-learn-plan-from-roadmap)
    (define-key map (kbd "P") 'org-learn-progress-report)
    (define-key map (kbd "s") 'org-learn-study)
    (define-key map (kbd "T") 'org-learn-ai-trace)
    (define-key map (kbd "F") 'org-learn-ai-focus)
    (define-key map (kbd "C") 'org-learn-ai-clear)
    (define-key map (kbd "?") 'org-learn-mode-help)
    map)
  "Prefix keymap holding the whole org-learn learning loop.
Bound into `org-learn-mode-map' under the prefix key of
`org-learn-mode'; \\[org-learn-mode-help] lists every key with its
command.  The binds are lazy: each command autoloads its module, so
an absent optional module still has its key and reports itself with
a friendly `user-error' when invoked."
  :type '(restricted-sexp :match-alternatives (keymapp))
  :set #'org-learn-mode--set-keymap-prefix
  :group 'org-learn-mode)

(defvar org-learn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l") org-learn-mode-keymap-prefix)
    map)
  "Keymap of `org-learn-mode': the whole learning loop, one prefix.
\\[org-learn-mode-help] lists every key with its command.")

(easy-menu-define org-learn-mode-menu org-learn-mode-map
  "Menu for `org-learn-mode': the learning loop in one place."
  '("Learn"
    ["Capture a learning note" org-learn-capture]
    ["Open the notes file" org-learn-open]
    "--"
    ["Review what is due" org-learn-review]
    ["Drill the cards" org-learn-drill]
    ["Start a study session" org-learn-study]
    "--"
    ["Show the dashboard" org-learn-dashboard]
    ["Plan review from the roadmap" org-learn-plan-from-roadmap]
    ["Show the progress report" org-learn-progress-report]
    "--"
    ["Ask the AI tutor" org-learn-ai-ask]
    ["Generate AI flashcards" org-learn-ai-generate-flashcards]
    ["Quiz me with the AI" org-learn-ai-quiz]
    ["Trace the concept with the AI" org-learn-ai-trace]
    ["Focus the session with the AI" org-learn-ai-focus]
    ["Clear the AI chat" org-learn-ai-clear]
    "--"
    ["Help: list every key" org-learn-mode-help]))

(defun org-learn-mode--turn-on ()
  "Enable `org-learn-mode' when the current buffer is an Org buffer.
Org-Learn mode is meaningless outside Org, so the global mode never
enables it elsewhere."
  (when (derived-mode-p 'org-mode)
    (org-learn-mode 1)))

;;;###autoload
(define-minor-mode org-learn-mode
  "One keymap for the whole org-learn learning loop, in Org buffers.
Every command lives under `org-learn-mode-keymap-prefix';
\\[org-learn-mode-help] pops up a temporary window listing every key
with its command, so it is always clear which button does what."
  :lighter " Learn"
  :keymap org-learn-mode-map
  :group 'org-learn-mode)

;;;###autoload
(define-globalized-minor-mode global-org-learn-mode-everywhere
  org-learn-mode org-learn-mode--turn-on
  :group 'org-learn-mode)

;;; Help popup (spec 008: "make it clear which button does what").

(defconst org-learn-mode-help-buffer "*org-learn-mode help*"
  "Name of the buffer where `org-learn-mode-help' lists the keys.")

(defun org-learn-mode--help-line (key command)
  "Return one help line for KEY bound to COMMAND."
  (format "%-3s %-32s %s"
          (single-key-description key)
          command
          (or (ignore-errors
                (car (split-string (documentation command t) "\n" t)))
              "")))

(defun org-learn-mode--help-text ()
  "Return the key/command listing shown by `org-learn-mode-help'."
  (let ((lines nil))
    (map-keymap
     (lambda (key command)
       (when (symbolp command)
         (push (org-learn-mode--help-line key command) lines)))
     org-learn-mode-keymap-prefix)
    (concat "The whole learning loop, one prefix.  Every key:\n\n"
            (mapconcat #'identity (sort lines #'string<) "\n")
            "\n")))

;;;###autoload
(defun org-learn-mode-help ()
  "Pop up a temporary window listing every key of the prefix keymap.
Each line pairs a key with its command and the command's first
documentation line, so it is always clear which button does what."
  (interactive)
  (with-output-to-temp-buffer org-learn-mode-help-buffer
    (princ (org-learn-mode--help-text))))

(provide 'org-learn-mode)
;;; org-learn-mode.el ends here
