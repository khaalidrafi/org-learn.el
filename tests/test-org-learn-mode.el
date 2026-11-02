;;; tests/test-org-learn-mode.el --- ERT suite for org-learn-mode.el -*- lexical-binding: t; -*-

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

;; Spec 008 (specs/008-learn-mode.org) acceptance: org-learn-mode is a
;; buffer-local minor mode and global-org-learn-mode-everywhere enables
;; it only in Org buffers; the prefix keymap binds every key of the
;; loop to a commandp symbol (lazy, autoload-style binds); the help
;; popup builds without error; the easy-menu "Learn" entry exists.
;; Loaded additively from tests/test-org-learn.el (one CI entry).

;;; Code:

(require 'ert)
(require 'org-learn)
(require 'org-learn-review)
(require 'org-learn-ai)
(require 'org-learn-drill)
(require 'org-learn-dashboard)
(require 'org-learn-roadmap)
(require 'org-learn-mode)

(defvar org-learn-mode-test-bindings
  '(("c" . org-learn-capture)
    ("o" . org-learn-open)
    ("r" . org-learn-review)
    ("d" . org-learn-drill)
    ("a" . org-learn-ai-ask)
    ("f" . org-learn-ai-generate-flashcards)
    ("Q" . org-learn-ai-quiz)
    ("D" . org-learn-dashboard)
    ("p" . org-learn-plan-from-roadmap)
    ("P" . org-learn-progress-report)
    ("s" . org-learn-study)
    ("T" . org-learn-ai-trace)
    ("F" . org-learn-ai-focus)
    ("C" . org-learn-ai-clear)
    ("?" . org-learn-mode-help))
  "Spec 008 keymap table: key string -> command symbol.")

(ert-deftest org-learn-mode-predicates-exist-and-are-commands ()
  "Spec 008: both mode toggles and the help command are interactive."
  (should (commandp 'org-learn-mode))
  (should (commandp 'global-org-learn-mode-everywhere))
  (should (commandp 'org-learn-mode-help))
  (should (boundp 'org-learn-mode-map)))

(ert-deftest org-learn-mode-keymap-resolves-every-key-to-a-command ()
  "Spec 008: every key above resolves to a commandp symbol in the map.
The binds are lazy (autoload-style): the key exists whether or not
the owning module is loaded."
  (dolist (binding org-learn-mode-test-bindings)
    (should (eq (lookup-key org-learn-mode-keymap-prefix
                            (kbd (car binding)))
                (cdr binding)))
    (should (commandp (cdr binding)))))

(ert-deftest org-learn-mode-prefix-keymap-is-bound-under-C-c-l ()
  "Spec 008: the prefix keymap is reachable under the C-c l prefix."
  (should (eq (lookup-key org-learn-mode-map (kbd "C-c l"))
              org-learn-mode-keymap-prefix)))

(ert-deftest org-learn-mode-help-popup-lists-every-key-and-command ()
  "Spec 008: the help popup builds without error and lists the map.
Each command of the loop is shown with its key, so it is always
clear which button does what."
  (org-learn-mode-help)
  (let ((text (with-current-buffer org-learn-mode-help-buffer
                (buffer-string))))
    (should (string-match-p "whole learning loop" text))
    (dolist (binding org-learn-mode-test-bindings)
      (should (string-match-p
               (regexp-quote (symbol-name (cdr binding)))
               text)))))

(ert-deftest org-learn-mode-ships-a-learn-menu ()
  "Spec 008: the easy-menu \"Learn\" entry exists for discoverability."
  (let ((menu (lookup-key org-learn-mode-map [menu-bar learn])))
    (should (keymapp menu))
    (should (equal (nth 1 menu) "Learn"))))

(ert-deftest org-learn-global-mode-enables-only-in-org-buffers ()
  "Spec 008 behavior law: the global mode enables Org-Learn in Org only.
Buffers switched to Org after the toggle are picked up too."
  (let ((org-buf (get-buffer-create " *org-learn-test-org*"))
        (plain-buf (get-buffer-create " *org-learn-test-plain*"))
        late-buf)
    (unwind-protect
        (progn
          (with-current-buffer org-buf (org-mode))
          (global-org-learn-mode-everywhere 1)
          (should (buffer-local-value 'org-learn-mode org-buf))
          (should-not (buffer-local-value 'org-learn-mode plain-buf))
          (setq late-buf (get-buffer-create " *org-learn-test-late*"))
          (with-current-buffer late-buf (org-mode))
          (should (buffer-local-value 'org-learn-mode late-buf)))
      (global-org-learn-mode-everywhere -1)
      (dolist (buf (list org-buf plain-buf late-buf))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(provide 'test-org-learn-mode)
;;; test-org-learn-mode.el ends here
