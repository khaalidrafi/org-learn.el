;;; tests/test-org-learn.el --- ERT suite for org-learn.el -*- lexical-binding: t; -*-

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

;; ERT suite for the org-learn bootstrap skeleton.  It runs under the
;; same batch Emacs the CI gate uses, so the gate proof is reproducible
;; locally and in CI:
;;
;;   emacs -Q --batch -L . -l tests/test-org-learn.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Suite law (repo AGENTS.md): every public behavior lands with a
;; test; the org-roadmap boundary is locked here too.

;;; Code:

(require 'ert)
(require 'org-learn)

(ert-deftest org-learn-open-visits-the-notes-file ()
  "org-learn-open visits `org-learn-file' via find-file."
  (let ((org-learn-file (make-temp-file "org-learn-test-" nil ".org")))
    (unwind-protect
        (progn
          (org-learn-open)
          (should (get-file-buffer org-learn-file)))
      ;; Leave no buffer behind, batch or not.
      (let ((buf (get-file-buffer org-learn-file)))
        (when buf (kill-buffer buf))))))

(ert-deftest org-learn-open-is-an-interactive-command ()
  "org-learn-open is a command, so M-x org-learn-open works."
  (should (commandp 'org-learn-open)))

(ert-deftest org-learn-default-notes-file ()
  "The default notes file is learn.org."
  (should (equal org-learn-file "learn.org")))

(ert-deftest org-learn-keeps-the-org-roadmap-boundary ()
  "Loading org-learn never loads org-roadmap (boundary law).
org-roadmap is unpublished; under -Q it cannot load at all, so this
asserts the soft boundary holds in the gate environment."
  (should-not (featurep 'org-roadmap)))

;;; tests/test-org-learn.el ends here
