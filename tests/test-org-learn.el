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
;; same batch Emacs (invoked with -Q) and the same
;; `ert-run-tests-batch-and-exit' entry point the CI gate uses, so the
;; gate proof is reproducible locally and in CI.
;; Suite law (repo AGENTS.md): every public behavior lands with a
;; test.  The org-roadmap boundary test that used to live here was
;; replaced by the spec 006 dependency law (Owner order 2026-11-01
;; 18:28Z: org-roadmap is a real dependency now); see
;; tests/test-org-learn-roadmap.el.  Spec 001 (capture) acceptance
;; criteria land here too -- tests first, the implementation lands
;; with them (local AGENTS.md).

;;; Code:

(require 'ert)
(require 'org-learn)

(defun org-learn-test-read-file (file)
  "Return FILE's contents as a string, without visiting it."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-learn-test-count-matches (regexp string)
  "Count non-overlapping REGEXP matches in STRING (line-anchored)."
  (let ((count 0))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq count (1+ count))))
    count))

(ert-deftest org-learn-open-visits-the-notes-file ()
  "`org-learn-open' visits `org-learn-file' via `find-file'."
  (let ((org-learn-file (make-temp-file "org-learn-test-" nil ".org")))
    (unwind-protect
        (progn
          (org-learn-open)
          (should (get-file-buffer org-learn-file)))
      ;; Leave no buffer behind, batch or not.
      (let ((buf (get-file-buffer org-learn-file)))
        (when buf (kill-buffer buf))))))

(ert-deftest org-learn-open-is-an-interactive-command ()
  "`org-learn-open' is a command, so it can be invoked by name."
  (should (commandp 'org-learn-open)))

(ert-deftest org-learn-default-notes-file ()
  "The default notes file is learn.org."
  (should (equal org-learn-file "learn.org")))

;;; Spec 001 (specs/001-capture.org) — org-learn-capture acceptance.

(ert-deftest org-learn-capture-creates-missing-file-with-header ()
  "Spec 001: capture into a missing file creates it — header + TODO.
The file gets the minimal Org header, one TODO heading and a CREATED
property drawer with an ISO-8601 timestamp."
  (let* ((dir (make-temp-file "org-learn-cap-" t))
         (file (expand-file-name "notes.org" dir))
         (org-learn-file file))
    (unwind-protect
        (progn
          (should-not (file-exists-p file))
          (org-learn-capture "Alpha note")
          (let ((content (org-learn-test-read-file file)))
            (should (string-match-p "^#\\+TITLE:" content))
            (should (string-match-p "^\\* TODO Alpha note" content))
            (should (string-match-p "^  :PROPERTIES:" content))
            (should (string-match-p
                     "^  :CREATED: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T"
                     content))
            (should (string-match-p "^  :END:" content))))
      (delete-directory dir t))))

(ert-deftest org-learn-capture-appends-and-never-duplicates-header ()
  "Spec 001: second capture appends; the header is written once."
  (let* ((file (make-temp-file "org-learn-cap2-" nil ".org"))
         (org-learn-file file))
    (unwind-protect
        (progn
          (org-learn-capture "First note")
          (org-learn-capture "Second note")
          (let ((content (org-learn-test-read-file file)))
            (should (= 1 (org-learn-test-count-matches
                          "^#\\+TITLE:" content)))
            (should (= 2 (org-learn-test-count-matches
                          "^\\* TODO " content)))
            ;; First note precedes Second note.  The any-char
            ;; alternative is the Emacs idiom for PCRE's [\s\S]:
            ;; inside a character alternative \s introduces a syntax
            ;; class, so [\s\S] would match only "S" and backslash.
            (should (string-match-p
                     "\\* TODO First note\\(?:.\\|\n\\)*\\* TODO Second note"
                     content))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest org-learn-capture-from-region-embeds-region-text ()
  "Spec 001: region capture embeds the region text as the body.
The region text lands under the TODO heading, indented as the
heading body."
  (let* ((file (make-temp-file "org-learn-cap3-" nil ".org"))
         (org-learn-file file))
    (unwind-protect
        (with-temp-buffer
          (insert "first region line\nsecond region line")
          (org-learn-capture-from-region (point-min) (point-max)
                                         "Region note")
          (let ((content (org-learn-test-read-file file)))
            (should (string-match-p "^\\* TODO Region note" content))
            (should (string-match-p "^  first region line$" content))
            (should (string-match-p "^  second region line$" content))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest org-learn-capture-honors-the-org-learn-file-custom ()
  "Spec 001: the org-learn-file custom decides where notes land."
  (let* ((file (make-temp-file "org-learn-cap4-" nil ".org"))
         (org-learn-file file))
    (unwind-protect
        (progn
          (org-learn-capture "Custom target")
          (should (file-exists-p file))
          (should (string-match-p "\\* TODO Custom target"
                                  (org-learn-test-read-file file))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest org-learn-capture-commands-are-interactive-commands ()
  "Spec 001: both capture entry points are commands, runnable by name."
  (should (commandp 'org-learn-capture))
  (should (commandp 'org-learn-capture-from-region)))

(ert-deftest org-learn-capture-leaves-no-orphan-buffer ()
  "Spec 001 behavior law: the notes file is never visited."
  (let* ((file (make-temp-file "org-learn-cap6-" nil ".org"))
         (org-learn-file file))
    (unwind-protect
        (progn
          (org-learn-capture "No orphan")
          (should-not (get-file-buffer file)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest org-learn-capture-default-title-region-point-truncate ()
  "Spec 001: default title = region, else point; always truncated.
Region text wins when the region is active, the symbol at point is
the fallback, and long defaults truncate to `org-learn--title-max'
with an ellipsis."
  ;; Region active wins over the symbol at point.
  (with-temp-buffer
    (insert "alpha beta gamma")
    (goto-char 7)                    ; on "beta"
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (should (equal (org-learn--default-title) "alpha beta gamma")))
  ;; No region: symbol at point.
  (with-temp-buffer
    (insert "alpha beta gamma")
    (goto-char 7)
    (should (equal (org-learn--default-title) "beta")))
  ;; Truncation: 100 chars -> exactly the max, ellipsis-terminated.
  (should (= (length (org-learn--truncate-title (make-string 100 ?x)))
             org-learn--title-max))
  (should (string-suffix-p "..."
                           (org-learn--truncate-title (make-string 100 ?x)))))

;;; Review scheduler tests (spec 002) -- loaded here so the single CI
;;; entry (one -l of this file) runs the whole suite.

(load (expand-file-name "tests/test-org-learn-review.el" default-directory))

;;; AI tutor tests (spec 004) -- same single-entry law.

(load (expand-file-name "tests/test-org-learn-ai.el" default-directory))

;;; Drill session tests (spec 003) -- same single-entry law.

(load (expand-file-name "tests/test-org-learn-drill.el" default-directory))

;;; Dashboard tests (spec 005) -- same single-entry law.

(load (expand-file-name "tests/test-org-learn-dashboard.el" default-directory))

;;; Roadmap bridge tests (spec 006) -- same single-entry law.  The
;;; org-roadmap stub lives on the `load-path' only inside that suite;
;;; its dependency-law test replaces the deleted
;;; org-learn-keeps-the-org-roadmap-boundary test above (Owner order
;;; 2026-11-01 18:28Z).

(load (expand-file-name "tests/test-org-learn-roadmap.el" default-directory))

(provide 'test-org-learn)
;;; test-org-learn.el ends here
