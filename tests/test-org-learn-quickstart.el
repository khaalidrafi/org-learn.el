;;; tests/test-org-learn-quickstart.el --- ERT suite for quickstart -*- lexical-binding: t; -*-

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

;; Spec 010 (specs/010-quickstart.org) acceptance: the quickstart
;; wizard creates the learning file from the template and visits it,
;; never clobbers an existing file, skips cleanly when the capture and
;; review offers are declined (their counters stay 0), binds "q" under
;; the C-c l prefix, and its create path is idempotent.  Every prompt
;; is answered through `cl-letf' on `y-or-n-p' and every write lands
;; in a temp directory, so the suite never prompts and never writes
;; outside temp dirs.  Loaded additively from tests/test-org-learn.el
;; (one CI entry).

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org-learn)
(require 'org-learn-mode)
(require 'org-learn-quickstart nil t)

(defun org-learn-quickstart-test--read-file (file)
  "Return FILE's contents as a string, without visiting it."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-learn-quickstart-test--count-matches (regexp string)
  "Count non-overlapping REGEXP matches in STRING (line-anchored)."
  (let ((count 0))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq count (1+ count))))
    count))

(defun org-learn-quickstart-test--answer-queue (answers)
  "Return a `y-or-n-p' stand-in answering from ANSWERS, then nil."
  (lambda (&rest _)
    (prog1 (car answers)
      (setq answers (cdr answers)))))

(ert-deftest org-learn-quickstart-creates-file-from-template ()
  "Spec 010: the wizard creates the learning file from the template.
Step 1 answers yes (create), the three offers after step 1 answer
no; the file then exists with the template markers and its buffer
is visited."
  (let* ((dir (make-temp-file "org-learn-qs-create-" t))
         (file (expand-file-name "learn.org" dir))
         (org-learn-quickstart-file file))
    (unwind-protect
        (progn
          (should-not (file-exists-p file))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (org-learn-quickstart-test--answer-queue
                      (list t nil nil nil))))
            (org-learn-quickstart))
          (should (file-exists-p file))
          (let ((content (org-learn-quickstart-test--read-file file)))
            (should (string-match-p "^#\\+TITLE: My Learning" content))
            (should (string-match-p "^#\\+FILETAGS: :learning:" content))
            (should (string-match-p "^\\* TODO .*:drill:" content))
            (should (string-match-p "^  :LEARN_REPS: 0$" content)))
          (should (get-file-buffer file)))
      (let ((buf (get-file-buffer file)))
        (when buf (kill-buffer buf)))
      (delete-directory dir t))))

(ert-deftest org-learn-quickstart-never-clobbers ()
  "Spec 010: an existing learning file is never clobbered.
The wizard meets the sentinel file, the create path refuses when
called on it directly, and the sentinel text survives with no
template content merged in."
  (let* ((dir (make-temp-file "org-learn-qs-clobber-" t))
         (file (expand-file-name "learn.org" dir))
         (org-learn-quickstart-file file))
    (unwind-protect
        (progn
          (write-region "SENTINEL keep me\n" nil file nil 'quiet)
          (cl-letf (((symbol-function 'y-or-n-p)
                     (org-learn-quickstart-test--answer-queue nil)))
            (org-learn-quickstart)
            (org-learn-quickstart--create-file file))
          (let ((content (org-learn-quickstart-test--read-file file)))
            (should (string-match-p "SENTINEL keep me" content))
            (should-not (string-match-p "My Learning" content))
            (should-not (string-match-p ":drill:" content))))
      (let ((buf (get-file-buffer file)))
        (when buf (kill-buffer buf)))
      (delete-directory dir t))))

(ert-deftest org-learn-quickstart-skips-cleanly ()
  "Spec 010: a declined offer skips its step without aborting.
Step 1 answers yes (create), capture and review answer no and are
never invoked (the flet counters stay 0), the tour is declined,
and the wizard completes without error."
  (let* ((dir (make-temp-file "org-learn-qs-skip-" t))
         (file (expand-file-name "learn.org" dir))
         (org-learn-quickstart-file file)
         (captures 0)
         (reviews 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'y-or-n-p)
                     (org-learn-quickstart-test--answer-queue
                      (list t nil nil nil)))
                    ((symbol-function 'org-learn-capture)
                     (lambda (&optional _title)
                       (interactive)
                       (setq captures (1+ captures))))
                    ((symbol-function 'org-learn-review)
                     (lambda () (setq reviews (1+ reviews)))))
            (org-learn-quickstart))
          (should (file-exists-p file))
          (should (= captures 0))
          (should (= reviews 0)))
      (let ((buf (get-file-buffer file)))
        (when buf (kill-buffer buf)))
      (delete-directory dir t))))

(ert-deftest org-learn-quickstart-binds-q ()
  "Spec 010: \"q\" under the C-c l prefix runs `org-learn-quickstart'."
  (should (commandp 'org-learn-quickstart))
  (should (eq (lookup-key org-learn-mode-keymap-prefix (kbd "q"))
              'org-learn-quickstart))
  (should (eq (lookup-key org-learn-mode-map (kbd "C-c l q"))
              'org-learn-quickstart)))

(ert-deftest org-learn-quickstart-template-idempotence ()
  "Spec 010: running the create path twice writes the template once.
The second call refuses (the file now exists), so no template
content is duplicated."
  (let* ((dir (make-temp-file "org-learn-qs-idem-" t))
         (file (expand-file-name "learn.org" dir))
         (org-learn-quickstart-file file))
    (unwind-protect
        (progn
          (org-learn-quickstart--create-file file)
          (org-learn-quickstart--create-file file)
          (let ((content (org-learn-quickstart-test--read-file file)))
            (should (= 1 (org-learn-quickstart-test--count-matches
                          "^#\\+TITLE:" content)))
            (should (= 1 (org-learn-quickstart-test--count-matches
                          ":drill:" content)))))
      (let ((buf (get-file-buffer file)))
        (when buf (kill-buffer buf)))
      (delete-directory dir t))))

(provide 'test-org-learn-quickstart)
;;; test-org-learn-quickstart.el ends here
