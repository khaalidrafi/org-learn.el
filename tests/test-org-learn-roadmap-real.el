;;; tests/test-org-learn-roadmap-real.el --- Real org-roadmap ERT -*- lexical-binding: t; -*-

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

;; Spec 011 real-dependency suite (specs/011-roadmap-rollup.org):
;; proves the bridge contract against the REAL org-roadmap, not only
;; the stub.  The suite loads the real org-roadmap.el from the
;; `load-path' (the gate mounts the sibling repo with -L
;; ../org-roadmap); when the package is absent every test here skips
;; with a LOUD `ert-skip' message, so CI without the sibling stays
;; green.  Two proofs are required by the spec:
;;
;; - the round trip: plan review items from a real org-roadmap parse,
;;   write :LEARN_PROGRESS: per node through the progress report,
;;   then roll the tree up and check the milestone cells against the
;;   written percents;
;; - the stub-drift guard: the stub's public signatures
;;   (tests/support/org-roadmap-stub.el) still match the real
;;   package's, so the stub suite keeps proving the same contract.
;;
;; Loaded by tests/test-org-learn.el so the single CI entry runs
;; everything.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-learn)
(require 'org-learn-roadmap)

(defconst org-learn-roadmap-real-test--contract
  '(org-roadmap-get-todo-tree
    org-roadmap-get-subtree-tree
    org-roadmap-generate
    org-roadmap-import-roadmap-sh)
  "Public org-roadmap API the bridge consumes (spec 006).
The stub-drift guard asserts each symbol still exists in the real
package with the same required-argument signature the stub and the
bridge rely on.")

(defun org-learn-roadmap-real-test--required-args (symbol)
  "Return the required (non-optional) argument symbols of SYMBOL."
  (let ((arglist (help-function-arglist symbol)))
    (cl-loop for arg in arglist
             until (memq arg '(&optional &rest))
             collect arg)))

(defun org-learn-roadmap-real-test--real-loaded-p ()
  "Return non-nil when the loaded `org-roadmap' is the real package.
The stub lives in org-roadmap-stub.el, the real package in
org-roadmap.el, so the defining file tells them apart."
  (and (featurep 'org-roadmap)
       (let ((file (symbol-file 'org-roadmap-generate)))
         (and file (equal (file-name-nondirectory file)
                          "org-roadmap.el")))))

(defun org-learn-roadmap-real-test--load-real ()
  "Load the REAL org-roadmap; `ert-skip' loudly when it is absent.
Any loaded stub is unloaded first so the real definitions win."
  (when (and (featurep 'org-roadmap)
             (not (org-learn-roadmap-real-test--real-loaded-p)))
    (unload-feature 'org-roadmap 'force))
  (unless (org-learn-roadmap-real-test--real-loaded-p)
    (let ((lib (locate-library "org-roadmap.el")))
      (unless lib
        (ert-skip (concat
                   "org-roadmap.el is not on the load-path; "
                   "mount the sibling org-roadmap repo to run the "
                   "real-dependency suite (CI default skips it)")))
      (load lib nil 'nomessage)
      (should (org-learn-roadmap-real-test--real-loaded-p)))))

(defun org-learn-roadmap-real-test--feature-state ()
  "Return the current `org-roadmap' feature state as a symbol.
`real' when the real package is loaded, `stub' when the test stub
is loaded and nil when the feature is absent.  Captured before a
test loads anything so its cleanup can restore the exact state --
the rest of the suite asserts on the feature being absent, and ERT
makes no promise about execution order."
  (cond ((org-learn-roadmap-real-test--real-loaded-p) 'real)
        ((featurep 'org-roadmap) 'stub)
        (t nil)))

(defun org-learn-roadmap-real-test--restore-feature (state)
  "Restore the `org-roadmap' feature STATE captured earlier.
STATE is nil, `stub' or `real' (see
`org-learn-roadmap-real-test--feature-state'); anything loaded
right now is unloaded first."
  (when (featurep 'org-roadmap)
    (unload-feature 'org-roadmap 'force))
  (pcase state
    ('real (load (locate-library "org-roadmap.el") nil 'nomessage))
    ('stub (org-learn-roadmap-test--load-stub))
    (_ nil)))

;;; Real round trip (spec 011: plan -> progress write -> rollup).

(ert-deftest org-learn-roadmap-real-plan-progress-rollup-round-trip ()
  "Spec 011 round trip on the real org-roadmap.
`org-learn-plan-from-roadmap' plans from a real parse,
`org-learn-progress-report' writes :LEARN_PROGRESS: per node and
renders the ASCII roadmap through the real generator, and
`org-learn-roadmap-progress' rolls the tree up; the rollup percent
equals the percent written onto the root node."
  (let* ((initial (org-learn-roadmap-real-test--feature-state))
         (dir (make-temp-file "org-learn-rmreal-" t))
         (notes (expand-file-name "learn.org" dir))
         (org-learn-file notes)
         (buffer (get-buffer-create " *org-learn-roadmap-real*")))
    (unwind-protect
        (progn
          (org-learn-roadmap-real-test--load-real)
          (with-current-buffer buffer
            (erase-buffer)
            (insert "* Roadmap: real\n** TODO Alpha\n** TODO Beta\n")
            (org-mode)
            (goto-char (point-min))
            ;; Plan from a real org-roadmap parse.
            (should (= 2 (org-learn-plan-from-roadmap)))
            ;; Complete Alpha's review item behind the bridge's back.
            (with-temp-buffer
              (insert-file-contents notes)
              (goto-char (point-min))
              (re-search-forward "^\\* TODO Alpha :drill:")
              (replace-match "* DONE Alpha :drill:" t t)
              (write-region (point-min) (point-max) notes nil 'quiet))
            ;; Progress write through the real subtree parse + report.
            (goto-char (point-min))
            (org-learn-progress-report)
            (should (equal "50" (org-entry-get (point-min) "LEARN_PROGRESS")))
            (let ((report (get-buffer org-learn-roadmap-progress-buffer)))
              (should (buffer-live-p report))
              (with-current-buffer report
                (let ((text (buffer-string)))
                  ;; The real generator drew its boxed ASCII roadmap.
                  (should (string-match-p "┌" text))
                  ;; The report records which rollup path ran.
                  (if (fboundp 'org-roadmap-progress)
                      (should (string-match-p "^rollup: org-roadmap" text))
                    (should (string-match-p "^rollup: local fallback$" text)))))
              (kill-buffer report))
            ;; Rollup round trip: mark Alpha DONE on the roadmap side,
            ;; then the rollup percent equals the written property.
            (goto-char (point-min))
            (re-search-forward "^\\*\\* TODO Alpha$")
            (replace-match "** DONE Alpha" t t)
            (goto-char (point-min))
            (let* ((org-done-keywords '("DONE"))
                   (rollup (org-learn-roadmap-progress
                            (org-roadmap-get-todo-tree)))
                   (cell (cdr (assoc "Roadmap: real"
                                     (plist-get rollup :rollup)))))
              (should (stringp (plist-get rollup :path)))
              (should (equal cell '(1 . 2)))
              (should (= (string-to-number
                          (org-entry-get (point-min) "LEARN_PROGRESS"))
                         (org-learn-roadmap--percent (car cell)
                                                     (cdr cell)))))))
      (kill-buffer buffer)
      (delete-directory dir t)
      (org-learn-roadmap-real-test--restore-feature initial))))

;;; Stub-drift guard (spec 011).

(ert-deftest org-learn-roadmap-real-stub-drift-guard ()
  "Spec 011: the stub's public signatures still match the real
package's.  Fails when org-roadmap renames a consumed symbol or
changes a required-argument signature the bridge calls, so the
stub suite cannot silently drift away from the real contract."
  (let* ((initial (org-learn-roadmap-real-test--feature-state))
         (stub-args
         (progn
           ;; Capture the STUB signatures faithfully: unload whatever
           ;; org-roadmap is loaded, load the stub fresh, record, unload.
           (when (featurep 'org-roadmap)
             (unload-feature 'org-roadmap 'force))
           (org-learn-roadmap-test--load-stub)
           (let ((alist (mapcar (lambda (symbol)
                                  (cons symbol
                                        (org-learn-roadmap-real-test--required-args
                                         symbol)))
                                org-learn-roadmap-real-test--contract)))
             (unload-feature 'org-roadmap 'force)
             alist))))
    (unwind-protect
        (progn
          (org-learn-roadmap-real-test--load-real)
          (dolist (symbol org-learn-roadmap-real-test--contract)
            (should (fboundp symbol))
            (should (equal (cdr (assq symbol stub-args))
                           (org-learn-roadmap-real-test--required-args
                            symbol)))))
      (org-learn-roadmap-real-test--restore-feature initial))))

(provide 'test-org-learn-roadmap-real)
;;; test-org-learn-roadmap-real.el ends here
