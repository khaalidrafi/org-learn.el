;;; tests/test-org-learn-roadmap-real.el --- Spec 011 real-dep ERT -*- lexical-binding: t; -*-

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

;; Spec 011 REAL-dependency ERT suite
;; (specs/011-roadmap-rollup.org): the spec 006 bridge is proven
;; against the REAL org-roadmap package, not only the stub.  When the
;; real org-roadmap is not on the `load-path' the whole suite skips
;; with a LOUD `ert-skip' message (CI default; the spec 006 stub is
;; NOT the real package and never satisfies this suite).  When it is
;; mounted (gate: -L /home/z/my-project/org-roadmap), the suite proves
;; the plan-from-roadmap -> progress write -> rollup round-trip on the
;; real parse and guards against stub drift: every org-roadmap symbol
;; the bridge consumes must exist in the real package with the
;; signature the bridge (and the stub) are written against --
;; including the 0-arg `org-roadmap-progress' (org-roadmap board
;; TODO-006; it parses the current buffer) and the
;; `org-roadmap-parse-todo-tree' parse primitive the bridge's trees
;; come from.  Loaded last by tests/test-org-learn.el so the single CI
;; entry runs everything.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'help-fns)
(require 'org-learn)
(require 'org-learn-roadmap)

(defconst org-learn-roadmap-real-test--stub-dir
  (if load-file-name
      (expand-file-name "support" (file-name-directory load-file-name))
    (expand-file-name "tests/support"))
  "Directory holding the org-roadmap test stub (never the real thing).")

(defconst org-learn-roadmap-real-test--candidate-dirs
  (let ((tests-dir (if load-file-name
                       (file-name-directory load-file-name)
                     (expand-file-name "tests/"))))
    ;; Spec layout: the real org-roadmap is the ../org-roadmap sibling
    ;; of the repo root; this workspace nests the seat worktree one
    ;; level deeper, hence the grandparent candidate too.
    (list (expand-file-name "../org-roadmap" (expand-file-name ".." tests-dir))
          (expand-file-name "../../org-roadmap"
                            (expand-file-name ".." tests-dir))))
  "Sibling directories the real org-roadmap may live in.")

(defconst org-learn-roadmap-real-test--bridge-api
  '((org-roadmap-parse-todo-tree (&optional _limit))
    (org-roadmap-get-todo-tree (&optional _limit))
    (org-roadmap-get-subtree-tree)
    (org-roadmap-generate (&optional subtree-p))
    (org-roadmap-progress)
    (org-roadmap-import-roadmap-sh (source &optional output-file)))
  "Arglist contract of the spec 006 bridge API.
Each entry is one org-roadmap symbol the bridge consumes (or the
parse primitive its trees come from) with the arglist the bridge
and the stub are written against; a 0-arg function is a
one-element entry, because `help-function-arglist' returns nil
for it.  The `declare-function' block of org-learn-roadmap.el and
the stub must keep matching the real package: the stub-drift
guard asserts it, so a rename or reshape here fails the suite,
not the user's session.")

(defun org-learn-roadmap-real-test--locate-real ()
  "Return the real org-roadmap.el path, or nil when absent.
The `load-path' wins (the gate mounts the sibling repo with -L);
the spec's sibling-directory candidates are the fallback."
  (or (locate-library "org-roadmap")
      (cl-loop for dir in org-learn-roadmap-real-test--candidate-dirs
               for candidate = (expand-file-name "org-roadmap.el" dir)
               when (file-exists-p candidate)
               return (progn
                        (add-to-list 'load-path (directory-file-name dir))
                        candidate))))

(defun org-learn-roadmap-real-test--require-real ()
  "Load the REAL org-roadmap, or skip loudly (spec 011).
A stub of the feature may already be loaded (the stub-world suites
run before this one); it is unloaded first, because these tests
must run against the real package.  Returns the loaded library
path and asserts the feature really came from it."
  (let ((library (org-learn-roadmap-real-test--locate-real)))
    (unless library
      (ert-skip (concat
                 "real org-roadmap is not on the load-path; mount the "
                 "sibling repo (gate: -L /home/z/my-project/org-roadmap) "
                 "or install org-roadmap first (M-x package-install RET "
                 "org-roadmap); spec 011 real-dependency suite")))
    (when (featurep 'org-roadmap)
      (unload-feature 'org-roadmap 'force))
    (load library nil 'nomessage)
    (should (featurep 'org-roadmap))
    ;; Guard the guard: the definitions must really come from the
    ;; real file, never from tests/support/org-roadmap-stub.el.
    (should (equal (file-truename (symbol-file 'org-roadmap-get-todo-tree))
                   (file-truename library)))
    library))

(defun org-learn-roadmap-real-test--kill-render-buffers ()
  "Kill the buffers the real renderer and report create."
  (dolist (name (list "*Org Roadmap*" "*org-learn-progress*"))
    (let ((buffer (get-buffer name)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

;;; Round-trip on the real package (spec 011: plan -> progress ->
;;; rollup against the REAL parse, not the stub).

(ert-deftest org-learn-roadmap-real-plan-progress-rollup-round-trip ()
  "Spec 011: plan, progress write and rollup round-trip for real.
`org-learn-plan-from-roadmap' plans from a real org-roadmap parse,
`org-learn-progress-report' writes :LEARN_PROGRESS: through the
real subtree parse and renders the real ASCII roadmap, and
`org-learn-roadmap-rollup' rolls the real tree up.  The 0-arg
`org-roadmap-progress' API (org-roadmap board TODO-006) is
exercised exactly the way `org-learn-progress-report' calls it:
roadmap buffer current, no arguments, alist out.  The report
records the org-roadmap rollup path that call delivers."
  (org-learn-roadmap-real-test--require-real)
  (let* ((dir (make-temp-file "org-learn-real-" t))
         (notes (expand-file-name "learn.org" dir))
         (roadmap-file (expand-file-name "roadmap.org" dir))
         (org-learn-file notes))
    (unwind-protect
        (with-current-buffer (find-file-noselect roadmap-file)
          (let ((org-learn-review-scope 'file))
            (erase-buffer)
            (insert "* Roadmap: real\n** TODO Alpha\n** TODO Beta\n")
            (org-mode)
            (goto-char (point-min))
            ;; 1. Plan from the REAL whole-buffer parse.
            (should (= 2 (org-learn-plan-from-roadmap)))
            (with-temp-buffer
              (insert-file-contents notes)
              (should (string-match-p "^\\* TODO Alpha :drill:$"
                                      (buffer-string)))
              (should (string-match-p "^  :LEARN_SOURCE: roadmap$"
                                      (buffer-string))))
            ;; Complete Alpha's review item behind the bridge's back.
            (with-temp-buffer
              (insert-file-contents notes)
              (goto-char (point-min))
              (re-search-forward "^\\* TODO Alpha :drill:")
              (replace-match "* DONE Alpha :drill:" t t)
              (write-region (point-min) (point-max) notes nil 'quiet))
            ;; 2. Progress write through the REAL subtree parse.
            (goto-char (point-min))
            (org-learn-progress-report)
            (org-map-entries
             (lambda ()
               (let ((title (org-get-heading t t t t))
                     (progress (org-entry-get (point) "LEARN_PROGRESS")))
                 (should progress)
                 (pcase title
                   ("Roadmap: real" (should (equal progress "50")))
                   ("Alpha" (should (equal progress "100")))
                   ("Beta" (should (equal progress "0"))))))
             nil nil)
            ;; 3. Rollup round-trip on the REAL subtree parse.  The
            ;; roadmap node Alpha itself is marked DONE: leaf keywords
            ;; drive the rollup, the root's derived :LEARN_PROGRESS:
            ;; never does.
            (goto-char (point-min))
            (re-search-forward "^\\*\\* TODO Alpha")
            (org-todo "DONE")
            (goto-char (point-min))
            (should (equal (org-learn-roadmap-rollup
                            (org-roadmap-get-subtree-tree))
                           (list (cons "Roadmap: real" (cons 1 2)))))
            ;; 3b. The REAL 0-arg progress API, called exactly the way
            ;; `org-learn-progress-report' now calls it: the roadmap
            ;; buffer is current here, no arguments, alist out
            ;; (org-roadmap board TODO-006).
            (should (equal (org-roadmap-progress)
                           (list (cons "Roadmap: real" (cons 1 2)))))
            ;; 4. The report states which rollup path ran: with the
            ;; real package loaded the 0-arg API wins, and the
            ;; recorded line is echoed for the gate log.
            (let ((report (get-buffer "*org-learn-progress*")))
              (should (buffer-live-p report))
              (with-current-buffer report
                (should (derived-mode-p 'org-mode))
                (should buffer-read-only)
                (let* ((text (buffer-string))
                       (path-line (and (string-match "^rollup: .*$" text)
                                       (match-string 0 text))))
                  (should (equal path-line "rollup: org-roadmap"))
                  (message "org-learn: real-dep report records %s"
                           path-line)
                  ;; The real ASCII roadmap landed in the report.
                  (should (string-match-p "Roadmap: real" text)))))
            (org-learn-roadmap-real-test--kill-render-buffers))
          (set-buffer-modified-p nil))
      (let ((buffer (get-file-buffer roadmap-file)))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (let ((buffer (get-file-buffer notes)))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-learn-roadmap-real-subtree-parse-carries-learn-progress ()
  "Spec 011: the real parse plus the written property feeds the rollup.
The real subtree parse carries the node markers; its :properties
alist holds special properties only (org-entry-properties with
'inherited under the default `org-use-property-inheritance' does
not list own drawer properties), so this test attaches the drawer
properties the real markers point at -- exactly the adapter a
consumer needs to bring a real parse into the spec 011 node shape.
A leaf-less milestone then inherits the percent exactly as encoded
by the bridge's progress write: (percent . 100) in the rollup."
  (org-learn-roadmap-real-test--require-real)
  (let* ((dir (make-temp-file "org-learn-realp-" t))
         (roadmap-file (expand-file-name "roadmap.org" dir)))
    (unwind-protect
        (with-current-buffer (find-file-noselect roadmap-file)
          (erase-buffer)
          (insert "\
* Solo milestone
:PROPERTIES:
:LEARN_PROGRESS: 40
:END:\n")
          (org-mode)
          (goto-char (point-min))
          (let* ((tree (org-roadmap-get-subtree-tree))
                 (tree (mapcar
                        (lambda (node)
                          (let ((loc (plist-get node :location)))
                            (when (and (markerp loc)
                                       (buffer-live-p (marker-buffer loc)))
                              (plist-put node :properties
                                         (org-entry-properties
                                          (marker-position loc))))
                            node))
                        tree)))
            (should (equal (org-learn-roadmap-rollup tree)
                           (list (cons "Solo milestone" (cons 40 100))))))
          (set-buffer-modified-p nil))
      (let ((buffer (get-file-buffer roadmap-file)))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory dir t))))

;;; Stub-drift guard (spec 011: a rename in org-roadmap must fail
;;; here, not the user's session).

(ert-deftest org-learn-roadmap-real-bridge-api-matches-real-package ()
  "Spec 011: every symbol the bridge consumes exists for real.
Each consumed symbol is fboundp in the REAL package with exactly
the arglist the bridge is written against -- the 0-arg
`org-roadmap-progress' included now that the bridge consumes it
for real (org-roadmap board TODO-006)."
  (org-learn-roadmap-real-test--require-real)
  (pcase-dolist (`(,symbol ,expected) org-learn-roadmap-real-test--bridge-api)
    (ert-info ((format "bridge API symbol: %s" symbol))
      (should (fboundp symbol))
      (should (equal expected (help-function-arglist symbol))))))

(ert-deftest org-learn-roadmap-real-stub-mirrors-real-signatures ()
  "Spec 011: the stub's public signatures still match the real ones.
The stub is loaded over the real package, its arglists are compared
symbol by symbol, and the real package is loaded back so the suite
never ends on stub definitions."
  (let ((real (org-learn-roadmap-real-test--require-real))
        (real-arglists
         (mapcar (lambda (entry)
                   (cons (car entry)
                         (help-function-arglist (car entry))))
                 org-learn-roadmap-real-test--bridge-api)))
    (unwind-protect
        (progn
          (load (expand-file-name "org-roadmap-stub.el"
                                  org-learn-roadmap-real-test--stub-dir)
                nil 'nomessage)
          (pcase-dolist (`(,symbol . ,expected) real-arglists)
            (ert-info ((format "stub mirror symbol: %s" symbol))
              (should (fboundp symbol))
              (should (equal expected (help-function-arglist symbol))))))
      ;; Restore the real package: this suite must leave the world
      ;; exactly as real as it found it.
      (load real nil 'nomessage)
      (should (equal (file-truename (symbol-file 'org-roadmap-get-todo-tree))
                     (file-truename real))))))

(ert-deftest org-learn-roadmap-real-menu-command ()
  "Spec 016: the one-click roadmap menu is real-dep wired end to end.
The command exists, the REAL org-roadmap is loaded, its catalog is
non-empty, and the public `org-roadmap-catalog-url' builds the
roadmap.sh page URL `org-learn-roadmap-menu' imports from.  The
keymap binding (C-c l m) is asserted in test-org-learn-mode.el --
this suite never loads org-learn-mode."
  (org-learn-roadmap-real-test--require-real)
  (should (fboundp 'org-learn-roadmap-menu))
  (should (boundp 'org-roadmap-catalog))
  (should (> (length org-roadmap-catalog) 50))
  (should (equal (org-roadmap-catalog-url "ai-engineer")
                 "https://roadmap.sh/ai-engineer")))

(provide 'test-org-learn-roadmap-real)
;;; test-org-learn-roadmap-real.el ends here
