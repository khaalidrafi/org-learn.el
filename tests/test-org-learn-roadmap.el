;;; tests/test-org-learn-roadmap.el --- ERT suite for the roadmap bridge -*- lexical-binding: t; -*-

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

;; ERT suite for the spec 006 roadmap bridge
;; (specs/006-roadmap-bridge.org): planning review items from a
;; roadmap TODO tree, the per-node :LEARN_PROGRESS: progress report
;; and the roadmap.sh import pass-through.  The `org-roadmap' stub
;; (tests/support/org-roadmap-stub.el) is put on the `load-path' ONLY
;; inside this suite, per the spec stub strategy, so the integration
;; contract is proven without the real package.  The dependency law
;; test replaces the old org-learn-keeps-the-org-roadmap-boundary
;; test of tests/test-org-learn.el (Owner order 2026-11-01 18:28Z).
;; Loaded by tests/test-org-learn.el so the single CI entry runs
;; everything.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-learn)
(require 'org-learn-roadmap)

(defconst org-learn-roadmap-test--stub-dir
  (if load-file-name
      (expand-file-name "support" (file-name-directory load-file-name))
    (expand-file-name "tests/support"))
  "Directory holding the org-roadmap test stub.")

(defun org-learn-roadmap-test--load-stub ()
  "Load the org-roadmap stub unless the feature is already there.
The stub file (org-roadmap-stub.el, providing `org-roadmap') is
loaded explicitly with the stub directory prepended to a LOCAL
binding of `load-path' only, so the gate environment never picks
the stub up on its own."
  (unless (featurep 'org-roadmap)
    (let ((load-path (cons org-learn-roadmap-test--stub-dir load-path)))
      (load (expand-file-name "org-roadmap-stub.el"
                              org-learn-roadmap-test--stub-dir)
            nil 'nomessage)))
  (should (featurep 'org-roadmap)))

(defun org-learn-roadmap-test--read-file (file)
  "Return FILE's contents as a string, without visiting it."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-learn-roadmap-test--count-matches (regexp string)
  "Count non-overlapping REGEXP matches in STRING (line-anchored)."
  (let ((count 0))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq count (1+ count))))
    count))

(defun org-learn-roadmap-test--roadmap-buffer ()
  "Return a fresh temp buffer holding a two-topic roadmap tree."
  (let ((buffer (get-buffer-create " *org-learn-roadmap-test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Roadmap: test\n** TODO Alpha\n** TODO Beta\n")
      (org-mode)
      (goto-char (point-min)))
    buffer))

;;; Dependency law (spec 006; Owner order 2026-11-01 18:28Z).

(ert-deftest org-learn-roadmap-dependency-law-core-loads-bridge-user-errors ()
  "Spec 006 dependency law; replaces the old boundary test.
By Owner order 2026-11-01 18:28Z (#hq: \"org-learn should depend on
org-roadmap for planning, progress etc.\") the old
org-learn-keeps-the-org-roadmap-boundary law is superseded: the
dependency is real.  The core still loads with org-roadmap absent
\(feature `org-learn' is up in this suite), the bridge commands are
commands, and each of them signals `user-error' with an install
hint when the org-roadmap feature is missing (stub not reachable)."
  (should (featurep 'org-learn))
  (should (commandp 'org-learn-plan-from-roadmap))
  (should (commandp 'org-learn-progress-report))
  (should (commandp 'org-learn-roadmap-import))
  (when (featurep 'org-roadmap)
    (unload-feature 'org-roadmap 'force))
  (should-not (featurep 'org-roadmap))
  ;; An empty `load-path' makes the lazy in-command require fail even
  ;; if a stub or the real package sits somewhere reachable.
  (let ((load-path nil))
    (should-error (org-learn-plan-from-roadmap) :type 'user-error)
    (should-error (org-learn-progress-report) :type 'user-error)
    (should-error (org-learn-roadmap-import "stub://x") :type 'user-error)))

;;; Plan from roadmap (spec 006).

(ert-deftest org-learn-roadmap-plan-creates-one-review-item-per-topic ()
  "Spec 006: every topic heading gets one :drill: review item.
`org-learn-plan-from-roadmap' adds a TODO heading per level-2
topic to `org-learn-file' -- same title, :LEARN_SOURCE: roadmap,
reset LEARN_* properties -- and a second run adds nothing
\(idempotence law)."
  (org-learn-roadmap-test--load-stub)
  (let* ((dir (make-temp-file "org-learn-rm-" t))
         (notes (expand-file-name "learn.org" dir))
         (org-learn-file notes)
         (roadmap (org-learn-roadmap-test--roadmap-buffer)))
    (unwind-protect
        (with-current-buffer roadmap
          (should (= 2 (org-learn-plan-from-roadmap)))
          (let ((content (org-learn-roadmap-test--read-file notes)))
            (should (= 1 (org-learn-roadmap-test--count-matches
                          "^\\* TODO Alpha :drill:" content)))
            (should (= 1 (org-learn-roadmap-test--count-matches
                          "^\\* TODO Beta :drill:" content)))
            (should (string-match-p "^  :LEARN_SOURCE: roadmap$" content))
            (should (string-match-p "^  :LEARN_EASE: 2.5$" content))
            (should (string-match-p "^  :LEARN_REPS: 0$" content)))
          ;; Idempotent: existing titles are never duplicated.
          (should (zerop (org-learn-plan-from-roadmap)))
          (should (= 2 (org-learn-roadmap-test--count-matches
                        "^\\* TODO .* :drill:$"
                        (org-learn-roadmap-test--read-file notes)))))
      (kill-buffer roadmap)
      (delete-directory dir t))))

(ert-deftest org-learn-roadmap-plan-honors-topic-level-custom ()
  "Spec 006: `org-learn-roadmap-topic-level' selects the topic level.
The default is 2; with the custom bound to 1 the level-1 headings
become the planned topics."
  (should (= org-learn-roadmap-topic-level 2))
  (org-learn-roadmap-test--load-stub)
  (let* ((dir (make-temp-file "org-learn-rml-" t))
         (notes (expand-file-name "learn.org" dir))
         (org-learn-file notes)
         (org-learn-roadmap-topic-level 1)
         (buffer (get-buffer-create " *org-learn-roadmap-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          (insert "* TODO Alpha\n* TODO Beta\n")
          (org-mode)
          (goto-char (point-min))
          (should (= 2 (org-learn-plan-from-roadmap)))
          (should (= 2 (org-learn-roadmap-test--count-matches
                        "^\\* TODO .* :drill:$"
                        (org-learn-roadmap-test--read-file notes)))))
      (kill-buffer buffer)
      (delete-directory dir t))))

;;; Progress report (spec 006).

(ert-deftest org-learn-roadmap-progress-writes-property-and-renders ()
  "Spec 006: progress writes :LEARN_PROGRESS: per node and renders.
One review item is DONE (100%), the other is not (0%); the roadmap
root aggregates both (50%).  The *org-learn-progress* buffer is a
read-only org-mode buffer naming every topic."
  (org-learn-roadmap-test--load-stub)
  (let* ((dir (make-temp-file "org-learn-rmp-" t))
         (notes (expand-file-name "learn.org" dir))
         (org-learn-file notes)
         (roadmap (org-learn-roadmap-test--roadmap-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer roadmap
            (org-learn-plan-from-roadmap))
          ;; Complete Alpha's review item behind the bridge's back.
          (with-temp-buffer
            (insert (org-learn-roadmap-test--read-file notes))
            (goto-char (point-min))
            (re-search-forward "^\\* TODO Alpha :drill:")
            (replace-match "* DONE Alpha :drill:" t t)
            (write-region (point-min) (point-max) notes nil 'quiet))
          (with-current-buffer roadmap
            (goto-char (point-min))
            (org-learn-progress-report)
            ;; :LEARN_PROGRESS: landed on every roadmap node.
            (org-map-entries
             (lambda ()
               (let ((title (org-get-heading t t t t))
                     (progress (org-entry-get (point) "LEARN_PROGRESS")))
                 (should progress)
                 (pcase title
                   ("Roadmap: test" (should (equal progress "50")))
                   ("Alpha" (should (equal progress "100")))
                   ("Beta" (should (equal progress "0"))))))
             nil nil))
          (let ((report (get-buffer "*org-learn-progress*")))
            (should (buffer-live-p report))
            (with-current-buffer report
              (should (derived-mode-p 'org-mode))
              (should buffer-read-only)
              (let ((text (buffer-string)))
                (should (string-match-p "Roadmap: test" text))
                (should (string-match-p "Alpha" text))
                (should (string-match-p "Beta" text)))))
          (kill-buffer "*org-learn-progress*"))
      (kill-buffer roadmap)
      (delete-directory dir t))))

;;; roadmap.sh import pass-through (spec 006).

(ert-deftest org-learn-roadmap-import-is-a-pass-through-that-offers-planning ()
  "Spec 006: import passes through to org-roadmap, then offers planning.
Accepting the offer plans review items from the imported tree in
the current buffer; declining leaves `org-learn-file' untouched.
The import itself is never re-implemented in org-learn."
  (org-learn-roadmap-test--load-stub)
  (let* ((dir (make-temp-file "org-learn-rmi-" t))
         (notes (expand-file-name "learn.org" dir))
         (org-learn-file notes)
         (roadmap (get-buffer-create " *org-learn-roadmap-test*")))
    (unwind-protect
        (progn
          ;; Accepting the offer: import + plan.
          (with-current-buffer roadmap
            (erase-buffer)
            (org-mode)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (org-learn-roadmap-import "stub://roadmap"))
            (should (string-match-p
                     "^\\* TODO Roadmap: stub://roadmap$"
                     (buffer-string)))
            (should (= 2 (org-learn-roadmap-test--count-matches
                          "^\\* TODO .* :drill:$"
                          (org-learn-roadmap-test--read-file notes)))))
          ;; Declining the offer: no review items get planned.
          (delete-file notes)
          (with-current-buffer roadmap
            (erase-buffer)
            (org-mode)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
              (org-learn-roadmap-import "stub://roadmap-2"))
            (should-not (file-exists-p notes))))
      (kill-buffer roadmap)
      (delete-directory dir t))))

(provide 'test-org-learn-roadmap)
;;; test-org-learn-roadmap.el ends here
