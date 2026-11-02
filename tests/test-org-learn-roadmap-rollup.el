;;; tests/test-org-learn-roadmap-rollup.el --- Spec 011 rollup ERT -*- lexical-binding: t; -*-

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

;; ERT suite for the spec 011 roadmap progress rollup
;; (specs/011-roadmap-rollup.org): the pure `org-learn-roadmap-rollup'
;; math (table-driven over the fixed parsed-node shape, >= 6 cases),
;; the dashboard "Roadmap" line (present with roadmap nodes, absent
;; without; the existing spec 005 dashboard tests stay untouched) and
;; the spec 011 rollup-path record of `org-learn-progress-report'
;; ("rollup: org-roadmap" vs "rollup: local fallback").  The
;; `org-roadmap' stub (tests/support/org-roadmap-stub.el) is loaded
;; ONLY where integration with the bridge is exercised, per the spec
;; 006 stub strategy.  Loaded by tests/test-org-learn.el so the single
;; CI entry runs everything.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-learn)
(require 'org-learn-dashboard)
(require 'org-learn-roadmap)

(defconst org-learn-roadmap-rollup-test--stub-dir
  (if load-file-name
      (expand-file-name "support" (file-name-directory load-file-name))
    (expand-file-name "tests/support"))
  "Directory holding the org-roadmap test stub.")

(defun org-learn-roadmap-rollup-test--load-stub ()
  "Load the org-roadmap stub unless the feature is already there.
Local `load-path' binding only, exactly like the spec 006 suite:
the stub never leaks into the gate environment on its own."
  (unless (featurep 'org-roadmap)
    (let ((load-path (cons org-learn-roadmap-rollup-test--stub-dir load-path)))
      (load (expand-file-name "org-roadmap-stub.el"
                              org-learn-roadmap-rollup-test--stub-dir)
            nil 'nomessage)))
  (should (featurep 'org-roadmap)))

(defun org-learn-roadmap-rollup-test--node (headline todo-state level children
                                                     &optional properties)
  "Build one parsed roadmap node in the spec 011 fixed shape.
HEADLINE is the title string, TODO-STATE the keyword symbol (or
nil), LEVEL the heading level, CHILDREN the nested node list and
PROPERTIES the optional property alist (how :LEARN_PROGRESS: lands
in the org-roadmap subtree parse)."
  (append (list :headline headline
                :todo-state todo-state
                :level level
                :children children)
          (when properties (list :properties properties))))

(defun org-learn-roadmap-rollup-test--count-matches (regexp string)
  "Count non-overlapping REGEXP matches in STRING (line-anchored)."
  (let ((count 0))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq count (1+ count))))
    count))

(defun org-learn-roadmap-rollup-test--roadmap-buffer (text)
  "Return a fresh temp Org buffer holding the roadmap TEXT."
  (let ((buffer (get-buffer-create " *org-learn-rollup-test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert text)
      (org-mode)
      (goto-char (point-min)))
    buffer))

(defun org-learn-roadmap-rollup-test--kill-dashboard ()
  "Kill the dashboard buffer, leaving no litter behind."
  (let ((buffer (get-buffer org-learn-dashboard--buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer))))

;;; Rollup math (spec 011 acceptance: exact (done . total), pure).

(ert-deftest org-learn-roadmap-rollup-table-driven ()
  "Spec 011: rollup math is exact per milestone, table-driven.
One entry per top-level milestone, document order; leaves (childless
nodes at any depth) count DONE when their :todo-state is in
`org-done-keywords'; a leaf-less milestone inherits its
:LEARN_PROGRESS: percent when present and otherwise counts itself
by its own keyword."
  (let ((org-done-keywords '("DONE"))
        (cases
         (list
          ;; 1. Empty tree: no milestones, no rollup.
          (list "empty tree" nil nil)
          ;; 2. Single leaf, TODO: the milestone counts its one leaf.
          (list "single leaf todo"
                (list (org-learn-roadmap-rollup-test--node
                       "M" 'TODO 1
                       (list (org-learn-roadmap-rollup-test--node
                              "a" 'TODO 2 nil))))
                (list (cons "M" (cons 0 1))))
          ;; 3. Single leaf, DONE.
          (list "single leaf done"
                (list (org-learn-roadmap-rollup-test--node
                       "M" 'TODO 1
                       (list (org-learn-roadmap-rollup-test--node
                              "a" 'DONE 2 nil))))
                (list (cons "M" (cons 1 1))))
          ;; 4. Mixed states: exact split per milestone.
          (list "mixed states"
                (list (org-learn-roadmap-rollup-test--node
                       "M" 'TODO 1
                       (list (org-learn-roadmap-rollup-test--node
                              "a" 'DONE 2 nil)
                             (org-learn-roadmap-rollup-test--node
                              "b" 'TODO 2 nil))))
                (list (cons "M" (cons 1 2))))
          ;; 5. Nested rollup: leaves at any depth count once.
          (list "nested rollup"
                (list (org-learn-roadmap-rollup-test--node
                       "M" 'TODO 1
                       (list (org-learn-roadmap-rollup-test--node
                              "a" 'TODO 2
                              (list (org-learn-roadmap-rollup-test--node
                                     "x" 'DONE 3 nil)
                                    (org-learn-roadmap-rollup-test--node
                                     "y" 'TODO 3 nil)))
                             (org-learn-roadmap-rollup-test--node
                              "b" 'TODO 2 nil))))
                (list (cons "M" (cons 1 3))))
          ;; 6. Leaf-less milestone inherits :LEARN_PROGRESS: percent,
          ;;    encoded exactly as (percent . 100).
          (list "leaf-less milestone inherits LEARN_PROGRESS"
                (list (org-learn-roadmap-rollup-test--node
                       "M" 'TODO 1 nil
                       (list (cons "LEARN_PROGRESS" "50"))))
                (list (cons "M" (cons 50 100))))
          ;; 7. Inherited 100% means the milestone is done.
          (list "leaf-less milestone inherited 100 percent is done"
                (list (org-learn-roadmap-rollup-test--node
                       "M" nil 1 nil
                       (list (cons "LEARN_PROGRESS" "100"))))
                (list (cons "M" (cons 100 100))))
          ;; 8. All-done tree.
          (list "all done"
                (list (org-learn-roadmap-rollup-test--node
                       "M" 'TODO 1
                       (list (org-learn-roadmap-rollup-test--node
                              "a" 'DONE 2 nil)
                             (org-learn-roadmap-rollup-test--node
                              "b" 'DONE 2 nil)
                             (org-learn-roadmap-rollup-test--node
                              "c" 'DONE 2 nil))))
                (list (cons "M" (cons 3 3))))
          ;; 9. Leaf-less milestone without :LEARN_PROGRESS: counts
          ;;    itself by its own TODO keyword.
          (list "leaf-less milestone without progress todo"
                (list (org-learn-roadmap-rollup-test--node
                       "M" 'TODO 1 nil))
                (list (cons "M" (cons 0 1))))
          (list "leaf-less milestone without progress done"
                (list (org-learn-roadmap-rollup-test--node
                       "M" 'DONE 1 nil))
                (list (cons "M" (cons 1 1))))
          ;; 10. Several milestones, document order preserved.
          (list "multiple milestones in document order"
                (list (org-learn-roadmap-rollup-test--node
                       "A" 'DONE 1 nil)
                      (org-learn-roadmap-rollup-test--node
                       "B" 'TODO 1
                       (list (org-learn-roadmap-rollup-test--node
                              "b1" 'DONE 2 nil)
                             (org-learn-roadmap-rollup-test--node
                              "b2" 'TODO 2 nil))))
                (list (cons "A" (cons 1 1))
                      (cons "B" (cons 1 2))))
          ;; 11. :LEARN_PROGRESS: on a milestone WITH children is
          ;;     derived data: the leaves roll up, the property is
          ;;     never inherited there.
          (list "milestone with children ignores its LEARN_PROGRESS"
                (list (org-learn-roadmap-rollup-test--node
                       "M" 'TODO 1
                       (list (org-learn-roadmap-rollup-test--node
                              "a" 'TODO 2 nil))
                       (list (cons "LEARN_PROGRESS" "50"))))
                (list (cons "M" (cons 0 1))))
          ;; 12. Only `org-done-keywords' members count DONE.
          (list "non-done keyword does not count"
                (list (org-learn-roadmap-rollup-test--node
                       "M" 'TODO 1
                       (list (org-learn-roadmap-rollup-test--node
                              "a" 'DONE 2 nil)
                             (org-learn-roadmap-rollup-test--node
                              "b" 'WAITING 2 nil))))
                (list (cons "M" (cons 1 2)))))))
    (pcase-dolist (`(,name ,tree ,expected) cases)
      (ert-info ((format "rollup case: %s" name))
        (should (equal expected (org-learn-roadmap-rollup tree)))))))

(ert-deftest org-learn-roadmap-rollup-is-pure ()
  "Spec 011: the rollup never mutates its input tree.
Rolling the same tree twice yields the same result and the nodes
keep their shape."
  (let* ((org-done-keywords '("DONE"))
         (tree (list (org-learn-roadmap-rollup-test--node
                      "M" 'TODO 1
                      (list (org-learn-roadmap-rollup-test--node
                             "a" 'DONE 2 nil)))))
         (before (prin1-to-string tree))
         (first (org-learn-roadmap-rollup tree))
         (second (org-learn-roadmap-rollup tree)))
    (should (equal first second))
    (should (equal before (prin1-to-string tree)))))

;;; Dashboard-grade summary line (spec 011 deliverable 2).

(ert-deftest org-learn-roadmap-rollup-line-matches-spec-arithmetic ()
  "Spec 011: the summary line counts fully-done milestones.
The spec example \"Roadmap: 3/7 milestones done (43%)\" fixes the
arithmetic: percent = share of milestones with done = total,
rounded; the optional next-review clause appends when given."
  (let ((rollup (append (make-list 3 (cons "done" (cons 2 2)))
                        (make-list 4 (cons "wip" (cons 0 1))))))
    (should (equal "Roadmap: 3/7 milestones done (43%)"
                   (org-learn-roadmap-rollup-line rollup))))
  (should (equal "Roadmap: 1/2 milestones done (50%)"
                 (org-learn-roadmap-rollup-line
                  (list (cons "A" (cons 1 2))
                        (cons "B" (cons 1 1))))))
  (should (equal "Roadmap: 1/1 milestones done (100%), next review in 1d"
                 (org-learn-roadmap-rollup-line
                  (list (cons "A" (cons 3 3))) 1))))

(ert-deftest org-learn-roadmap-rollup-dashboard-line-present-and-absent ()
  "Spec 011: the dashboard renders the Roadmap line from the data.
It stays silent without it; the existing sections are untouched
either way."
  (with-temp-buffer
    (org-learn-dashboard--render
     (list :due 0 :new 0 :done-today 0 :queue nil :leeches nil
           :stats nil :next-review 1
           :roadmap (list (cons "M" (cons 3 3)))))
    (let ((text (buffer-string)))
      (should (string-match-p
               "^- Roadmap: 1/1 milestones done (100%), next review in 1d$"
               text))
      (should (string-match-p "^\\* Today$" text))
      (should (string-match-p "^\\* Review queue$" text))))
  (with-temp-buffer
    (org-learn-dashboard--render
     (list :due 0 :new 0 :done-today 0 :queue nil :leeches nil
           :stats nil :next-review nil :roadmap nil))
    (should-not (string-match-p "Roadmap:" (buffer-string)))))

;;; Dashboard integration (spec 011: present with roadmap nodes,
;;; silent without; existing dashboard tests stay green).

(ert-deftest org-learn-roadmap-rollup-dashboard-shows-the-roadmap-line ()
  "Spec 011: with roadmap nodes the dashboard renders the line.
With one leaf-less DONE milestone plus one half-done milestone in
the scoped file it reads \"1/2 milestones done (50%)\"."
  (org-learn-roadmap-rollup-test--load-stub)
  (let* ((file (make-temp-file "org-learn-rollup-" nil ".org"))
         (org-learn-file file)
         (org-learn-review-scope 'file))
    (unwind-protect
        (progn
          (write-region "\
* Roadmap: gate
** DONE Alpha
** TODO Beta
* DONE Solo
" nil file nil 'quiet)
          (org-learn-dashboard)
          (with-current-buffer org-learn-dashboard--buffer
            (should (string-match-p
                     "^- Roadmap: 1/2 milestones done (50%)$"
                     (buffer-string)))))
      (org-learn-roadmap-rollup-test--kill-dashboard)
      (let ((buffer (get-file-buffer file)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest org-learn-roadmap-rollup-dashboard-silent-without-roadmap-nodes ()
  "Spec 011: without roadmap nodes the dashboard stays silent.
A scoped file with no headings yields no tree, no rollup and no
Roadmap line."
  (org-learn-roadmap-rollup-test--load-stub)
  (let* ((file (make-temp-file "org-learn-rollup0-" nil ".org"))
         (org-learn-file file)
         (org-learn-review-scope 'file))
    (unwind-protect
        (progn
          (write-region "" nil file nil 'quiet)
          (org-learn-dashboard)
          (with-current-buffer org-learn-dashboard--buffer
            (should-not (string-match-p "Roadmap:" (buffer-string)))))
      (org-learn-roadmap-rollup-test--kill-dashboard)
      (let ((buffer (get-file-buffer file)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (file-exists-p file) (delete-file file)))))

;;; Rollup-path record (spec 011 deliverable 3).

(ert-deftest org-learn-roadmap-rollup-progress-records-local-fallback ()
  "Spec 011: without `org-roadmap-progress' the fallback is recorded.
The report says \"rollup: local fallback\" and the rollup feeds
the summary line."
  (org-learn-roadmap-rollup-test--load-stub)
  (should-not (fboundp 'org-roadmap-progress))
  (let* ((dir (make-temp-file "org-learn-rollupp-" t))
         (notes (expand-file-name "learn.org" dir))
         (org-learn-file notes)
         (roadmap (org-learn-roadmap-rollup-test--roadmap-buffer
                   "\
* Roadmap: gate
** TODO Alpha
** TODO Beta
* DONE Solo\n")))
    (unwind-protect
        (progn
          (with-current-buffer roadmap
            (org-learn-plan-from-roadmap)
            ;; Complete Alpha's review item behind the bridge's back.
            (with-temp-buffer
              (insert (with-temp-buffer
                        (insert-file-contents notes)
                        (buffer-string)))
              (goto-char (point-min))
              (re-search-forward "^\\* TODO Alpha :drill:")
              (replace-match "* DONE Alpha :drill:" t t)
              (write-region (point-min) (point-max) notes nil 'quiet))
            (goto-char (point-min))
            (org-learn-progress-report))
          (let ((report (get-buffer "*org-learn-progress*")))
            (should (buffer-live-p report))
            (with-current-buffer report
              (let ((text (buffer-string)))
                (should (string-match-p "^rollup: local fallback$" text))
                ;; The progress command rolls up the subtree at
                ;; point: the gate root with both leaves still TODO
                ;; (Solo is outside the subtree).
                (should (string-match-p
                         "^Roadmap: 0/1 milestones done (0%)$" text)))))
          (kill-buffer "*org-learn-progress*"))
      (kill-buffer roadmap)
      (delete-directory dir t))))

(ert-deftest org-learn-roadmap-rollup-progress-prefers-org-roadmap-api ()
  "Spec 011: when `org-roadmap-progress' exists its rollup wins.
The report records the org-roadmap path and the API's own rollup."
  (org-learn-roadmap-rollup-test--load-stub)
  (cl-letf (((symbol-function 'org-roadmap-progress)
             (lambda (&rest _) (list (cons "API milestone" (cons 2 2))))))
    (let* ((dir (make-temp-file "org-learn-rollupa-" t))
           (notes (expand-file-name "learn.org" dir))
           (org-learn-file notes)
           (roadmap (org-learn-roadmap-rollup-test--roadmap-buffer
                     "\
* Roadmap: gate
** TODO Alpha
** TODO Beta\n")))
      (unwind-protect
          (progn
            (with-current-buffer roadmap
              (org-learn-plan-from-roadmap)
              (goto-char (point-min))
              (org-learn-progress-report))
            (let ((report (get-buffer "*org-learn-progress*")))
              (should (buffer-live-p report))
              (with-current-buffer report
                (let ((text (buffer-string)))
                  (should (string-match-p "^rollup: org-roadmap" text))
                  ;; The API's own rollup, not the local computation.
                  (should (string-match-p
                           "^Roadmap: 1/1 milestones done (100%)$" text)))))
            (kill-buffer "*org-learn-progress*"))
        (kill-buffer roadmap)
        (delete-directory dir t)))))

(provide 'test-org-learn-roadmap-rollup)
;;; test-org-learn-roadmap-rollup.el ends here
