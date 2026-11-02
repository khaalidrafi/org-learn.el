;;; org-roadmap-stub.el --- Minimal org-roadmap stand-in for ERT -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Khalid

;; Author: Khalid
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.0"))
;; Keywords: org, learning, test

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Spec 006 stub strategy (specs/006-roadmap-bridge.org): provide the
;; `org-roadmap' feature with just the minimal public API the
;; org-learn bridge calls -- whole-buffer/subtree tree parsing, ASCII
;; generation into *Org Roadmap*, the roadmap.sh import pass-through
;; and the 0-arg progress alist (org-roadmap board TODO-006).  Every
;; public defun mirrors the REAL package's signature and return shape
;; exactly, names included: the spec 011 stub-drift guard compares
;; `help-function-arglist' symbol by symbol, so a drift here fails the
;; suite, not the user's session.  The stub goes on the `load-path'
;; only inside the roadmap test suites, proving the integration
;; contract without the real package.  Never ship or install this
;; file.

;;; Code:

(require 'org)

(defun org-roadmap-parse-todo-tree (&optional _limit)
  "Stub of `org-roadmap-parse-todo-tree': the flat node list.
Parses every heading of the current buffer (respecting any
restriction, so the same parse serves the whole-buffer and
subtree paths) into the real flat node shape: :headline
:todo-state :tags :level :children :location per node, document
order.  _LIMIT is accepted and ignored, exactly like the real
optional argument."
  (let ((nodes nil))
    (org-map-entries
     (lambda ()
       (push (list :headline (org-get-heading t t t t)
                   :todo-state (when (org-get-todo-state)
                                 (intern (org-get-todo-state)))
                   :tags (org-get-tags nil 'local)
                   :level (org-current-level)
                   :children nil
                   :location (point-marker))
             nodes)))
    (nreverse nodes)))

(defun org-roadmap-stub--build-tree (flat)
  "Nest the FLAT node list by :level, like the real tree builder."
  (let ((roots nil)
        (stack nil))
    (dolist (node flat)
      (let ((level (plist-get node :level)))
        (while (and stack (>= (caar stack) level))
          (setq stack (cdr stack)))
        (let ((parent (cdar stack)))
          (if parent
              (plist-put parent :children
                         (append (plist-get parent :children) (list node)))
            (push node roots)))
        (push (cons level node) stack)))
    (nreverse roots)))

(defun org-roadmap-stub--flatten (tree)
  "Return every node of TREE in document order."
  (apply #'append
         (mapcar (lambda (node)
                   (cons node (org-roadmap-stub--flatten
                               (plist-get node :children))))
                 tree)))

(defun org-roadmap-get-todo-tree (&optional _limit)
  "Stub of `org-roadmap-get-todo-tree': the whole-buffer tree.
Mirrors the real composition: the flat parse nested by :level."
  (org-roadmap-stub--build-tree (org-roadmap-parse-todo-tree)))

(defun org-roadmap-get-subtree-tree ()
  "Stub of `org-roadmap-get-subtree-tree': the tree at point."
  (save-excursion
    (org-back-to-heading t)
    (org-narrow-to-subtree)
    (unwind-protect
        (org-roadmap-stub--build-tree (org-roadmap-parse-todo-tree))
      (widen))))

(defun org-roadmap-generate (&optional subtree-p)
  "Stub of `org-roadmap-generate': ASCII lines into *Org Roadmap*.
Renders SUBTREE-P the same way the bridge asks for it: one boxed
line per node, headline text included."
  (let* ((tree (if subtree-p
                   (org-roadmap-get-subtree-tree)
                 (org-roadmap-get-todo-tree)))
         (buf (get-buffer-create "*Org Roadmap*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (node (org-roadmap-stub--flatten tree))
          (insert (format "── [%s] %s\n"
                          (or (plist-get node :todo-state) " ")
                          (plist-get node :headline))))
        (goto-char (point-min))))
    (display-buffer buf)))

(defun org-roadmap-stub--progress-cell (node)
  "Return the (DONE . TOTAL) progress cell for parse NODE.
Stub mirror of the real private cell: a leaf counts individually
by keyword (DONE marks it done, any other TODO keyword marks it
open, no keyword counts nothing), a container rolls up its
subtree, and a rollup with no TODO leaves falls back to the
numeric :LEARN_PROGRESS: property of NODE's :location heading
with 0-100 mapped to X/100, else to (0 . 0).  Pure: reads NODE's
buffer, never modifies it."
  (let* ((children (plist-get node :children))
         (cell (if (not children)
                   (cond
                    ((eq (plist-get node :todo-state) 'DONE) (cons 1 1))
                    ((plist-get node :todo-state) (cons 0 1))
                    (t (cons 0 0)))
                 (let ((acc (cons 0 0)))
                   (dolist (child children acc)
                     (let ((sub (org-roadmap-stub--progress-cell child)))
                       (setcar acc (+ (car acc) (car sub)))
                       (setcdr acc (+ (cdr acc) (cdr sub)))))))))
    (if (zerop (cdr cell))
        (let ((loc (plist-get node :location))
              (fallback (cons 0 0)))
          (if (and (markerp loc) (marker-buffer loc)
                   (marker-position loc))
              (let ((value (with-current-buffer (marker-buffer loc)
                             (save-excursion
                               (goto-char (marker-position loc))
                               (org-entry-get nil "LEARN_PROGRESS")))))
                (if (and (stringp value)
                         (string-match-p
                          "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" value))
                    (let ((n (string-to-number value)))
                      (if (and (>= n 0) (<= n 100))
                          (cons (round n) 100)
                        fallback))
                  fallback))
            fallback))
      cell)))

(defun org-roadmap-progress ()
  "Stub of `org-roadmap-progress': the whole-buffer progress alist.
Mirrors the real 0-arg contract: parses the current buffer and
maps each top-level milestone heading to a (DONE . TOTAL) cell,
returning ((MILESTONE . (DONE . TOTAL)) ...) in buffer order, nil
for a buffer with no headings.  No buffer modification, no faces
-- data only."
  (let ((tree (org-roadmap-get-todo-tree)))
    (and tree
         (mapcar (lambda (node)
                   (cons (plist-get node :headline)
                         (org-roadmap-stub--progress-cell node)))
                 tree))))

(defun org-roadmap-import-roadmap-sh (source &optional output-file)
  "Stub importer: insert a fixed TODO tree for SOURCE.
Mirrors the real contract for the simple {title, items} shape: a
level-1 root heading plus level-2 topic headings.  A nil
OUTPUT-FILE inserts into the current buffer; otherwise the tree is
written to and saved in OUTPUT-FILE."
  (with-current-buffer (if output-file
                           (find-file-noselect output-file)
                         (current-buffer))
    (org-mode)
    (goto-char (point-max))
    (insert (format "\n* TODO Roadmap: %s\n" source))
    (dolist (title '("Alpha" "Beta"))
      (insert (format "** TODO %s\n" title)))
    (when output-file
      (save-buffer))
    (message "Stub import of %s" source)))

(provide 'org-roadmap)
;;; org-roadmap-stub.el ends here
