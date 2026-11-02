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
;; generation into *Org Roadmap* and the roadmap.sh import
;; pass-through.  It goes on the `load-path' only inside
;; tests/test-org-learn-roadmap.el, proving the integration contract
;; without the real package.  Never ship or install this file.

;;; Code:

(require 'org)

(defun org-roadmap-stub--parse-tree ()
  "Parse every heading of the current buffer into flat tree nodes.
Nodes carry the org-roadmap plist shape: :headline :todo-state
:level :children :location."
  (let ((nodes nil))
    ;; Scope nil: the current buffer (respecting any restriction), so
    ;; the same parse serves the whole-buffer and subtree paths.
    (org-map-entries
     (lambda ()
       (push (list :headline (org-get-heading t t t t)
                   :todo-state (when (org-get-todo-state)
                                 (intern (org-get-todo-state)))
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
  "Stub of `org-roadmap-get-todo-tree': the whole-buffer tree."
  (org-roadmap-stub--build-tree (org-roadmap-stub--parse-tree)))

(defun org-roadmap-get-subtree-tree ()
  "Stub of `org-roadmap-get-subtree-tree': the tree at point."
  (save-excursion
    (org-back-to-heading t)
    (org-narrow-to-subtree)
    (unwind-protect
        (org-roadmap-stub--build-tree (org-roadmap-stub--parse-tree))
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
