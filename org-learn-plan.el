;;; org-learn-plan.el --- learning-plan generator (spec 015) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Khalid

;; Author: Khalid
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.0"))
;; Keywords: org, learning, plan

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Spec 015, CORE segment: from a goal plus the learner's own notes,
;; generate an Org plan -- a roadmap-shaped TODO tree (root,
;; milestones, tasks) -- that flows into the existing org-roadmap
;; machinery with zero new rendering: `org-roadmap-get-todo-tree' and
;; the 0-arg `org-roadmap-progress' parse it, `org-learn-plan-from-
;; roadmap' drills its milestones, `org-learn-progress-report' writes
;; :LEARN_PROGRESS: onto its nodes.
;;
;; The core generator is DETERMINISTIC: topics come from the current
;; subtree, or from the TODO headings of `org-learn-file', in document
;; order; milestones are consecutive chunks of
;; `org-learn-plan-milestone-size' (cognitive-load management, the
;; LearnLM principle the DoE notice pinned).  The AI decomposition
;; rider is QUEUED by the spec's coordination law (after the
;; org-learn-ai.el segments of specs 009/012/013 merge) and is
;; deliberately absent here.
;;
;; The drill offer goes through the seam `org-learn-plan--roadmap-
;; available-p' (the spec 011 seam law: tests bind the seam, never the
;; function cell), and the drill itself is the bridge command's job --
;; this file never re-implements org-roadmap.

;;; Code:

(require 'org)
(require 'cl-lib)

;; The drill bridge, autoloaded from org-learn.el (spec 006 law);
;; declared so the byte compiler holds its peace.
(declare-function org-learn-plan-from-roadmap "org-learn-roadmap" ())

(defgroup org-learn-plan nil
  "Learning-plan generator: goal + notes to a roadmap-shaped plan."
  :prefix "org-learn-plan-"
  :group 'org-learn)

(defcustom org-learn-plan-buffer-name "*org-learn-plan*"
  "Name of the buffer holding the generated plan.
Every generation replaces the previous plan: the buffer is the
current draft, the save offer is the only write path."
  :type 'string
  :group 'org-learn-plan)

(defcustom org-learn-plan-max-topics 40
  "Maximum topics a plan is generated from.
A 500-heading subtree must never become a 167-milestone plan:
past the cap the list is truncated and the command says so."
  :type 'natnum
  :group 'org-learn-plan)

(defcustom org-learn-plan-milestone-size 3
  "Consecutive topics grouped into one milestone.
An integer >= 1; the last milestone may be smaller."
  :type 'natnum
  :group 'org-learn-plan)

;;;; Topic source (deterministic, ordered)

(defun org-learn-plan--dedupe (titles)
  "Return TITLES trimmed and deduplicated, first occurrence wins."
  (let ((seen (make-hash-table :test #'equal))
        (out nil))
    (dolist (title titles)
      (let ((clean (string-trim title)))
        (when (and (not (string-empty-p clean))
                   (not (gethash clean seen)))
          (puthash clean t seen)
          (push clean out))))
    (nreverse out)))

(defun org-learn-plan--subtree-topics ()
  "Return the heading titles strictly inside the current subtree.
Document order, trimmed, deduplicated; the root heading itself is
excluded.  Nil when the buffer is not Org or point sits on no
heading."
  (when (and (derived-mode-p 'org-mode)
             (ignore-errors (org-back-to-heading t)))
    (save-excursion
      (let ((level (org-outline-level))
            (end (save-excursion (org-end-of-subtree t)))
            (titles nil))
        (org-next-visible-heading 1)
        (while (< (point) end)
          (when (> (org-outline-level) level)
            (push (org-get-heading t t t t) titles))
          (org-next-visible-heading 1))
        (org-learn-plan--dedupe (nreverse titles))))))

(defun org-learn-plan--file-topics ()
  "Return the TODO-state heading titles of `org-learn-file'.
Document order, trimmed, deduplicated; the file is read in a temp
buffer and never visited.  Nil when the file is missing."
  (let ((file (and (boundp 'org-learn-file)
                   org-learn-file
                   (expand-file-name org-learn-file))))
    (when (and file (file-exists-p file))
      (let ((titles nil))
        (with-temp-buffer
          (insert-file-contents file)
          (org-mode)
          (org-map-entries
           (lambda ()
             (let ((todo (org-get-todo-state)))
               (when (and todo (not (member todo org-done-keywords)))
                 (push (org-get-heading t t t t) titles)))))
          (org-learn-plan--dedupe (nreverse titles)))))))

(defun org-learn-plan--cap (topics)
  "Apply the cap law to TOPICS: truncate past
`org-learn-plan-max-topics' and message the cut."
  (if (> (length topics) org-learn-plan-max-topics)
      (progn
        (message "org-learn: plan capped at %d topics"
                 org-learn-plan-max-topics)
        (seq-take topics org-learn-plan-max-topics))
    topics))

;;;; Milestone grouping and rendering (pure)

(defun org-learn-plan--milestones (topics)
  "Partition TOPICS into consecutive milestone chunks.
Each chunk carries `org-learn-plan-milestone-size' topics; the
title of a milestone is its first topic, so the plan reads as a
promise followed by its steps.  Nil for empty TOPICS; the size is
guarded, a non-positive size is a user error."
  (when (<= org-learn-plan-milestone-size 0)
    (user-error "org-learn-plan-milestone-size must be >= 1"))
  (when topics
    (let ((size org-learn-plan-milestone-size)
          (chunks nil))
      (while topics
        (let ((chunk (seq-take topics size)))
          (push (cons (car chunk) chunk) chunks)
          (setq topics (seq-drop topics size))))
      (nreverse chunks))))

(defun org-learn-plan--render (goal source milestones)
  "Return the complete plan text for GOAL, SOURCE and MILESTONES.
Byte-exact per the spec 015 plan format law: root TODO with a
:GOAL:/:SOURCE: drawer, level-2 milestones, level-3 tasks, one
blank line and the origin footer.  MILESTONES is the grouping
law's output, already titled -- the AI rider (queued) would feed
its regrouped milestones through this same renderer."
  (concat (format "* TODO Plan: %s\n" goal)
          "  :PROPERTIES:\n"
          (format "  :GOAL: %s\n" goal)
          (format "  :SOURCE: %s\n" source)
          "  :END:\n"
          (mapconcat
           (lambda (i-and-milestone)
             (let ((i (car i-and-milestone))
                   (milestone (cdr i-and-milestone)))
               (concat (format "** TODO Milestone %d: %s\n"
                               (1+ i) (car milestone))
                       (mapconcat
                        (lambda (task)
                          (format "*** TODO %s\n" task))
                        (cdr milestone) ""))))
           (cl-loop for milestone in milestones
                    for i from 0 collect (cons i milestone))
           "")
          "\n"
          (format "# Generated by org-learn plan (spec 015) from %s.\n"
                  source)))

;;;; Core

(defun org-learn-plan--generate (goal)
  "Generate the plan for GOAL and return its buffer.
No buffer is touched before the goal passes the guard; the plan
replaces whatever the plan buffer held (the reset law), and no
window is selected -- the core is batch-safe."
  (setq goal (string-trim (or goal "")))
  (when (string-empty-p goal)
    (user-error "org-learn: no goal for the plan"))
  (let* ((subtree (org-learn-plan--subtree-topics))
         (source (if subtree "subtree" "org-learn-file"))
         (topics (or subtree (org-learn-plan--file-topics))))
    (unless topics
      (user-error "org-learn: no notes to plan from (no subtree topics and no org-learn-file)"))
    (setq topics (org-learn-plan--cap topics))
    (let* ((milestones (org-learn-plan--milestones topics))
           (text (org-learn-plan--render goal source milestones))
           (buffer (get-buffer-create org-learn-plan-buffer-name)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text)
          (org-mode)
          (goto-char (point-min))))
      buffer)))

;;;; Seam

(defun org-learn-plan--roadmap-available-p ()
  "Non-nil when org-roadmap is loadable.
The spec 011 seam law: tests bind this predicate, never `require'
-- the `org-learn-roadmap--progress-api-available-p' precedent."
  (and (require 'org-roadmap nil t) t))

;;;; Command

;;;###autoload
(defun org-learn-plan-generate ()
  "Generate a learning plan from a goal and the learner's notes.
The goal defaults to the current heading's text in an Org buffer;
topics come from the current subtree, else from the TODO headings
of `org-learn-file'.  After the plan buffer is shown, the drill
offer (first -- it mutates `org-learn-file' while the plan is
fresh) turns the milestones into :drill: review items through
`org-learn-plan-from-roadmap', and the save offer writes the plan
to an Org file; declined, the buffer stays as a draft."
  (interactive)
  (let* ((default (save-excursion
                    (when (and (derived-mode-p 'org-mode)
                               (ignore-errors (org-back-to-heading t)))
                      (org-get-heading t t t t))))
         (goal (string-trim
                (or (read-string "Learning goal: "
                                 nil 'org-learn-plan-history default)
                    ""))))
    (when (string-empty-p goal)
      (user-error "org-learn: no goal for the plan"))
    (let ((buffer (org-learn-plan--generate goal))
          (count 0)
          (tasks 0))
      (pop-to-buffer buffer)
      (with-current-buffer buffer
        (org-map-entries
         (lambda ()
           (let ((level (org-outline-level)))
             (cond ((= level 2) (setq count (1+ count)))
                   ((> level 2) (setq tasks (1+ tasks))))))))
      (message "org-learn: plan ready (%d milestones, %d tasks)"
               count tasks)
      ;; Drill offer first (it mutates org-learn-file), then save.
      (when (and (org-learn-plan--roadmap-available-p)
                 (yes-or-no-p
                  "Create :drill: review items from the plan milestones? "))
        (with-current-buffer buffer
          (goto-char (point-min))
          (org-learn-plan-from-roadmap)))
      (when (yes-or-no-p "Save the plan to an Org file? ")
        (let ((file (read-file-name "Save plan to: ")))
          (with-current-buffer buffer
            (write-region (point-min) (point-max) file))
          (message "org-learn: plan saved to %s" file)))
      buffer)))

(provide 'org-learn-plan)
;;; org-learn-plan.el ends here
