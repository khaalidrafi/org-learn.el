;;; org-learn-roadmap.el --- Roadmap bridge: org-learn over org-roadmap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Khalid

;; Author: Khalid
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.0") (org-roadmap "0.1"))
;; Keywords: org, learning, roadmap
;; URL: https://github.com/khaalidrafi/org-learn.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Spec 006 (specs/006-roadmap-bridge.org): org-learn DEPENDS on
;; org-roadmap for planning and progress (Owner order 2026-11-01
;; 18:28Z: "org-learn should depend on org-roadmap for planning,
;; progress etc.").  The old declared-never-required boundary law is
;; superseded.
;;
;; - `org-learn-plan-from-roadmap' reads the roadmap TODO tree of the
;;   current buffer through org-roadmap's public API and ensures one
;;   :drill: review item per topic heading in `org-learn-file' (same
;;   title, :LEARN_SOURCE: roadmap, LEARN_* properties reset).  It is
;;   idempotent: existing titles are never duplicated.
;; - `org-learn-progress-report' summarizes the review items per
;;   topic (due/done, average ease, retention), writes the
;;   :LEARN_PROGRESS: percent property onto every roadmap node and
;;   renders the ASCII roadmap via `org-roadmap-generate' into the
;;   read-only `org-learn-roadmap-progress-buffer' (org-mode).
;; - `org-learn-roadmap-import' is a thin pass-through to
;;   `org-roadmap-import-roadmap-sh' (roadmap.sh -> Org TODO tree)
;;   and then offers `org-learn-plan-from-roadmap'.  No roadmap logic
;;   is re-implemented here.
;;
;; org-roadmap is required lazily INSIDE the commands (`require'
;; with nil t): the org-learn core keeps loading under CI -Q, and a
;; missing feature turns into a `user-error' with a one-line install
;; hint.  The commands are autoloaded from org-learn.el.

;;; Code:

(require 'org)
(require 'seq)
(require 'org-learn)
(require 'org-learn-dashboard)

(defgroup org-learn-roadmap nil
  "Roadmap bridge between org-learn and org-roadmap."
  :group 'org-learn
  :prefix "org-learn-")

(defcustom org-learn-roadmap-topic-level 2
  "Heading level that `org-learn-plan-from-roadmap' treats as topics."
  :type 'integer
  :group 'org-learn-roadmap)

(defconst org-learn-roadmap-progress-buffer "*org-learn-progress*"
  "Name of the read-only progress report buffer.")

(defconst org-learn-roadmap--render-buffer "*Org Roadmap*"
  "Buffer name `org-roadmap-generate' renders into (org-roadmap.el).")

(defconst org-learn-roadmap--install-hint
  "install org-roadmap first (M-x package-install RET org-roadmap)"
  "One-line hint `user-error' shows when org-roadmap is missing.")

;; The bridge calls org-roadmap lazily inside its commands, so the
;; byte-compiler never sees the definitions; declare them to keep the
;; gate warning-free (spec 006: org-roadmap is never required at load
;; time).
(declare-function org-roadmap-get-todo-tree "org-roadmap")
(declare-function org-roadmap-get-subtree-tree "org-roadmap")
(declare-function org-roadmap-generate "org-roadmap")
(declare-function org-roadmap-import-roadmap-sh "org-roadmap")

;;; org-roadmap plumbing (lazy dependency, spec 006).

(defun org-learn-roadmap--require ()
  "Load `org-roadmap' or `user-error' with the install hint.
The require happens inside the commands only: the org-learn core
loads without org-roadmap present (dependency law, spec 006)."
  (require 'org-roadmap nil t)
  (unless (featurep 'org-roadmap)
    (user-error "The org-roadmap feature is required for planning and progress; %s"
                org-learn-roadmap--install-hint)))

(defun org-learn-roadmap--flatten (tree)
  "Return every node of the org-roadmap TREE in document order."
  (apply #'append
         (mapcar (lambda (node)
                   (cons node (org-learn-roadmap--flatten
                               (plist-get node :children))))
                 tree)))

(defun org-learn-roadmap--topics-under (node level)
  "Return the topic nodes at LEVEL under NODE (inclusive)."
  (seq-filter (lambda (candidate)
                (= (plist-get candidate :level) level))
              (cons node (org-learn-roadmap--flatten
                          (plist-get node :children)))))

;;; Plan review items from the roadmap.

(defun org-learn-roadmap--existing-titles (file)
  "Return the heading titles present in the Org FILE.
The file is read without visiting it; a missing FILE yields nil."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((org-inhibit-startup t))
        (delay-mode-hooks (org-mode)))
      (org-map-entries (lambda () (org-get-heading t t t t)) nil nil))))

(defun org-learn-roadmap--review-entry (title)
  "Return the Org text of the roadmap review item titled TITLE.
The item carries the :drill: tag, :LEARN_SOURCE: roadmap and the
LEARN_* properties in their reset state."
  (concat "* TODO " title " :drill:"
          "\n  :PROPERTIES:"
          "\n  :CREATED: " (org-learn--iso8601-now)
          "\n  :LEARN_SOURCE: roadmap"
          "\n  :LEARN_EASE: 2.5"
          "\n  :LEARN_INTERVAL: 1"
          "\n  :LEARN_REPS: 0"
          "\n  :LEARN_LAPSES: 0"
          "\n  :END:\n"))

;; The autoload for this command lives in org-learn.el (spec 006).
(defun org-learn-plan-from-roadmap ()
  "Ensure a review item per topic heading of the roadmap in this buffer.
Reads the roadmap TODO tree through org-roadmap (spec 006).  Every
heading at `org-learn-roadmap-topic-level' gets a matching review
item in `org-learn-file': same title, :drill: tag, :LEARN_SOURCE:
roadmap, LEARN_* properties reset.  Existing titles are never
duplicated (idempotent).  Reports the number of items added."
  (interactive)
  (org-learn-roadmap--require)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer; visit the roadmap file first"))
  (let* ((tree (org-roadmap-get-todo-tree))
         (topics (apply #'append
                        (mapcar (lambda (node)
                                  (org-learn-roadmap--topics-under
                                   node org-learn-roadmap-topic-level))
                                tree)))
         (existing (org-learn-roadmap--existing-titles org-learn-file))
         (added 0))
    (unless topics
      (user-error "No roadmap topics at level %d; check %s"
                  org-learn-roadmap-topic-level
                  "org-learn-roadmap-topic-level"))
    (dolist (topic topics)
      (let ((title (plist-get topic :headline)))
        (unless (member title existing)
          (org-learn--append-entry (org-learn-roadmap--review-entry title))
          (setq existing (cons title existing))
          (setq added (1+ added)))))
    (message "org-learn: %d review item%s added from roadmap"
             added (if (= added 1) "" "s"))
    added))

;;; Progress report.

(defun org-learn-roadmap--scan-review-items ()
  "Return the review-item stats of `org-learn-file' as an alist.
Key is the heading title, value a plist with :total :due :done
:eases :reps :lapses aggregated over every heading of that title.
The file is read without visiting it; a missing file yields nil."
  (when (and org-learn-file (file-exists-p org-learn-file))
    (let ((table nil))
      (with-temp-buffer
        (insert-file-contents (expand-file-name org-learn-file))
        (let ((org-inhibit-startup t))
          (delay-mode-hooks (org-mode)))
        (org-map-entries
         (lambda ()
           (let* ((title (org-get-heading t t t t))
                  (scheduled (org-entry-get (point) "SCHEDULED"))
                  (due-p (and scheduled
                              (<= (time-to-days
                                   (org-time-string-to-time scheduled))
                                  (org-today))))
                  (ease-string (org-entry-get (point) "LEARN_EASE"))
                  (old (assoc-default title table))
                  (stats (or old (list :total 0 :due 0 :done 0
                                       :eases nil :reps 0 :lapses 0)))
                  (stats (plist-put stats :total
                                    (1+ (plist-get stats :total))))
                  (stats (plist-put stats :due
                                    (+ (plist-get stats :due)
                                       (if due-p 1 0))))
                  (stats (plist-put stats :done
                                    (+ (plist-get stats :done)
                                       (if (equal (org-entry-get
                                                   (point) "TODO") "DONE")
                                           1 0))))
                  (stats (plist-put stats :eases
                                    (if ease-string
                                        (cons (string-to-number
                                               ease-string)
                                              (plist-get stats :eases))
                                      (plist-get stats :eases))))
                  (stats (plist-put stats :reps
                                    (+ (plist-get stats :reps)
                                       (string-to-number
                                        (or (org-entry-get
                                             (point) "LEARN_REPS") "0")))))
                  (stats (plist-put stats :lapses
                                    (+ (plist-get stats :lapses)
                                       (string-to-number
                                        (or (org-entry-get
                                             (point) "LEARN_LAPSES")
                                            "0"))))))
             (unless old
               (push (cons title stats) table))))
         nil nil))
      table)))

(defun org-learn-roadmap--percent (done total)
  "Return DONE of TOTAL as a rounded percent (0 when TOTAL is zero)."
  (if (zerop total)
      0
    (round (* 100.0 (/ (float done) total)))))

(defun org-learn-roadmap--node-percent (node stats)
  "Return the percent for NODE's topics from the STATS alist."
  (let ((total 0)
        (done 0))
    (dolist (topic (org-learn-roadmap--topics-under
                    node org-learn-roadmap-topic-level))
      (let ((s (assoc-default (plist-get topic :headline) stats)))
        (when s
          (setq total (+ total (plist-get s :total))
                done (+ done (plist-get s :done))))))
    (org-learn-roadmap--percent done total)))

(defun org-learn-roadmap--write-progress (tree stats)
  "Write the :LEARN_PROGRESS: percent onto every node of TREE.
The percent is computed from the STATS alist over each node's
topics."
  (dolist (node (org-learn-roadmap--flatten tree))
    (let ((loc (plist-get node :location)))
      (when (and (markerp loc) (buffer-live-p (marker-buffer loc)))
        (org-entry-put
         loc "LEARN_PROGRESS"
         (number-to-string (org-learn-roadmap--node-percent node stats)))))))

(defun org-learn-roadmap--topic-line (topic stats)
  "Return one summary line for TOPIC from the STATS alist."
  (let* ((title (plist-get topic :headline))
         (s (assoc-default title stats)))
    (if (not s)
        (format "- *%s* - no review items yet (plan first)\n" title)
      (let ((eases (plist-get s :eases))
            (retention (org-learn-dashboard-retention
                        (plist-get s :lapses) (plist-get s :reps))))
        (format "- *%s* - due %d, done %d/%d, avg ease %s, retention %s\n"
                title
                (plist-get s :due)
                (plist-get s :done)
                (plist-get s :total)
                (if eases
                    (format "%.1f" (/ (apply #'+ eases) (float (length eases))))
                  "n/a")
                (if retention (format "%d%%" retention) "n/a"))))))

(defun org-learn-roadmap--generated-roadmap (roadmap-buffer)
  "Return the ASCII roadmap text rendered by `org-roadmap-generate'.
The public org-roadmap renderer fills
`org-learn-roadmap--render-buffer' for the subtree at point in
ROADMAP-BUFFER; the text is copied from there, so no rendering is
re-implemented (spec 006 API law)."
  (with-current-buffer roadmap-buffer
    (save-window-excursion
      (let ((pre-existing (get-buffer org-learn-roadmap--render-buffer)))
        (org-roadmap-generate t)
        (let ((source (get-buffer org-learn-roadmap--render-buffer)))
          (prog1
              (if source
                  (with-current-buffer source (buffer-string))
                "")
            (unless pre-existing
              (when source (kill-buffer source)))))))))

(defun org-learn-roadmap--render-report (tree stats roadmap-buffer)
  "Render the progress report into `org-learn-roadmap-progress-buffer'.
TREE is the roadmap tree, STATS the review-item alist and
ROADMAP-BUFFER the buffer the ASCII roadmap is rendered from; the
buffer lists one summary line per topic and the ASCII roadmap."
  (let ((progress (get-buffer-create org-learn-roadmap-progress-buffer))
        (topics (apply #'append
                       (mapcar (lambda (node)
                                 (org-learn-roadmap--topics-under
                                  node org-learn-roadmap-topic-level))
                               tree)))
        (roadmap (org-learn-roadmap--generated-roadmap roadmap-buffer)))
    (with-current-buffer progress
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: org-learn roadmap progress\n\n")
        (dolist (topic topics)
          (insert (org-learn-roadmap--topic-line topic stats)))
        (insert "\n")
        (insert roadmap)
        (goto-char (point-min))
        (setq buffer-read-only t)))
    (display-buffer progress)
    progress))

;; The autoload for this command lives in org-learn.el (spec 006).
(defun org-learn-progress-report ()
  "Report review progress for the roadmap subtree at point.
For every roadmap node the matching review items in `org-learn-file'
are summarized (due/done, average ease, retention) and the
:LEARN_PROGRESS: percent property is written onto the node.  The
ASCII roadmap is then rendered through org-roadmap into the
read-only `org-learn-roadmap-progress-buffer' (org-mode)."
  (interactive)
  (org-learn-roadmap--require)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer; visit the roadmap file first"))
  (unless (org-at-heading-p)
    (user-error "No roadmap heading at point; put point on the tree root"))
  (let* ((roadmap-buffer (current-buffer))
         (tree (org-roadmap-get-subtree-tree))
         (stats (org-learn-roadmap--scan-review-items))
         (topics (apply #'append
                        (mapcar (lambda (node)
                                  (org-learn-roadmap--topics-under
                                   node org-learn-roadmap-topic-level))
                                tree))))
    (org-learn-roadmap--write-progress tree stats)
    (org-learn-roadmap--render-report tree stats roadmap-buffer)
    (message "org-learn: progress written for %d topic%s"
             (length topics) (if (= (length topics) 1) "" "s"))))

;;; roadmap.sh import pass-through.

;; The autoload for this command lives in org-learn.el (spec 006).
(defun org-learn-roadmap-import (source &optional output-file)
  "Import a roadmap.sh SOURCE as an Org TODO tree, then offer planning.
SOURCE and OUTPUT-FILE pass straight through to
`org-roadmap-import-roadmap-sh'; no import logic lives here.  A nil
OUTPUT-FILE inserts the tree into the current buffer.  After the
import, offer `org-learn-plan-from-roadmap' on the imported tree."
  (interactive
   (list (read-string "Roadmap.sh URL or JSON file: ")
         (unless current-prefix-arg
           (read-file-name "Export Org to file: "))))
  (org-learn-roadmap--require)
  (org-roadmap-import-roadmap-sh source output-file)
  (when (y-or-n-p "Plan review items from the imported roadmap? ")
    (when (and output-file (not (derived-mode-p 'org-mode)))
      (find-file output-file))
    (org-learn-plan-from-roadmap)))

(provide 'org-learn-roadmap)
;;; org-learn-roadmap.el ends here
