;;; org-learn.el --- Org-based learning helper -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Khalid

;; Author: Khalid
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.0") (org-roadmap "0.1"))
;; Keywords: org, learning
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

;; org-learn.el is an Org-based learning helper: learning material lives
;; in Org files and the package helps you capture, review and grow it.
;;
;; STATUS: bootstrap skeleton (2026-10-28).  The team layer is being
;; formed on the agent-team kit -- see BOOTSTRAP-PROMPT.org at the repo
;; root and agent-team/ once the teamify lands.
;;
;; DEPENDENCY: org-roadmap (a package under the khaalidrafi profile) is
;; planned but NOT completed or published yet.  It is declared in
;; Package-Requires as intent (version placeholder "0.1"); this skeleton
;; does NOT (require 'org-roadmap) -- keep the boundary soft until its
;; API lands.  Do not block on it.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'thingatpt)

(defgroup org-learn nil
  "Org-based learning helper."
  :group 'org
  :prefix "org-learn-")

(defcustom org-learn-file "learn.org"
  "Default Org file where org-learn keeps learning notes."
  :type 'file
  :group 'org-learn)

(defconst org-learn--title-max 60
  "Maximum capture title length before an ellipsis is appended.")

(defconst org-learn--file-header
  "#+TITLE: Learning notes\n\n"
  "Minimal Org header written when the notes file is created.")

;;;###autoload
(defun org-learn-open ()
  "Open the org-learn notes file."
  (interactive)
  (find-file org-learn-file))

;;; Spec 001 -- capture (specs/001-capture.org).

(defun org-learn--truncate-title (title)
  "Truncate TITLE to `org-learn--title-max' with an ellipsis.
TITLE is trimmed of surrounding whitespace; nil becomes the empty
string.  Long titles lose their tail and end with \"...\"."
  (let ((title (string-trim (or title ""))))
    (if (<= (length title) org-learn--title-max)
        title
      (concat (substring title 0 (- org-learn--title-max 3)) "..."))))

(defun org-learn--default-title ()
  "Return the default title for a new capture.
The active region wins over the thing at point; the result is
collapsed to a single line and truncated (see
`org-learn--truncate-title')."
  (let ((raw (if (use-region-p)
                 (buffer-substring-no-properties
                  (region-beginning) (region-end))
               (thing-at-point 'symbol t))))
    (org-learn--truncate-title
     (and raw (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " raw))))))

(defun org-learn--iso8601-now ()
  "Return the current time as a sorting-safe ISO-8601 timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%S%:z"))

(defun org-learn--format-entry (title &optional body)
  "Return the Org text for a TODO heading TITLE with optional BODY.
BODY must already be indented as heading body text, without a
trailing newline; a CREATED property drawer is always included."
  (concat "* TODO " title
          "\n  :PROPERTIES:\n  :CREATED: " (org-learn--iso8601-now)
          "\n  :END:\n"
          (if (and body (not (string= body ""))) (concat body "\n") "")))

(defun org-learn--indent-block (text)
  "Return TEXT with every non-blank line indented two spaces.
Leading and trailing blank lines are removed, so the result fits
under a heading as its body."
  (with-temp-buffer
    (insert (string-trim text))
    (goto-char (point-min))
    (while (not (eobp))
      (unless (looking-at-p "[ \t]*$")
        (insert "  "))
      (forward-line 1))
    (string-trim-right (buffer-string))))

(defun org-learn--append-entry (entry)
  "Append ENTRY text to `org-learn-file'.
When the file is missing or empty it is created with the minimal
header from `org-learn--file-header', written exactly once.  The
file is never visited: the work happens in a temp buffer, so no
orphan buffer is left behind."
  (let ((file (expand-file-name org-learn-file))
        (exists (file-exists-p org-learn-file)))
    (with-temp-buffer
      (when exists
        (insert-file-contents file))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (when (or (not exists) (bobp))
        (insert org-learn--file-header))
      (insert entry)
      (write-region (point-min) (point-max) file nil 'quiet))))

;;;###autoload
(defun org-learn-capture (&optional title)
  "Capture a learning note titled TITLE into `org-learn-file'.
Appends a TODO heading with a CREATED property; the notes file is
created with a minimal header when missing and the header is
never duplicated.  Interactively, TITLE is prompted for and
defaults to the region or the thing at point, truncated."
  (interactive
   (list (read-string "Learning note title: " nil nil
                      (org-learn--default-title))))
  (let ((title (string-trim (or title (org-learn--default-title) ""))))
    (when (string= title "")
      (setq title "Untitled"))
    (org-learn--append-entry
     (org-learn--format-entry (org-learn--truncate-title title)))))

;;;###autoload
(defun org-learn-capture-from-region (beg end &optional title)
  "Capture the region BEG..END as the body of a new learning note.
The region text is indented as the heading body below the TODO
heading.  Interactively this requires an active region and prompts
for TITLE (default: the region text, truncated)."
  (interactive
   (progn
     (unless (use-region-p)
       (user-error "No active region; mark the text to capture first"))
     (list (region-beginning) (region-end)
           (read-string "Learning note title: " nil nil
                        (org-learn--default-title)))))
  (let ((title (string-trim (or title (org-learn--default-title) ""))))
    (when (string= title "")
      (setq title "Untitled"))
    (org-learn--append-entry
     (org-learn--format-entry (org-learn--truncate-title title)
                              (org-learn--indent-block
                               (buffer-substring-no-properties beg end))))))

(provide 'org-learn)
;;; org-learn.el ends here
