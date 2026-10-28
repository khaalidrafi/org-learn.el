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

(defgroup org-learn nil
  "Org-based learning helper."
  :group 'org
  :prefix "org-learn-")

(defcustom org-learn-file "learn.org"
  "Default Org file where org-learn keeps learning notes."
  :type 'file
  :group 'org-learn)

;;;###autoload
(defun org-learn-open ()
  "Open the org-learn notes file."
  (interactive)
  (find-file org-learn-file))

(provide 'org-learn)
;;; org-learn.el ends here
