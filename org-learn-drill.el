;;; org-learn-drill.el --- Drill sessions for org-learn -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Khalid

;; Author: Khalid
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.0"))
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

;; Spec 003 (specs/003-drill-sessions.org): interactive drill sessions
;; in a dedicated buffer -- cards hidden until revealed, graded 0-5,
;; scheduled by the spec 002 review core.
;;
;; - A card is a heading tagged `org-learn-drill-tag'.  The card type
;;   comes from the DRILL_CARD_TYPE property, default "simple":
;;   simple shows the body and hides child subheadings until reveal,
;;   cloze masks [answer||hint] spans in the body (hints stay visible
;;   while masked), and twosided shows the first child heading and
;;   hides the second until reveal (further children are notes).
;; - `org-learn-drill--cards' enumerates the cards per scope with the
;;   same semantics as `org-learn-review-scope'; empty cards are
;;   skipped.  `org-learn-drill' runs due/new cards,
;;   `org-learn-drill-cram' ignores the schedule but skips cards
;;   reviewed within `org-learn-drill-cram-hours'.
;; - Grading delegates to `org-learn-review--process-entry' (single
;;   source of scheduling truth).  Lapses past
;;   `org-learn-drill-leech-threshold' tag the entry :leech:
;;   (idempotent); `org-learn-drill-leech-method' warns about or skips
;;   such leeches during sessions.
;; - Masking happens via overlays on a COPY of the content in the
;;   *org-learn-drill* buffer: the source file is never modified by
;;   presentation.
;;
;; org-roadmap stays at the boundary: it is never required here
;; (AGENTS.md).

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)
(require 'org-learn)
(require 'org-learn-review)

(defgroup org-learn-drill nil
  "Drill sessions for org-learn."
  :group 'org-learn
  :prefix "org-learn-")

(defcustom org-learn-drill-tag "drill"
  "Heading tag that marks an entry as a drill card."
  :type 'string
  :group 'org-learn-drill)

(defcustom org-learn-drill-cram-hours 12
  "Cram sessions skip cards reviewed within this many hours."
  :type 'natnum
  :group 'org-learn-drill)

(defcustom org-learn-drill-max-items 30
  "Maximum cards per session, or nil for no limit."
  :type '(choice (const :tag "Unlimited" nil) (natnum :tag "Limit"))
  :group 'org-learn-drill)

(defcustom org-learn-drill-leech-threshold 15
  "Lapses at which a card is tagged :leech:."
  :type 'natnum
  :group 'org-learn-drill)

(defcustom org-learn-drill-leech-method 'warn
  "How sessions treat leech cards.
`warn' shows a warning line when a leech is presented, `skip'
leaves leeches out of sessions entirely, nil ignores them."
  :type '(choice (const :tag "Warn" warn)
                 (const :tag "Skip" skip)
                 (const :tag "Off" nil))
  :group 'org-learn-drill)

(defconst org-learn-drill--buffer "*org-learn-drill*"
  "Name of the dedicated drill session buffer.")

(defvar-local org-learn-drill--state nil
  "Buffer-local session state plist of the drill buffer.
Keys: :index :total :mode :revealed :graded :skipped.")

;;; Cloze parsing (pure, spec 003 acceptance).

(defun org-learn-drill--parse-cloze (body)
  "Split cloze BODY into segments.
Returns a list of plists: (:type text :text STRING) for literal
text and (:type cloze :answer STRING :hint STRING-or-nil) for each
[answer] or [answer||hint] span.  Empty literal segments are
dropped, so answers and hints are extracted exactly."
  (let ((pos 0)
        (segments nil))
    (while (string-match "\\[\\([^][]+\\)\\]" body pos)
      (let* ((before (substring body pos (match-beginning 0)))
             (raw (match-string 1 body))
             (parts (string-split raw "||" t)))
        (unless (string-empty-p before)
          (push (list :type 'text :text before) segments))
        (push (list :type 'cloze
                    :answer (nth 0 parts)
                    :hint (nth 1 parts))
              segments)
        (setq pos (match-end 0))))
    (let ((rest (substring body pos)))
      (unless (string-empty-p rest)
        (push (list :type 'text :text rest) segments)))
    (nreverse segments)))

;;; Card model.

(defun org-learn-drill--body-text (limit)
  "Return the card body between the heading at point and LIMIT.
Property drawers, planning lines and edge blank lines are
excluded; the lines keep their source indentation."
  (save-excursion
    (forward-line 1)
    (let ((in-drawer nil)
          lines)
      (while (< (point) limit)
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match-p "\\`[ \t]*:[A-Za-z][A-Za-z0-9_-]*:[ \t]*\\'" line)
            (setq in-drawer (not in-drawer)))
           ((or in-drawer
                (string-match-p "\\`\\(SCHEDULED\\|DEADLINE\\|CLOSED\\|CLOCK\\):"
                                line)))
           (t (push line lines))))
        (forward-line 1))
      (setq lines (nreverse lines))
      (while (and lines (string-blank-p (car lines)))
        (setq lines (cdr lines)))
      (setq lines (nreverse lines))
      (while (and lines (string-blank-p (car lines)))
        (setq lines (cdr lines)))
      (string-join (nreverse lines) "\n"))))

(defun org-learn-drill--child-sides (pos)
  "Return (SIDE1 SIDE2) for the twosided card at POS.
Each side is (TITLE . BODY) of the first two child headings;
further children are notes, not sides."
  (save-match-data
    (org-with-point-at pos
      (when (org-goto-first-child)
        (let ((sides nil)
              (done nil))
          (while (and (not done) (< (length sides) 2))
            (push (cons (org-get-heading t t t t)
                        (org-learn-drill--body-text
                         (org-entry-end-position)))
                  sides)
            (unless (org-get-next-sibling)
              (setq done t)))
          (nreverse sides))))))

(defun org-learn-drill--card-at (pos)
  "Return the card plist for the tagged entry at POS, or nil.
Nil when the entry is an empty card (no body and no children).
The plist carries :marker :title :tags :type :scheduled :lapses
:body :children :sides :notes :level :heading-line."
  (org-with-point-at pos
    (let* ((title (org-get-heading t t t t))
           (tags (org-get-tags (point) t))
           (type (or (org-entry-get (point) "DRILL_CARD_TYPE") "simple"))
           (scheduled (org-entry-get (point) "SCHEDULED"))
           (lapses (string-to-number
                    (or (org-entry-get (point) "LEARN_LAPSES") "0")))
           (level (org-current-level))
           (end (org-end-of-subtree))
           (children-beg (save-excursion
                           (and (org-goto-first-child) (point))))
           (body (org-learn-drill--body-text
                  (if children-beg children-beg end)))
           (sides (when (string= type "twosided")
                    (org-learn-drill--child-sides pos)))
           (notes
            (when children-beg
              (if (and (string= type "twosided") sides)
                  (save-excursion
                    (org-goto-first-child)
                    (when (and (org-get-next-sibling)
                               (org-get-next-sibling))
                      (buffer-substring-no-properties (point) end)))
                (buffer-substring-no-properties children-beg end))))
           (heading-line
            (save-excursion
              (buffer-substring-no-properties
               (line-beginning-position)
               (progn (forward-line 1) (point))))))
      (when (or (not (string-blank-p body)) children-beg)
        (list :marker (copy-marker pos)
              :title title
              :tags tags
              :type type
              :scheduled scheduled
              :lapses lapses
              :body body
              :children (when children-beg
                          (buffer-substring-no-properties children-beg end))
              :sides sides
              :notes notes
              :level level
              :heading-line heading-line)))))

(defun org-learn-drill--scan-buffer (buffer)
  "Return the drill cards in BUFFER, in file order.
Cards are the tagged headings per `org-learn-drill--card-at';
empty cards are skipped.  The buffer stays alive: card markers
point into it."
  (with-current-buffer buffer
    (org-with-point-at (point-min)
      (let (cards)
        (org-map-entries
         (lambda ()
           (when (member org-learn-drill-tag (org-get-tags (point) t))
             (let ((card (org-learn-drill--card-at (point))))
               (when card
                 (push card cards)))))
         nil 'file)
        (nreverse cards)))))

(defun org-learn-drill--cards (&optional scope)
  "Return the drill cards in SCOPE, in file order.
SCOPE uses the `org-learn-review-scope' semantics (file, agenda,
directory) and defaults to `org-learn-review-scope'.  Source files
are visited with `find-file-noselect' and left open, so the card
markers stay alive; the session kills the ones it opened."
  (let* ((org-learn-review-scope (or scope org-learn-review-scope))
         (files (seq-filter #'file-exists-p
                            (org-learn-review--scoped-files)))
         cards)
    (dolist (file files)
      (setq cards
            (append cards
                    (org-learn-drill--scan-buffer
                     (find-file-noselect file)))))
    cards))

;;; Selection (due/new ordering, cram window, leeches, limits).

(defun org-learn-drill--select (cards &optional include-future)
  "Order CARDS for a session: due oldest SCHEDULED first, then the rest.
The rest (new or, with INCLUDE-FUTURE, future-scheduled cards)
keeps file order.  Without INCLUDE-FUTURE, future-scheduled cards
are dropped."
  (let (due rest)
    (dolist (card cards)
      (let ((scheduled (plist-get card :scheduled)))
        (cond
         ((and scheduled
               (<= (time-to-days (org-time-string-to-time scheduled))
                   (org-today)))
          (push card due))
         ((or include-future (not scheduled))
          (push card rest)))))
    (setq due (sort due
                    (lambda (a b)
                      (time-less-p
                       (org-time-string-to-time (plist-get a :scheduled))
                       (org-time-string-to-time (plist-get b :scheduled))))))
    (append due (nreverse rest))))

(defun org-learn-drill--last-review-time (marker)
  "Return the time of the entry's last LOGBOOK review note, or nil.
MARKER points at the entry heading."
  (save-match-data
    (org-with-point-at marker
      (let ((end (org-entry-end-position))
            (last nil))
        (save-excursion
          (while (re-search-forward
                  "- Note taken on \\(\\[[^]]+\\]\\)" end t)
            (setq last (match-string 1))))
        (when last
          (org-time-string-to-time last))))))

(defun org-learn-drill--cram-filter (cards)
  "Drop CARDS reviewed within `org-learn-drill-cram-hours'.
Cramming ignores the schedule, but a card reviewed inside the cram
window does not come back."
  (let ((cutoff (time-subtract (current-time)
                               (seconds-to-time
                                (* 3600 org-learn-drill-cram-hours)))))
    (seq-filter
     (lambda (card)
       (let ((last (org-learn-drill--last-review-time
                    (plist-get card :marker))))
         (or (not last) (time-less-p last cutoff))))
     cards)))

(defun org-learn-drill--skip-leeches (cards)
  "Drop leech-tagged cards from CARDS per `org-learn-drill-leech-method'."
  (if (eq org-learn-drill-leech-method 'skip)
      (seq-filter (lambda (card)
                    (not (member "leech" (plist-get card :tags))))
                  cards)
    cards))

(defun org-learn-drill--limit (cards)
  "Apply the `org-learn-drill-max-items' session limit to CARDS."
  (if org-learn-drill-max-items
      (seq-take cards org-learn-drill-max-items)
    cards))

;;; Leech law.

(defun org-learn-drill--apply-leech-law (marker)
  "Tag the entry at MARKER :leech: when the lapse threshold is hit.
Idempotent: the tag is only ever switched on."
  (org-with-point-at marker
    (let ((lapses (string-to-number
                   (or (org-entry-get (point) "LEARN_LAPSES") "0"))))
      (when (>= lapses org-learn-drill-leech-threshold)
        (org-toggle-tag "leech" 'on)))))

;;; Presentation (a copy; the source is never modified).

(defun org-learn-drill--insert-cloze-body (body)
  "Insert the cloze-masked BODY into the drill buffer.
Answers inside [answer] or [answer||hint] spans are covered by a
display overlay showing an ellipsis; hints stay visible while
masked.  The body always ends with a newline."
  (dolist (segment (org-learn-drill--parse-cloze body))
    (if (eq (plist-get segment :type) 'text)
        (insert (plist-get segment :text))
      (insert "[")
      (let ((beg (point)))
        (insert (plist-get segment :answer))
        (let ((ov (make-overlay beg (point))))
          (overlay-put ov 'display "…")
          (overlay-put ov 'org-learn-drill-mask t)))
      (when (plist-get segment :hint)
        (insert (format " (%s)" (plist-get segment :hint))))
      (insert "]")))
  (insert "\n"))

(defun org-learn-drill--hide-region (beg end)
  "Hide the region BEG..END behind a mask overlay."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'invisible 'org-learn-drill-mask)
    (overlay-put ov 'org-learn-drill-mask t)))

(defun org-learn-drill--reveal ()
  "Reveal every masked region in the drill buffer."
  (with-current-buffer org-learn-drill--buffer
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'org-learn-drill-mask)
        (delete-overlay ov)))
    (setq org-learn-drill--state
          (plist-put org-learn-drill--state :revealed t))))

(defun org-learn-drill--render (card)
  "Render CARD (a copy) into the drill buffer and mask the hidden parts."
  (let* ((inhibit-read-only t)
         (type (plist-get card :type))
         (leech-p (member "leech" (plist-get card :tags)))
         (child-stars (make-string (1+ (plist-get card :level)) ?*)))
    (erase-buffer)
    (insert (plist-get card :heading-line))
    (when (and (eq org-learn-drill-leech-method 'warn) leech-p)
      (insert (format "/Leech warning: %d lapses./\n"
                      (plist-get card :lapses))))
    (cond
     ((string= type "twosided")
      (unless (string-empty-p (plist-get card :body))
        (insert (plist-get card :body) "\n"))
      (let ((sides (plist-get card :sides)))
        (when sides
          (insert (format "%s %s\n" child-stars (car (nth 0 sides))))
          (insert (cdr (nth 0 sides)) "\n")
          (when (nth 1 sides)
            (let ((beg (point)))
              (insert (format "%s %s\n" child-stars (car (nth 1 sides))))
              (insert (cdr (nth 1 sides)) "\n")
              (org-learn-drill--hide-region beg (point))))))
      (when (plist-get card :notes)
        (insert (plist-get card :notes))))
     ((string= type "cloze")
      (org-learn-drill--insert-cloze-body (plist-get card :body))
      (when (plist-get card :children)
        (insert (plist-get card :children))))
     (t
      (unless (string-empty-p (plist-get card :body))
        (insert (plist-get card :body) "\n"))
      (when (plist-get card :children)
        (let ((beg (point)))
          (insert (plist-get card :children))
          (org-learn-drill--hide-region beg (point))))))
    (insert "\nGrade 0-5 (0 blackout ... 5 perfect) · SPC reveal · s skip · e source · q quit\n")
    (goto-char (point-min))
    (setq buffer-read-only t)
    (setq header-line-format
          (format "org-learn drill: %d/%d · %s · scope %s"
                  (1+ (plist-get org-learn-drill--state :index))
                  (plist-get org-learn-drill--state :total)
                  (plist-get org-learn-drill--state :mode)
                  org-learn-review-scope))))

;;; Session.

(defun org-learn-drill--grade (card quality)
  "Grade CARD with QUALITY through the review core.
Also applies the leech law and saves the source file."
  (let ((marker (plist-get card :marker)))
    (with-current-buffer (marker-buffer marker)
      (save-match-data
        (goto-char marker)
        (org-learn-review--process-entry quality)
        (org-learn-drill--apply-leech-law marker)
        (save-buffer)))))

(defun org-learn-drill--pop-source (card)
  "Pop to CARD's source entry for editing."
  (save-match-data
    (pop-to-buffer (marker-buffer (plist-get card :marker)))
    (goto-char (plist-get card :marker))))

(defun org-learn-drill--cleanup-sources (buffers pre-existing)
  "Save modified source BUFFERS and kill the session-opened ones."
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (when (and (buffer-modified-p buffer) (buffer-file-name buffer))
        (with-current-buffer buffer
          (save-buffer)))
      (unless (memq buffer pre-existing)
        (kill-buffer buffer)))))

(defun org-learn-drill--run (cards mode)
  "Run a drill session over CARDS in MODE (drill or cram).
One card at a time in the dedicated buffer; keys: SPC reveal, 0-5
grade, s skip, e edit source, q quit (graded cards stay graded)."
  (let* ((total (length cards))
         (drill-buffer (get-buffer-create org-learn-drill--buffer))
         (graded 0)
         (skipped 0))
    (with-current-buffer drill-buffer
      (unless (derived-mode-p 'org-learn-drill-mode)
        (org-learn-drill-mode))
      (setq org-learn-drill--state
            (list :index 0 :total total :mode mode :revealed nil)))
    (pop-to-buffer drill-buffer)
    (unwind-protect
        (catch 'org-learn-drill-quit
          (let ((i 0))
            (while (< i total)
              (let* ((card (nth i cards))
                     (done nil))
                (with-current-buffer drill-buffer
                  (setq org-learn-drill--state
                        (plist-put org-learn-drill--state :index i))
                  (org-learn-drill--render card))
                (while (not done)
                  (message "SPC reveal · 0-5 grade · s skip · e source · q quit")
                  (let ((key (read-char)))
                    (pcase key
                      (?\s (org-learn-drill--reveal))
                      ((or ?0 ?1 ?2 ?3 ?4 ?5)
                       (org-learn-drill--grade card (- key ?0))
                       (setq graded (1+ graded) done t))
                      (?s (setq skipped (1+ skipped) done t))
                      (?e (org-learn-drill--pop-source card))
                      (?q (throw 'org-learn-drill-quit t))
                      (_ (message "Unknown key %c" key)
                         (sit-for 1)))))
                (setq i (1+ i)))))
          (message "org-learn-drill: session done (%d graded, %d skipped)."
                   graded skipped))
      (with-current-buffer drill-buffer
        (setq org-learn-drill--state
              (plist-put org-learn-drill--state :graded graded))
        (setq org-learn-drill--state
              (plist-put org-learn-drill--state :skipped skipped))
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (insert (format "\nSession ended: %d graded, %d skipped.\n"
                            graded skipped)))))
      (dolist (card cards)
        (let ((marker (plist-get card :marker)))
          (when (and (markerp marker) (marker-buffer marker))
            (set-marker marker nil)))))))

(defun org-learn-drill--session (mode &optional scope)
  "Select cards per SCOPE and run a MODE session (drill or cram).
Source files opened for the session are saved when modified and
killed when they were not open before."
  (let* ((org-learn-review-scope (or scope org-learn-review-scope))
         (files (seq-filter #'file-exists-p
                            (org-learn-review--scoped-files)))
         (pre-existing (mapcar #'get-file-buffer files))
         (cards (apply #'append
                       (mapcar #'org-learn-drill--scan-buffer
                               (mapcar #'find-file-noselect files))))
         (cards (if (eq mode 'cram)
                    (org-learn-drill--cram-filter cards)
                  (org-learn-drill--select cards)))
         (cards (org-learn-drill--skip-leeches cards))
         (cards (org-learn-drill--limit cards)))
    (if (null cards)
        (message "org-learn-drill: nothing to %s in scope %s."
                 (if (eq mode 'cram) "cram" "drill")
                 org-learn-review-scope)
      (unwind-protect
          (org-learn-drill--run cards mode)
        (org-learn-drill--cleanup-sources
         (mapcar #'find-file-noselect files) pre-existing)))))

;;;###autoload
(defun org-learn-drill (&optional scope)
  "Run a drill session over the due and new cards in SCOPE.
Cards are headings tagged `org-learn-drill-tag'; due cards come
first (oldest SCHEDULED first), then new unscheduled cards.  Keys
in the session buffer: SPC reveal, 0-5 grade (scheduling delegates
to the spec 002 review core), s skip, e edit source, q quit."
  (interactive)
  (org-learn-drill--session 'drill scope))

;;;###autoload
(defun org-learn-drill-cram (&optional scope)
  "Run a cram session: all cards in SCOPE regardless of schedule.
Cards reviewed within `org-learn-drill-cram-hours' are skipped."
  (interactive)
  (org-learn-drill--session 'cram scope))

(defvar org-learn-drill-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'org-learn-drill-reveal)
    map)
  "Keymap for the drill session buffer.
SPC reveals; grading, skipping, source editing and quitting run
through the session loop in `org-learn-drill--run'.")

(define-derived-mode org-learn-drill-mode org-mode "Org-Learn-Drill"
  "Major mode for the dedicated drill session buffer.
One card at a time; SPC reveals the hidden side.  Grading (0-5),
skip (s), source editing (e) and quitting (q) are handled by the
session loop in `org-learn-drill--run'.
\\{org-learn-drill-mode-map}"
  (add-to-invisibility-spec 'org-learn-drill-mask)
  (setq buffer-read-only t))

;;;###autoload
(defun org-learn-drill-reveal ()
  "Reveal the masked regions of the current drill card."
  (interactive)
  (org-learn-drill--reveal))

(provide 'org-learn-drill)
;;; org-learn-drill.el ends here
