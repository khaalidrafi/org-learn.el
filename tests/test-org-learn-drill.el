;;; tests/test-org-learn-drill.el --- ERT suite for org-learn-drill -*- lexical-binding: t; -*-

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

;; ERT suite for the spec 003 drill sessions
;; (specs/003-drill-sessions.org): the cloze parse table, card
;; enumeration on a temp Org file (tag law, empty-card skip, twosided
;; sides), the one-grade-one-SCHEDULED round trip through the review
;; core, the cram hours filter and the session limit.  No session
;; buffer is driven interactively; the graded path is the same
;; `org-learn-review--process-entry' call the session loop makes.
;; Loaded by tests/test-org-learn.el so the single CI entry runs
;; everything.

;;; Code:

(require 'ert)
(require 'org-learn)
(require 'org-learn-drill)

(defun org-learn-drill-test--count-matches (regexp string)
  "Count non-overlapping REGEXP matches in STRING (line-anchored)."
  (let ((count 0))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq count (1+ count))))
    count))

(defun org-learn-drill-test--file-string (file)
  "Return FILE's contents as a string, without visiting it."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-learn-drill-test--org-date (days)
  "Return the Org timestamp for DAYS days from today, like <2026-11-02 Mon>."
  (let ((greg (calendar-gregorian-from-absolute (+ (org-today) days))))
    (format-time-string "<%Y-%m-%d %a>"
                        (encode-time 0 0 0
                                     (nth 1 greg) (nth 0 greg) (nth 2 greg)))))

(defun org-learn-drill-test--kill-file-buffer (file)
  "Kill FILE's buffer, leaving no modified state behind."
  (let ((buffer (get-file-buffer file)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer))))

;;; Cloze parsing (spec 003 acceptance: answers + hints extracted
;;; exactly).

(ert-deftest org-learn-drill-parse-cloze-extracts-answers-hints-spans ()
  "Spec 003: cloze parsing extracts answers and hints exactly \
from [answer||hint], plain [answer] and multiple spans."
  ;; Answer with a hint: hint stays a separate segment.
  (should (equal (org-learn-drill--parse-cloze
                  "The [hippocampus||brain area] consolidates memory.")
                 (list '(:type text :text "The ")
                       '(:type cloze :answer "hippocampus"
                               :hint "brain area")
                       '(:type text :text " consolidates memory."))))
  ;; Plain answer: no hint.
  (should (equal (org-learn-drill--parse-cloze "The answer is [42].")
                 (list '(:type text :text "The answer is ")
                       '(:type cloze :answer "42" :hint nil)
                       '(:type text :text "."))))
  ;; Multiple spans: empty literal segments around them are dropped.
  (should (equal (org-learn-drill--parse-cloze "[a] and [b||c]")
                 (list '(:type cloze :answer "a" :hint nil)
                       '(:type text :text " and ")
                       '(:type cloze :answer "b" :hint "c"))))
  ;; A body without cloze spans is one literal segment; empty
  ;; brackets are not a cloze and stay literal.
  (should (equal (org-learn-drill--parse-cloze "no cloze here")
                 (list '(:type text :text "no cloze here"))))
  (should (equal (org-learn-drill--parse-cloze "[] stays literal")
                 (list '(:type text :text "[] stays literal")))))

;;; Card enumeration (spec 003 acceptance: tag law, empty skip,
;;; twosided sides).

(ert-deftest org-learn-drill-cards-enumeration ()
  "Spec 003: cards enumerate by tag in file order; empty cards \
are skipped and twosided sides resolve to the first two children."
  (let* ((file (make-temp-file "org-learn-drill-" nil ".org"))
         (org-learn-file file)
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert "\
* Deck

** Card one :drill:
   simple body

** Card two :drill:
   :PROPERTIES:
   :DRILL_CARD_TYPE: cloze
   :END:
   The answer is [42].

** Empty card :drill:
** Twosided card :drill:
   :PROPERTIES:
   :DRILL_CARD_TYPE: twosided
   :END:
*** Front
    front side
*** Back
    back side
*** Notes
    extra notes

** Untagged card
   not a card
")
            (let ((cards (org-learn-drill--cards 'file)))
              (should (equal (mapcar (lambda (card)
                                       (plist-get card :title))
                                     cards)
                             (list "Card one" "Card two"
                                   "Twosided card")))
              ;; Tag law: every enumerated card carries the tag.
              (dolist (card cards)
                (should (member "drill" (plist-get card :tags))))
              ;; The empty card is skipped; the untagged entry is not
              ;; a card at all.
              (should-not (member "Empty card"
                                  (mapcar (lambda (card)
                                            (plist-get card :title))
                                          cards)))
              (should-not (member "Untagged card"
                                  (mapcar (lambda (card)
                                            (plist-get card :title))
                                          cards)))
              ;; Types: default simple, the cloze property honored.
              (should (equal (plist-get (nth 0 cards) :type) "simple"))
              (should (equal (plist-get (nth 1 cards) :type) "cloze"))
              (should (string-match-p "\\[42\\]"
                                      (plist-get (nth 1 cards) :body)))
              ;; Twosided: first two children are the sides, further
              ;; children are notes.
              (let ((sides (plist-get (nth 2 cards) :sides)))
                (should (equal (mapcar #'car sides)
                               (list "Front" "Back")))
                (should (string-match-p "front side"
                                        (cdr (nth 0 sides))))
                (should (string-match-p "back side"
                                        (cdr (nth 1 sides)))))
              (should (string-match-p "extra notes"
                                      (plist-get (nth 2 cards) :notes))))))
      (org-learn-drill-test--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

;;; Grading (spec 003 acceptance: one grade, one SCHEDULED, via the
;;; review core).

(ert-deftest org-learn-drill-grade-writes-one-scheduled ()
  "Spec 003: one session grade writes exactly one SCHEDULED on the \
source entry, through `org-learn-review--process-entry'."
  (let* ((file (make-temp-file "org-learn-grade-" nil ".org"))
         (org-learn-file file)
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert "* Card to grade :drill:\n   body to remember\n"))
          ;; The card the session carries is a plist whose :marker
          ;; points into the source entry; grade it the way the
          ;; session does (`org-learn-drill--grade'): the review core,
          ;; the leech law, then save the source file.
          (let ((card (with-current-buffer buffer
                        (goto-char (point-min))
                        (list :marker (copy-marker (point))
                              :title "Card to grade"))))
            (org-learn-drill--grade card 5)
            (let ((content (org-learn-drill-test--file-string file)))
              (should (= 1 (org-learn-drill-test--count-matches
                            "^[ \t]*SCHEDULED:" content)))
              (should (string-match-p "^[ \t]*:LEARN_REPS: 1$" content))
              (should (string-match-p
                       (regexp-quote
                        (concat "SCHEDULED: "
                                (org-learn-drill-test--org-date 1)))
                       content))
              (should (= 1 (org-learn-drill-test--count-matches
                            "^- Note taken on" content))))
            ;; A second grade updates the planning line in place: the
            ;; entry still carries exactly one SCHEDULED.
            (org-learn-drill--grade card 4)
            (let ((content (org-learn-drill-test--file-string file)))
              (should (= 1 (org-learn-drill-test--count-matches
                            "^[ \t]*SCHEDULED:" content)))
              (should (string-match-p "^[ \t]*:LEARN_REPS: 2$" content))
              (should (string-match-p
                       (regexp-quote
                        (concat "SCHEDULED: "
                                (org-learn-drill-test--org-date 6)))
                       content))
              (should (= 2 (org-learn-drill-test--count-matches
                            "^- Note taken on" content))))))
      (org-learn-drill-test--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

;;; Leech law (spec 003: lapses at the threshold tag :leech:,
;;; idempotently).

(ert-deftest org-learn-drill-leech-threshold-tags-the-entry ()
  "Spec 003: lapses at `org-learn-drill-leech-threshold' tag the \
entry :leech:; below the threshold nothing is tagged, and applying \
the law again is idempotent."
  (let* ((file (make-temp-file "org-learn-leech-" nil ".org"))
         (org-learn-file file)
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert "* Leech candidate :drill:\n   body\n"))
          (let ((card (with-current-buffer buffer
                        (goto-char (point-min))
                        (list :marker (copy-marker (point))))))
            ;; 14 lapses, just below the default threshold of 15: no
            ;; tag.
            (with-current-buffer buffer
              (goto-char (point-min))
              (org-entry-put (point) "LEARN_LAPSES" "14"))
            (org-learn-drill--apply-leech-law (plist-get card :marker))
            (with-current-buffer buffer
              (goto-char (point-min))
              (should-not (member "leech" (org-get-tags (point) t))))
            ;; 15 lapses, at the threshold: tagged once.
            (with-current-buffer buffer
              (goto-char (point-min))
              (org-entry-put (point) "LEARN_LAPSES" "15"))
            (org-learn-drill--apply-leech-law (plist-get card :marker))
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (member "leech" (org-get-tags (point) t))))
            ;; Idempotent: applying again keeps exactly one tag.
            (org-learn-drill--apply-leech-law (plist-get card :marker))
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (= 1 (org-learn-drill-test--count-matches
                            ":leech:" (buffer-string)))))))
      (org-learn-drill-test--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-learn-drill-grade-applies-the-leech-law ()
  "Spec 003: a grade that lapses past the threshold tags the source \
entry :leech:; a further lapse never duplicates the tag."
  (let* ((file (make-temp-file "org-learn-leech2-" nil ".org"))
         (org-learn-file file)
         (org-learn-drill-leech-threshold 1)
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (let ((card (with-current-buffer buffer
                        (insert "* Lapse card :drill:\n   hard body\n")
                        (goto-char (point-min))
                        (list :marker (copy-marker (point))))))
            ;; One failed grade: one lapse, at the threshold of 1.
            (org-learn-drill--grade card 2)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (string= (org-entry-get (point) "LEARN_LAPSES") "1"))
              (should (member "leech" (org-get-tags (point) t))))
            ;; A second lapse stays exactly one :leech: tag.
            (org-learn-drill--grade card 0)
            (let ((content (org-learn-drill-test--file-string file)))
              (should (= 1 (org-learn-drill-test--count-matches
                            ":leech:" content))))))
      (org-learn-drill-test--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

;;; Cram window (spec 003: reviewed-within-hours cards stay away).

(ert-deftest org-learn-drill-cram-hours-filter ()
  "Spec 003: cram keeps cards reviewed before the window or never \
reviewed, and drops cards reviewed within \
`org-learn-drill-cram-hours'."
  (let* ((file (make-temp-file "org-learn-cram-" nil ".org"))
         (org-learn-file file)
         (now (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (old (format-time-string "[%Y-%m-%d %a %H:%M]"
                                  (time-subtract (current-time)
                                                 (days-to-time 400))))
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert (format "\
* Cram recent :drill:
  :LOGBOOK:
- Note taken on %s \\\\
  Reviewed with quality 5.
  :END:
   reviewed just now

* Cram old :drill:
  :LOGBOOK:
- Note taken on %s \\\\
  Reviewed with quality 5.
  :END:
   reviewed long ago

* Cram fresh :drill:
   never reviewed
" now old))
            ;; Card plists the way the session carries them: a title
            ;; plus a marker into the source entry.
            (let ((cards nil))
              (org-map-entries
               (lambda ()
                 (when (member "drill" (org-get-tags (point) t))
                   (push (list :title (org-get-heading t t t t)
                               :marker (copy-marker (point)))
                         cards)))
               nil 'file)
              (setq cards (nreverse cards))
              (let ((org-learn-drill-cram-hours 12))
                (should (= 3 (length cards)))
                (should (equal
                         (mapcar (lambda (card)
                                   (plist-get card :title))
                                 (org-learn-drill--cram-filter cards))
                         (list "Cram old" "Cram fresh")))))))
      (org-learn-drill-test--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

;;; Session limit (spec 003: org-learn-drill-max-items honored).

(ert-deftest org-learn-drill-max-items-is-honored ()
  "Spec 003: the session limit truncates the card list; nil is unlimited."
  (let ((cards (list (list :title "one")
                     (list :title "two")
                     (list :title "three"))))
    (let ((org-learn-drill-max-items 2))
      (should (equal (mapcar (lambda (card)
                               (plist-get card :title))
                             (org-learn-drill--limit cards))
                     (list "one" "two"))))
    (let ((org-learn-drill-max-items nil))
      (should (= 3 (length (org-learn-drill--limit cards)))))))

;;; Command and boundary law.

(ert-deftest org-learn-drill-commands-are-commands ()
  "Spec 003: the drill, cram and reveal entry points are commands."
  (should (commandp 'org-learn-drill))
  (should (commandp 'org-learn-drill-cram))
  (should (commandp 'org-learn-drill-reveal)))

(ert-deftest org-learn-drill-keeps-the-org-roadmap-boundary ()
  "Spec 003: the drill feature never loads org-roadmap either."
  (should-not (featurep 'org-roadmap)))

(provide 'test-org-learn-drill)
;;; test-org-learn-drill.el ends here
