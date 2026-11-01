;;; tests/test-org-learn-ai.el --- ERT suite for org-learn-ai -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Khalid

;; Author: Khalid
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.0"))
;; Keywords: org, learning, ai

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT suite for the spec 004 AI enhancements
;; (specs/004-ai-tutor.org): the network core parse, the chat buffer,
;; flashcard generation and the quiz loop.  `org-learn-ai--complete'
;; (the single network choke point) and `url-retrieve-synchronously'
;; are mocked with `cl-letf', so the suite never touches the network.
;; Loaded by tests/test-org-learn.el so the single CI entry runs
;; everything.

;;; Code:

(require 'ert)
(require 'org-learn)
(require 'org-learn-ai)

(defun org-learn-ai-test--count-matches (regexp string)
  "Count non-overlapping REGEXP matches in STRING (line-anchored)."
  (let ((count 0))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq count (1+ count))))
    count))

(defun org-learn-ai-test--fake-url-buffer (status body)
  "Return a fresh buffer mimicking a url.el HTTP STATUS response.
BODY is inserted after the header block, like the real url.el
buffer layout (status line, headers, blank line, body)."
  (let ((buffer (generate-new-buffer " *org-learn-ai-test-url*")))
    (with-current-buffer buffer
      (insert (format "HTTP/1.1 %d Fake\n" status)
              "Content-Type: application/json\n"
              "\n"
              body)
      (goto-char (point-min)))
    buffer))

(defun org-learn-ai-test--kill-chat ()
  "Kill the chat buffer, leaving no litter behind."
  (let ((chat (get-buffer org-learn-ai--chat-buffer)))
    (when chat
      (with-current-buffer chat
        (set-buffer-modified-p nil))
      (kill-buffer chat))))

;;; Network core: request shape and reply parse (mocked url.el).

(ert-deftest org-learn-ai-complete-parses-the-openai-reply ()
  "Spec 004: the reply parses to choices[0].message.content."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _)
               (org-learn-ai-test--fake-url-buffer
                200
                "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"pong\"}}]}"))))
    (should (equal (org-learn-ai--complete
                    (list (org-learn-ai--user-message "hi")))
                   "pong"))))

(ert-deftest org-learn-ai-complete-sends-model-and-bearer-only-with-key ()
  "Spec 004: the request body JSON-encodes correctly and the \
Authorization header is present iff `org-learn-ai-api-key' is set."
  (let (captured-url captured-headers captured-data)
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest _)
                 (setq captured-url url
                       captured-headers url-request-extra-headers
                       captured-data url-request-data)
                 (org-learn-ai-test--fake-url-buffer 200
                                                     "{\"choices\":[{\"message\":{\"content\":\"pong\"}}]}"))))
      ;; No key: no Authorization header.
      (let ((org-learn-ai-api-key nil))
        (should (equal (org-learn-ai--complete
                        (list (org-learn-ai--user-message "hi")))
                       "pong"))
        (should (equal captured-url org-learn-ai-endpoint))
        (should (null (assoc-default "Authorization" captured-headers)))
        (should (equal (assoc-default "Content-Type" captured-headers)
                       "application/json"))
        (should (string-match-p "\"model\":\"openai\"" captured-data))
        (should (string-match-p "\"messages\"" captured-data))
        (should (string-match-p "\"role\":\"user\"" captured-data)))
      ;; Key set: Bearer header travels, and only then.
      (let ((org-learn-ai-api-key "sekrit"))
        (should (equal (org-learn-ai--complete
                        (list (org-learn-ai--user-message "hi")))
                       "pong"))
        (should (equal (assoc-default "Authorization" captured-headers)
                       "Bearer sekrit"))))))

(ert-deftest org-learn-ai-complete-http-errors-user-error-with-hint ()
  "Spec 004: an HTTP failure is a user-error naming status + endpoint."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _) (org-learn-ai-test--fake-url-buffer 500 "boom"))))
    (let ((err (should-error (org-learn-ai--complete
                              (list (org-learn-ai--user-message "hi")))
                             :type 'user-error)))
      (should (string-match-p "500" (error-message-string err)))
      (should (string-match-p "org-learn-ai-endpoint"
                              (error-message-string err))))))

(ert-deftest org-learn-ai-complete-connection-failure-user-error-with-hint ()
  "Spec 004: a connection failure is a user-error with the endpoint hint."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _) (error "Connection refused"))))
    (let ((err (should-error (org-learn-ai--complete
                              (list (org-learn-ai--user-message "hi")))
                             :type 'user-error)))
      (should (string-match-p "Connection refused" (error-message-string err)))
      (should (string-match-p "org-learn-ai-endpoint"
                              (error-message-string err))))))

(ert-deftest org-learn-ai-complete-timeout-user-error-with-hint ()
  "Spec 004: a nil (timed out) response is a user-error too."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _) nil)))
    (should-error (org-learn-ai--complete
                   (list (org-learn-ai--user-message "hi")))
                  :type 'user-error)))

;;; Context cap (spec 004: head-safe truncation, noted cut).

(ert-deftest org-learn-ai-cap-context-keeps-head-and-notes-the-cut ()
  "Spec 004: the cap keeps the first N characters and notes the cut."
  (let ((org-learn-ai-context-max-chars 20))
    (let ((capped (org-learn-ai--cap-context (make-string 100 ?x))))
      (should (string-prefix-p (make-string 20 ?x) capped))
      (should (string-match-p "study context truncated to 20 characters" capped))
      (should-not (string-match-p "x\\{30\\}" capped)))
    ;; Short context passes through untouched.
    (should (equal (org-learn-ai--cap-context "short") "short"))))

(ert-deftest org-learn-ai-chat-context-is-capped ()
  "Spec 004: the chat context line honors the character cap."
  (let ((org-learn-ai-context-max-chars 50)
        (body (concat "head-" (make-string 200 ?y) "-tail")))
    (with-temp-buffer
      (org-mode)
      (insert "* Big\n" body "\n")
      (goto-char (point-min))
      (let (captured)
        (cl-letf (((symbol-function 'org-learn-ai--complete)
                   (lambda (messages)
                     (setq captured messages)
                     "ok")))
          (org-learn-ai-ask)
          (with-current-buffer org-learn-ai--chat-buffer
            (goto-char (point-max))
            (insert "hello")
            (org-learn-ai--chat-send))
          (let ((content (cdr (assq 'content (car (last captured))))))
            (should (string-match-p
                     "study context truncated to 50 characters" content))
            (should (string-match-p (regexp-quote (substring body 0 30))
                                    content))
            ;; The tail fell off the head-safe cut.
            (should-not (string-match-p "tail" content))))))
    (org-learn-ai-test--kill-chat)))

;;; Tutor chat (spec 004).

(ert-deftest org-learn-ai-chat-appends-turns-with-context-line ()
  "Spec 004: the chat renders quoted user turns and plain replies, \
keeps buffer-local history and embeds the study context line."
  (let (captured)
    (unwind-protect
        (progn
          (with-temp-buffer
            (org-mode)
            (insert "* Chapter\n  The hippocampus consolidates memory.\n")
            (goto-char (point-min))
            (cl-letf (((symbol-function 'org-learn-ai--complete)
                       (lambda (messages)
                         (setq captured messages)
                         "It consolidates long-term memory.")))
              (org-learn-ai-ask)
              (with-current-buffer org-learn-ai--chat-buffer
                (goto-char (point-max))
                (insert "What does the hippocampus do?")
                (org-learn-ai--chat-send)
                (let ((text (buffer-string)))
                  (should (string-match-p
                           "^> What does the hippocampus do\\?$" text))
                  (should (string-match-p
                           "It consolidates long-term memory\\." text))
                  (should (string-suffix-p "> " text)))
                ;; System prompt first, user turn last, history grew.
                (should (equal (cdr (assq 'role (car captured))) "system"))
                (should (= (length org-learn-ai--chat-history) 2))
                (let ((last (car (last captured))))
                  (should (equal (cdr (assq 'role last)) "user"))
                  (should (string-match-p "hippocampus consolidates memory"
                                          (cdr (assq 'content last))))
                  (should (string-match-p "What does the hippocampus do\\?"
                                          (cdr (assq 'content last)))))))))
      (org-learn-ai-test--kill-chat))))

(ert-deftest org-learn-ai-chat-new-resets-history ()
  "Spec 004: C-c C-n starts a new chat (history reset)."
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Topic\n  body\n")
        (goto-char (point-min))
        (cl-letf (((symbol-function 'org-learn-ai--complete)
                   (lambda (_messages) "reply")))
          (org-learn-ai-ask)
          (with-current-buffer org-learn-ai--chat-buffer
            (goto-char (point-max))
            (insert "first chat")
            (org-learn-ai--chat-send)
            (should org-learn-ai--chat-history)
            (org-learn-ai--chat-new)
            (should-not org-learn-ai--chat-history)
            (should-not (string-match-p "first chat" (buffer-string))))))
    (org-learn-ai-test--kill-chat)))

(ert-deftest org-learn-ai-ask-prefix-refresh-recaptures-context ()
  "Spec 004: a prefix arg re-reads the study context."
  (unwind-protect
      (progn
        (with-temp-buffer
          (org-mode)
          (insert "* First topic\n  first context\n")
          (goto-char (point-min))
          (org-learn-ai-ask))
        (with-temp-buffer
          (org-mode)
          (insert "* Second topic\n  second context\n")
          (goto-char (point-min))
          (org-learn-ai-ask t))
        (let (captured)
          (cl-letf (((symbol-function 'org-learn-ai--complete)
                     (lambda (messages)
                       (setq captured messages)
                       "ok")))
            (with-current-buffer org-learn-ai--chat-buffer
              (goto-char (point-max))
              (insert "hello")
              (org-learn-ai--chat-send))
            (should (string-match-p
                     "second context"
                     (cdr (assq 'content (car (last captured)))))))))
    (org-learn-ai-test--kill-chat)))

;;; Flashcards (spec 004).

(ert-deftest org-learn-ai-extract-json-array-tolerates-prose ()
  "Spec 004: the first JSON array is extracted from a prose reply."
  (should (equal (org-learn-ai--extract-json-array
                  "Sure! [{\"q\": \"Q?\", \"a\": \"A.\"}] Good luck!")
                 '(((q . "Q?") (a . "A.")))))
  (should (null (org-learn-ai--extract-json-array
                 "no json here at all"))))

(ert-deftest org-learn-ai-flashcards-insert-drill-structure ()
  "Spec 004: cards land as :drill: children with the right \
properties and Question/Answer child headings."
  (with-temp-buffer
    (org-mode)
    (insert "* Memory\n  Consolidation happens in the hippocampus.\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'org-learn-ai--complete)
               (lambda (_messages)
                 (json-encode
                  (list `((q . "Where does consolidation happen?")
                          (a . "In the hippocampus."))
                        `((q . "What is memory?")
                          (a . "A stored trace.")))))))
      (org-learn-ai-generate-flashcards)
      (let ((text (buffer-string)))
        (should (= 2 (org-learn-ai-test--count-matches " :drill:$" text)))
        (should (= 2 (org-learn-ai-test--count-matches
                      "^   :DRILL_CARD_TYPE: twosided$" text)))
        (should (= 2 (org-learn-ai-test--count-matches
                      "^   :CREATED_BY: ai$" text)))
        (should (= 2 (org-learn-ai-test--count-matches
                      "^   :CREATED: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T"
                      text)))
        (should (= 2 (org-learn-ai-test--count-matches
                      "^\\*\\*\\* Question$" text)))
        (should (= 2 (org-learn-ai-test--count-matches
                      "^\\*\\*\\* Answer$" text)))
        (should (string-match-p
                 "^\\*\\* Where does consolidation happen\\? :drill:$" text))
        (should (string-match-p "^    In the hippocampus\\.$" text))
        ;; The first card parses back as a twosided drill entry.
        (goto-char (point-min))
        (search-forward "** Where does consolidation happen?")
        (beginning-of-line)
        (should (equal (org-entry-get (point) "DRILL_CARD_TYPE") "twosided"))
        (should (equal (org-entry-get (point) "CREATED_BY") "ai"))
        (should (member "drill" (org-get-tags)))))))

(ert-deftest org-learn-ai-flashcards-parse-prose-wrapped-json ()
  "Spec 004: a reply with prose around the JSON array still inserts."
  (with-temp-buffer
    (org-mode)
    (insert "* Topic\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'org-learn-ai--complete)
               (lambda (_messages)
                 "Sure! Here are your cards:\n\n\
[{\"q\": \"Q1?\", \"a\": \"A1.\"}, {\"q\": \"Q2?\", \"a\": \"A2.\"}]\n\n\
Good luck studying!")))
      (org-learn-ai-generate-flashcards))
    (should (= 2 (org-learn-ai-test--count-matches
                  " :drill:$" (buffer-string))))))

(ert-deftest org-learn-ai-flashcards-garbage-reply-errors-without-insert ()
  "Spec 004: garbage replies raise `user-error' and insert nothing."
  (with-temp-buffer
    (org-mode)
    (insert "* Topic\n")
    (goto-char (point-min))
    ;; Prose only: no JSON array at all.
    (cl-letf (((symbol-function 'org-learn-ai--complete)
               (lambda (_messages) "Sorry, I cannot help with that.")))
      (should-error (org-learn-ai-generate-flashcards) :type 'user-error))
    ;; An array, but not flashcard objects.
    (cl-letf (((symbol-function 'org-learn-ai--complete)
               (lambda (_messages) "[1, 2, 3]")))
      (should-error (org-learn-ai-generate-flashcards) :type 'user-error))
    (should (equal (buffer-string) "* Topic\n"))))

;;; Quiz (spec 004).

(ert-deftest org-learn-ai-quiz-grade-parse-maps-5-3-1 ()
  "Spec 004: correct/partly/wrong parse to SM-2 quality 5/3/1."
  (should (= 5 (org-learn-ai--grade-quality "Correct! Well done.")))
  (should (= 3 (org-learn-ai--grade-quality "Partly correct: you forgot X.")))
  (should (= 1 (org-learn-ai--grade-quality "Wrong: it is Y, not X.")))
  (should (= 1 (org-learn-ai--grade-quality "Incorrect, see the notes.")))
  (should-error (org-learn-ai--grade-quality "I have no idea.") :type 'user-error))

(ert-deftest org-learn-ai-quiz-maps-grades-to-quality ()
  "Spec 004: the quiz loop grades via the SM-2 quality mapping."
  (dolist (case '(("correct" 5) ("partly correct" 3) ("wrong" 1)))
    (with-temp-buffer
      (org-mode)
      (insert "* Quiz me\n  body\n")
      (goto-char (point-min))
      (let ((grade-text (car case))
            (expected (cadr case))
            (quality nil)
            (calls 0)
            (answers (list t nil))) ; yes to schedule, no to continue
        (cl-letf* (((symbol-function 'org-learn-ai--complete)
                    (lambda (_messages)
                      (setq calls (1+ calls))
                      (if (= calls 1) "What is memory?" grade-text)))
                   ((symbol-function 'read-string)
                    (lambda (&rest _) "an answer"))
                   ((symbol-function 'yes-or-no-p)
                    (lambda (&rest _) (pop answers)))
                   ((symbol-function 'org-learn-review--process-entry)
                    (lambda (q) (setq quality q))))
          (org-learn-ai-quiz))
        (should (equal quality expected))))))

(ert-deftest org-learn-ai-quiz-schedules-review-on-accept ()
  "Spec 004: accepting the offer schedules via the review core."
  (let* ((file (make-temp-file "org-learn-ai-quiz-" nil ".org"))
         buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (insert "* Quiz entry\n  body to learn\n")
            (goto-char (point-min))
            (let ((calls 0)
                  (answers (list t nil))) ; yes to schedule, no to continue
              (cl-letf* (((symbol-function 'org-learn-ai--complete)
                          (lambda (_messages)
                            (setq calls (1+ calls))
                            (if (= calls 1) "What is memory?" "correct")))
                         ((symbol-function 'read-string)
                          (lambda (&rest _) "an answer"))
                         ((symbol-function 'yes-or-no-p)
                          (lambda (&rest _) (pop answers))))
                (org-learn-ai-quiz))
              (goto-char (point-min))
              (should (equal (org-entry-get (point) "LEARN_REPS") "1"))
              (should (org-entry-get (point) "SCHEDULED")))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

;;; Command and boundary law.

(ert-deftest org-learn-ai-commands-are-commands ()
  "Spec 004: the three AI entry points are commands."
  (should (commandp 'org-learn-ai-ask))
  (should (commandp 'org-learn-ai-generate-flashcards))
  (should (commandp 'org-learn-ai-quiz)))

(ert-deftest org-learn-ai-keeps-the-org-roadmap-boundary ()
  "Spec 004: the AI feature never loads org-roadmap either."
  (should-not (featurep 'org-roadmap)))

(provide 'test-org-learn-ai)
;;; test-org-learn-ai.el ends here
