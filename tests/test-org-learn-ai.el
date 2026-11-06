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
;; flashcard generation and the quiz loop.  Spec 007
;; (specs/007-docneural-tutor-modes.org, as amended 2026-11-02
;; 01:54Z) adds the backend dispatch tests and the TRACE/FOCUS/CLEAR
;; tutor mode tests.  `org-learn-ai--complete' (the single network
;; choke point) and `url-retrieve-synchronously' are mocked with
;; `cl-letf', so the suite never touches the network and never loads
;; gptel.  Loaded by tests/test-org-learn.el so the single CI entry
;; runs everything.

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
  "Spec 004: the reply parses to choices[0].message.content.
The keyless lane is pinned, so the test stays lane-exact even when
gptel is installed (spec 007)."
  (let ((org-learn-ai-backend 'keyless))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (org-learn-ai-test--fake-url-buffer
                  200
                  "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"pong\"}}]}"))))
      (should (equal (org-learn-ai--complete
                      (list (org-learn-ai--user-message "hi")))
                     "pong")))))

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
      (let ((org-learn-ai-api-key nil)
            (org-learn-ai-backend 'keyless))
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
      (let ((org-learn-ai-api-key "sekrit")
            (org-learn-ai-backend 'keyless))
        (should (equal (org-learn-ai--complete
                        (list (org-learn-ai--user-message "hi")))
                       "pong"))
        (should (equal (assoc-default "Authorization" captured-headers)
                       "Bearer sekrit"))))))

(ert-deftest org-learn-ai-complete-http-errors-user-error-with-hint ()
  "Spec 004: an HTTP failure is a user-error naming status + endpoint."
  (let ((org-learn-ai-backend 'keyless))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) (org-learn-ai-test--fake-url-buffer 500 "boom"))))
      (let ((err (should-error (org-learn-ai--complete
                                (list (org-learn-ai--user-message "hi")))
                               :type 'user-error)))
        (should (string-match-p "500" (error-message-string err)))
        (should (string-match-p "org-learn-ai-endpoint"
                                (error-message-string err)))))))

(ert-deftest org-learn-ai-complete-connection-failure-user-error-with-hint ()
  "Spec 004: a connection failure is a user-error with the endpoint hint."
  (let ((org-learn-ai-backend 'keyless))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) (error "Connection refused"))))
      (let ((err (should-error (org-learn-ai--complete
                                (list (org-learn-ai--user-message "hi")))
                               :type 'user-error)))
        (should (string-match-p "Connection refused" (error-message-string err)))
        (should (string-match-p "org-learn-ai-endpoint"
                                (error-message-string err)))))))

(ert-deftest org-learn-ai-complete-timeout-user-error-with-hint ()
  "Spec 004: a nil (timed out) response is a user-error too."
  (let ((org-learn-ai-backend 'keyless))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) nil)))
      (should-error (org-learn-ai--complete
                     (list (org-learn-ai--user-message "hi")))
                    :type 'user-error))))

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

;;; Backend dispatch (spec 007, amended 2026-11-02 01:54Z).

(ert-deftest org-learn-ai-dispatch-auto-uses-keyless-lane-without-gptel ()
  "Spec 007: backend auto picks the keyless lane when gptel is absent."
  (let ((keyless-called 0) (gptel-called 0) (provider-asked nil))
    (cl-letf* (((symbol-function 'org-learn-ai--gptel-available-p)
                (lambda () (setq provider-asked t) nil))
               ((symbol-function 'org-learn-ai--complete-keyless)
                (lambda (_messages)
                  (setq keyless-called (1+ keyless-called)) "keyless"))
               ((symbol-function 'org-learn-ai--complete-gptel)
                (lambda (_messages)
                  (setq gptel-called (1+ gptel-called)) "gptel")))
      (let ((org-learn-ai-backend 'auto))
        (should (equal (org-learn-ai--complete
                        (list (org-learn-ai--user-message "hi")))
                       "keyless"))
        (should provider-asked)
        (should (= keyless-called 1))
        (should (= gptel-called 0))))))

(ert-deftest org-learn-ai-dispatch-auto-uses-gptel-lane-when-loaded ()
  "Spec 007: backend auto picks the gptel lane when gptel is loaded."
  (let ((keyless-called 0) (gptel-called 0))
    (cl-letf* (((symbol-function 'org-learn-ai--gptel-available-p)
                (lambda () t))
               ((symbol-function 'org-learn-ai--complete-keyless)
                (lambda (_messages)
                  (setq keyless-called (1+ keyless-called)) "keyless"))
               ((symbol-function 'org-learn-ai--complete-gptel)
                (lambda (_messages)
                  (setq gptel-called (1+ gptel-called)) "gptel")))
      (let ((org-learn-ai-backend 'auto))
        (should (equal (org-learn-ai--complete
                        (list (org-learn-ai--user-message "hi")))
                       "gptel"))
        (should (= gptel-called 1))
        (should (= keyless-called 0))))))

(ert-deftest org-learn-ai-dispatch-keyless-calls-the-keyless-lane ()
  "Spec 007: backend keyless always serves through the keyless lane."
  (let ((keyless-called 0) (gptel-called 0) (provider-asked nil))
    (cl-letf* (((symbol-function 'org-learn-ai--gptel-available-p)
                (lambda () (setq provider-asked t)))
               ((symbol-function 'org-learn-ai--complete-keyless)
                (lambda (_messages)
                  (setq keyless-called (1+ keyless-called)) "keyless"))
               ((symbol-function 'org-learn-ai--complete-gptel)
                (lambda (_messages)
                  (setq gptel-called (1+ gptel-called)) "gptel")))
      (let ((org-learn-ai-backend 'keyless))
        (should (equal (org-learn-ai--complete
                        (list (org-learn-ai--user-message "hi")))
                       "keyless"))
        (should-not provider-asked)
        (should (= keyless-called 1))
        (should (= gptel-called 0))))))

(ert-deftest org-learn-ai-dispatch-gptel-without-package-user-errors-with-hint ()
  "Spec 007: backend gptel without the package user-errors with an \
install hint."
  (let ((org-learn-ai-backend 'gptel))
    (cl-letf (((symbol-function 'require)
               (lambda (_feature &optional _file _noerror) nil)))
      (let ((err (should-error (org-learn-ai--complete
                                (list (org-learn-ai--user-message "hi")))
                               :type 'user-error)))
        (should (string-match-p "package-install gptel"
                                (error-message-string err)))))))

(ert-deftest org-learn-ai-gptel-lane-delegates-with-the-same-messages ()
  "Spec 007: the gptel lane maps the same messages onto gptel's \
public request API and keeps the string contract."
  (let (captured-prompt captured-system)
    (cl-letf* (((symbol-function 'require)
                (lambda (feature &optional _file _noerror)
                  (eq feature 'gptel)))
               ((symbol-function 'gptel-request)
                (lambda (prompt &rest args)
                  (setq captured-prompt prompt
                        captured-system (plist-get args :system))
                  (funcall (plist-get args :callback) "pong" '(:status "OK")))))
      (should (equal (org-learn-ai--complete-gptel
                      (list (org-learn-ai--system-message "Be brief.")
                            (org-learn-ai--user-message "hi")
                            (org-learn-ai--assistant-message "hello")
                            (org-learn-ai--user-message "more")))
                     "pong"))
      (should (equal captured-system "Be brief."))
      (should (equal captured-prompt '("hi" "hello" "more"))))))

(ert-deftest org-learn-ai-gptel-lane-failure-user-errors ()
  "Spec 007: a nil gptel reply is a user-error, keeping the contract."
  (cl-letf* (((symbol-function 'require)
              (lambda (feature &optional _file _noerror)
                (eq feature 'gptel)))
             ((symbol-function 'gptel-request)
              (lambda (_prompt &rest args)
                (funcall (plist-get args :callback) nil '(:status "HTTP 503")))))
    (let ((err (should-error (org-learn-ai--complete-gptel
                              (list (org-learn-ai--user-message "hi")))
                             :type 'user-error)))
      (should (string-match-p "gptel" (error-message-string err)))
      (should (string-match-p "HTTP 503" (error-message-string err))))))

;;; Tutor modes (spec 007).

(ert-deftest org-learn-ai-trace-sends-trace-prompt-and-renders-turns ()
  "Spec 007: TRACE sends its system prompt variant, embeds the \
subtree and keeps the multi-turn history."
  (let (calls)
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* Neurons\n  Neurons transmit signals.\n")
          (goto-char (point-min))
          (cl-letf (((symbol-function 'org-learn-ai--complete)
                     (lambda (messages)
                       (setq calls (append calls (list messages)))
                       "1. Signals travel along axons.\n   Example: a reflex arc.")))
            (org-learn-ai-trace)
            (with-current-buffer org-learn-ai--chat-buffer
              ;; First turn: the trace system prompt and the subtree.
              (let ((first (car calls)))
                (should (equal (cdr (assq 'content (car first)))
                               org-learn-ai--trace-prompt))
                (should (string-match-p
                         "Neurons transmit signals"
                         (cdr (assq 'content (car (last first)))))))
              (let ((text (buffer-string)))
                (should (string-match-p
                         "^> Walk me through this study context step by step\\.$"
                         text))
                (should (string-match-p "1\\. Signals travel along axons\\." text))
                (should (string-suffix-p "> " text)))
              ;; A follow-up reuses the trace system prompt and history.
              (goto-char (point-max))
              (insert "Why axons?")
              (org-learn-ai--chat-send)
              (let ((second (car (last calls))))
                (should (equal (cdr (assq 'content (car second)))
                               org-learn-ai--trace-prompt))
                (should (= (length second) 4))) ; system + 2 history + user
              (should (= (length org-learn-ai--chat-history) 4))
              (should (string-match-p "^> Why axons\\?$" (buffer-string))))))
      (org-learn-ai-test--kill-chat))))

(ert-deftest org-learn-ai-focus-maps-grades-to-quality-and-offers-review ()
  "Spec 007: FOCUS grades map to SM-2 quality 5/3/1 and offer scheduling."
  (dolist (case '(("correct" 5) ("partly correct" 3) ("wrong" 1)))
    (let ((grade-text (car case))
          (expected (cadr case))
          (calls nil)
          quality)
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (insert "* Memory\n  Consolidation happens in the hippocampus.\n")
            (goto-char (point-min))
            (let ((step 0))
              (cl-letf* (((symbol-function 'org-learn-ai--complete)
                          (lambda (messages)
                            (setq calls (append calls (list messages)))
                            (setq step (1+ step))
                            (if (= step 1)
                                "What consolidates memory?"
                              grade-text)))
                         ((symbol-function 'read-string)
                          (lambda (&rest _) "the hippocampus"))
                         ((symbol-function 'yes-or-no-p)
                          (lambda (&rest _) t))
                         ((symbol-function 'org-learn-review--process-entry)
                          (lambda (q) (setq quality q))))
                (org-learn-ai-focus)))
            (should (equal quality expected))
            ;; Both requests carry the FOCUS system prompt variant.
            (should (= (length calls) 2))
            (dolist (call calls)
              (should (equal (cdr (assq 'content (car call)))
                             org-learn-ai--focus-prompt)))
            (with-current-buffer org-learn-ai--chat-buffer
              (let ((text (buffer-string)))
                (should (string-match-p "What consolidates memory\\?" text))
                (should (string-match-p "^> My answer: the hippocampus$" text))
                (should (string-match-p (regexp-quote grade-text) text)))))
        (org-learn-ai-test--kill-chat)))))

(ert-deftest org-learn-ai-clear-renders-summary-and-offers-flashcards ()
  "Spec 007: CLEAR passes the summary through and offers flashcards; \
accepting the offer calls the generator."
  (dolist (accept '(t nil))
    (let ((flashcards-called 0) (calls nil))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (insert "* Synapses\n  Synapses strengthen with use.\n")
            (goto-char (point-min))
            (cl-letf* (((symbol-function 'org-learn-ai--complete)
                        (lambda (messages)
                          (setq calls (append calls (list messages)))
                          "- Repetition strengthens synapses.\n- Spacing beats cramming.\n- Sleep consolidates.\n\n[recall||cue] [testing||effect] [spacing||gap]"))
                       ((symbol-function 'yes-or-no-p)
                        (lambda (&rest _) accept))
                       ((symbol-function 'org-learn-ai-generate-flashcards)
                        (lambda ()
                          (setq flashcards-called (1+ flashcards-called)))))
              (org-learn-ai-clear))
            ;; The CLEAR system prompt variant went out, and the reply
            ;; passes through into the chat buffer unchanged.
            (should (= (length calls) 1))
            (should (equal (cdr (assq 'content (car (car calls))))
                           org-learn-ai--clear-prompt))
            (with-current-buffer org-learn-ai--chat-buffer
              (let ((text (buffer-string)))
                (should (string-match-p "Repetition strengthens synapses\\."
                                        text))
                (should (string-match-p "\\[recall||cue\\]" text))))
            (should (= flashcards-called (if accept 1 0))))
        (org-learn-ai-test--kill-chat)))))

(ert-deftest org-learn-ai-tutor-modes-are-commands ()
  "Spec 007: TRACE, FOCUS and CLEAR are interactive commands."
  (should (commandp 'org-learn-ai-trace))
  (should (commandp 'org-learn-ai-focus))
  (should (commandp 'org-learn-ai-clear)))

(ert-deftest org-learn-ai-tutor-modes-need-a-subtree-under-point ()
  "Spec 007: TRACE, FOCUS and CLEAR act on the current subtree only."
  (with-temp-buffer
    (org-mode)
    (insert "plain text with no heading\n")
    (goto-char (point-min))
    (dolist (mode '(org-learn-ai-trace org-learn-ai-focus org-learn-ai-clear))
      (should-error (funcall mode) :type 'user-error))))

;;; Command and boundary law.

(ert-deftest org-learn-ai-commands-are-commands ()
  "Spec 004: the three AI entry points are commands."
  (should (commandp 'org-learn-ai-ask))
  (should (commandp 'org-learn-ai-generate-flashcards))
  (should (commandp 'org-learn-ai-quiz)))

(ert-deftest org-learn-ai-keeps-the-org-roadmap-boundary ()
  "Spec 004: the AI feature never loads org-roadmap either."
  (should-not (featurep 'org-roadmap)))

;;; Guide mode (spec 009).

(ert-deftest org-learn-ai-guide-prompt-names-the-principles ()
  "Spec 009: the system prompt names the five LearnLM principles
and the never-answer law, verbatim-named."
  (should (string-match-p "active learning" org-learn-ai--guide-prompt))
  (should (string-match-p "cognitive-load management"
                          org-learn-ai--guide-prompt))
  (should (string-match-p "adapt to the learner"
                          org-learn-ai--guide-prompt))
  (should (string-match-p "stimulate curiosity"
                          org-learn-ai--guide-prompt))
  (should (string-match-p "deepen metacognition"
                          org-learn-ai--guide-prompt))
  (should (string-match-p "Never answer outright"
                          org-learn-ai--guide-prompt)))

(ert-deftest org-learn-ai-guide-demand-and-attem[REDACTED:pattern] ()
  "Spec 009: answer-demands climb the ladder, attempts never match."
  (dolist (demand '("just tell me the answer"
                    "what is the answer"
                    "I don't know"
                    "the answer please"
                    "just give me the answer"))
    (should (org-learn-ai--guide-demand-p demand))
    (should-not (org-learn-ai--guide-attempt-p demand)))
  (dolist (attempt '("the hippocampus consolidates it"
                     "I think it is about spacing, maybe?"))
    (should (org-learn-ai--guide-attempt-p attempt))
    (should-not (org-learn-ai--guide-demand-p attempt)))
  ;; blank input is neither an attempt nor a demand; it exits the loop
  (should-not (org-learn-ai--guide-attempt-p "   ")))

(ert-deftest org-learn-ai-guide-cycle-renders-in-order ()
  "Spec 009: probe -> answer -> grade-without-revealing -> next
probe render in order, each call carrying the guide system prompt."
  (let ((calls nil) (step 0) (scheduled nil) (quality nil))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* Spacing\n  Spacing beats cramming.\n")
          (goto-char (point-min))
          (cl-letf* (((symbol-function 'org-learn-ai--complete)
                      (lambda (messages)
                        (setq calls (append calls (list messages)))
                        (setq step (1+ step))
                        (if (= step 1)
                            "Why does spacing beat cramming?"
                          "Strength: you tied it to consolidation.\nGap: the timing.\nNext probe: when would you review first?")))
                     ((symbol-function 'read-string)
                      (lambda (&rest _)
                        (if (< step 2) "because memory needs time" "")))
                     ((symbol-function 'yes-or-no-p)
                      (lambda (&rest _) nil))
                     ((symbol-function 'org-learn-review--process-entry)
                      (lambda (q) (setq quality q) (setq scheduled t))))
            (org-learn-ai-guide))
          ;; every call carries the guide prompt (the mock captured it)
          (dolist (call calls)
            (should (equal (cdr (assq 'content (car call)))
                           org-learn-ai--guide-prompt)))
          (should (= (length calls) 2)) ; probe + one graded answer
          (with-current-buffer org-learn-ai--chat-buffer
            (let ((text (buffer-string)))
              (should (< (string-match-p
                          "Why does spacing beat cramming\\?" text)
                         (string-match-p
                          "^> My answer: because memory needs time$" text)))
              (should (< (string-match-p
                          "^> My answer: because memory needs time$" text)
                         (string-match-p "Strength: you tied it" text)))
              (should (string-match-p "Next probe: when would you review"
                                      text))))
          ;; declining the offers schedules nothing
          (should-not scheduled) (should-not quality))
      (org-learn-ai-test--kill-chat))))

(ert-deftest org-learn-ai-guide-hint-ladder-escalates ()
  "Spec 009: three consecutive answer-demands escalate nudge ->
scaffold -> worked example; without a written attempt the third
ask stays on the scaffold and never reveals."
  (dolist (case '((:third-attempt t
                   :expect "WORKED EXAMPLE"
                   :absent "NUDGE level")
                  (:third-attempt nil
                   :expect "Stay on the SCAFFOLD level"
                   :absent "WORKED EXAMPLE")))
    (let ((third-attempt (plist-get case :third-attempt))
          (expect-word (plist-get case :expect))
          (absent-word (plist-get case :absent))
          (calls nil) (step 0) (asks 0))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (insert "* Axons\n  Axons carry signals.\n")
            (goto-char (point-min))
            (cl-letf* (((symbol-function 'org-learn-ai--complete)
                        (lambda (messages)
                          (setq calls (append calls (list messages)))
                          (setq step (1+ step))
                          (if (= step 1)
                              "Why do axons matter?"
                            "A ladder reply.")))
                       ((symbol-function 'read-string)
                        (lambda (&rest _)
                          (setq asks (1+ asks))
                          (cond ((= asks 1) "tell me the answer")
                                ((= asks 2) "just give me the answer")
                                ((= asks 3)
                                 (if third-attempt
                                     "i don't know, but my attempt is: myelin speeds it"
                                   "i don't know"))
                                (t ""))))
                       ((symbol-function 'yes-or-no-p)
                        (lambda (&rest _) nil)))
              (org-learn-ai-guide))
            ;; four model calls: probe + three ladder turns
            (should (= (length calls) 4))
            ;; the three demand turns climb: 2nd call NUDGE, 3rd
            ;; SCAFFOLD, 4th per the case under test
            (let ((turn2 (cdr (assq 'content (car (last (nth 1 calls))))))
                  (turn3 (cdr (assq 'content (car (last (nth 2 calls))))))
                  (turn4 (cdr (assq 'content (car (last (nth 3 calls)))))))
              ;; exact-case ladder words: string-match-p honors
              ;; case-fold-search, and "WORKED EXAMPLE" must not match
              ;; the scaffold-repeat's lowercase prose
              (let ((case-fold-search nil))
                (should (string-match-p "NUDGE" turn2))
                (should (string-match-p "SCAFFOLD" turn3))
                (should (string-match-p expect-word turn4))
                (should-not (string-match-p absent-word turn4)))))
        (org-learn-ai-test--kill-chat)))))

(ert-deftest org-learn-ai-guide-exit-offer-maps-quality ()
  "Spec 009: the exit offer grades the session 5|3|1 through the
grading seam and schedules via the FOCUS contract internals."
  (dolist (case '(("correct" 5) ("partly" 3) ("wrong" 1)))
    (let ((grade-text (car case))
          (expected (cadr case))
          (quality nil) (scheduled nil) (step 0))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (insert "* Sleep\n  Sleep consolidates memory.\n")
            (goto-char (point-min))
            (cl-letf* (((symbol-function 'org-learn-ai--complete)
                        (lambda (_messages)
                          (setq step (1+ step))
                          (if (= step 1)
                              "What does sleep do?"
                            grade-text)))
                       ((symbol-function 'read-string)
                        (lambda (&rest _) ""))
                       ((symbol-function 'yes-or-no-p)
                        (lambda (&rest _) t))
                       ((symbol-function 'org-learn-review--process-entry)
                        (lambda (q) (setq quality q) (setq scheduled t))))
              (org-learn-ai-guide))
            (should (equal quality expected))
            (should scheduled))
        (org-learn-ai-test--kill-chat)))))

;;; Quiz persistence (spec 012, the queued part).

(defmacro org-learn-ai-test--with-quiz (grade-reply answer accept &rest body)
  "Run one quiz cycle in a fresh Org temp buffer, then BODY there.
GRADE-REPLY is the mocked grading turn, ANSWER the learner's
input; ACCEPT selects whether the SCHEDULING offer is accepted --
\"Another question?\" always reads no, so the loop stays at one
cycle.  BODY runs with the buffer current and point at point-min,
inside the buffer's lifetime."
  (declare (indent 3))
  `(let ((step 0))
     (with-temp-buffer
       (org-mode)
       (insert "* Quiz me\n  The material under quiz.\n")
       (goto-char (point-min))
       (cl-letf* (((symbol-function 'org-learn-ai--complete)
                   (lambda (_messages)
                     (setq step (1+ step))
                     (if (= step 1)
                         "What is the material about?"
                       ,grade-reply)))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) ,answer))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (prompt &rest _)
                     (and ,accept (string-match-p "Schedule" prompt))))
                  ((symbol-function 'org-learn-review--process-entry)
                   (lambda (_q) nil))
                  ((symbol-function 'save-buffer) (lambda () nil)))
         (org-learn-ai-quiz))
       (goto-char (point-min))
       (progn ,@body))))

(ert-deftest org-learn-ai-quiz-wrong-answers-count-both-counters ()
  "Spec 012: a wrong grade bumps LEARN_QUIZZES and LEARN_QUIZ_MISSES,
persisted on the entry even with every offer declined."
  (org-learn-ai-test--with-quiz "wrong: the answer is different"
      "a guess" nil
    (should (string= "1"
                     (org-entry-get (point-min) "LEARN_QUIZZES" nil)))
    (should (string= "1"
                     (org-entry-get (point-min) "LEARN_QUIZ_MISSES" nil)))))

(ert-deftest org-learn-ai-quiz-partly-is-never-a-miss ()
  "Spec 012: quality 3 counts an attempt; no miss counter appears."
  (org-learn-ai-test--with-quiz "partly correct"
      "half of it" nil
    (should (string= "1"
                     (org-entry-get (point-min) "LEARN_QUIZZES" nil)))
    (should (null (org-entry-get (point-min) "LEARN_QUIZ_MISSES" nil)))))

(ert-deftest org-learn-ai-quiz-counters-accumulate ()
  "The recorder accumulates on the same entry: three graded answers
with two wrongs read 3 quizzes, 2 misses."
  (with-temp-buffer
    (org-mode)
    (insert "* Accumulate\n  Body.\n")
    (goto-char (point-min))
    (org-learn-ai--record-quiz 1)
    (org-learn-ai--record-quiz 3)
    (org-learn-ai--record-quiz 1)
    (should (string= "3" (org-entry-get (point) "LEARN_QUIZZES" nil)))
    (should (string= "2" (org-entry-get (point) "LEARN_QUIZ_MISSES" nil)))))

(ert-deftest org-learn-ai-quiz-counters-land-before-the-offer ()
  "The counters are independent of the scheduling offer: an
accepted offer must not double-count, a declined one must not
erase the evidence (declined = the wrong-grade run above)."
  (org-learn-ai-test--with-quiz "correct"
      "the material" t
    (should (string= "1" (org-entry-get (point-min) "LEARN_QUIZZES" nil)))
    (should (null (org-entry-get (point-min) "LEARN_QUIZ_MISSES" nil)))))

(provide 'test-org-learn-ai)
;;; test-org-learn-ai.el ends here
