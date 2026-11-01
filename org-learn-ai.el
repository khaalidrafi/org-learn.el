;;; org-learn-ai.el --- AI tutor, flashcards and quiz for org-learn -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Khalid

;; Author: Khalid
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.0"))
;; Keywords: org, learning, ai
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

;; Spec 004 (specs/004-ai-tutor.org): AI enhancements native to Org --
;; study any subtree with a tutor that sees the subtree, turn the
;; subtree into drill cards, get quizzed by the model.
;;
;; - `org-learn-ai--complete' is the single network choke point: every
;;   model call goes through it, POSTing {model, messages} JSON to
;;   `org-learn-ai-endpoint' with url.el and json.el only (no new
;;   package dependencies).  The Authorization header is sent only
;;   when `org-learn-ai-api-key' is set; the key is never logged.
;; - `org-learn-ai-ask' runs a multi-turn tutor chat buffer over the
;;   current subtree context; RET sends, C-c C-n starts a new chat.
;; - `org-learn-ai-generate-flashcards' turns the current subtree into
;;   twosided drill cards (spec 003 card syntax).
;; - `org-learn-ai-quiz' asks one question at a time, grades the
;;   answer correct/partly/wrong (SM-2 quality 5/3/1) and offers to
;;   schedule a review through `org-learn-review--process-entry'.
;;
;; org-roadmap stays at the boundary: it is never required here
;; (AGENTS.md).

;;; Code:

(require 'org)
(require 'url)
(require 'json)
(require 'subr-x)
(require 'org-learn)

(defgroup org-learn-ai nil
  "AI tutor, flashcards and quiz for org-learn."
  :group 'org-learn
  :prefix "org-learn-")

(defcustom org-learn-ai-endpoint "https://text.pollinations.ai/openai"
  "OpenAI-compatible chat completions endpoint used by org-learn AI.
The default endpoint is keyless; any compatible endpoint works."
  :type 'string
  :group 'org-learn-ai)

(defcustom org-learn-ai-model "openai"
  "Model name sent in the request body to `org-learn-ai-endpoint'."
  :type 'string
  :group 'org-learn-ai)

(defcustom org-learn-ai-api-key nil
  "API key sent as a Bearer Authorization header, or nil for none.
The key travels only as this customization: it is never hardcoded
and never logged.  It is sent only when non-nil."
  :type '(choice (const :tag "No key (keyless endpoint)" nil)
                 (string :tag "API key"))
  :group 'org-learn-ai)

(defcustom org-learn-ai-context-max-chars 8000
  "Maximum number of subtree context characters sent to the model.
The context is truncated head-safe: the first N characters are
kept and the cut is noted in a trailing line."
  :type 'natnum
  :group 'org-learn-ai)

(defcustom org-learn-ai-timeout-seconds 30
  "Network timeout in seconds for one model request."
  :type 'natnum
  :group 'org-learn-ai)

(defconst org-learn-ai--chat-buffer "*org-learn-ai-chat*"
  "Name of the AI tutor chat buffer.")

(defconst org-learn-ai--tutor-prompt
  "You are a concise study tutor: explain, quiz and correct the user; answer in plain text without markdown."
  "System prompt giving the model the tutor persona (spec 004).")

(defconst org-learn-ai--flashcards-prompt
  "You generate study flashcards.  Reply with only a JSON array of 5-10 objects with keys \"q\" (question) and \"a\" (answer); no prose, no markdown fences."
  "System prompt for flashcard generation (spec 004).")

;;; Network core (the one choke point).

(defun org-learn-ai--system-message (content)
  "Return a system message alist carrying CONTENT."
  (list (cons 'role "system") (cons 'content content)))

(defun org-learn-ai--user-message (content)
  "Return a user message alist carrying CONTENT."
  (list (cons 'role "user") (cons 'content content)))

(defun org-learn-ai--assistant-message (content)
  "Return an assistant message alist carrying CONTENT."
  (list (cons 'role "assistant") (cons 'content content)))

(defun org-learn-ai--response-status ()
  "Return the HTTP status of the url.el response in the current buffer.
Interim 1xx responses are skipped; return nil when no status line
is found."
  (goto-char (point-min))
  (catch 'org-learn-ai-status
    (while (re-search-forward "^HTTP/[0-9.]+ +\\([0-9]+\\)" nil t)
      (let ((code (string-to-number (match-string 1))))
        (if (and (>= code 100) (< code 200))
            ;; Interim response: skip its header block, look again.
            (unless (re-search-forward "\r?\n\r?\n" nil t)
              (throw 'org-learn-ai-status nil))
          (throw 'org-learn-ai-status code))))
    nil))

(defun org-learn-ai--parse-response ()
  "Parse the url.el response in the current buffer; return the text.
The buffer holds a status line, headers, a blank line and then the
OpenAI-compatible JSON body.  Non-2xx statuses and malformed
bodies raise `user-error' with the status and an endpoint hint."
  (let ((status (org-learn-ai--response-status)))
    (unless (and status (>= status 200) (< status 300))
      (user-error "org-learn-ai: HTTP %s from %s (check org-learn-ai-endpoint)"
                  (or status "malformed") org-learn-ai-endpoint))
    (goto-char (point-min))
    (unless (re-search-forward "\r?\n\r?\n" nil t)
      (user-error "org-learn-ai: malformed response from %s (check org-learn-ai-endpoint)"
                  org-learn-ai-endpoint))
    (unless (search-forward "{" nil t)
      (user-error "org-learn-ai: no JSON body from %s (check org-learn-ai-endpoint)"
                  org-learn-ai-endpoint))
    (let* ((json (condition-case err
                     (json-read-from-string
                      (buffer-substring-no-properties
                       (1- (point)) (point-max)))
                   (error
                    (user-error "org-learn-ai: unreadable JSON body from %s: %s (check org-learn-ai-endpoint)"
                                org-learn-ai-endpoint
                                (error-message-string err)))))
           (choices (cdr (assq 'choices json))))
      (unless (and (vectorp choices) (> (length choices) 0))
        (user-error "org-learn-ai: no choices in the reply from %s (check org-learn-ai-endpoint)"
                    org-learn-ai-endpoint))
      (let ((content (cdr (assq 'content
                                (cdr (assq 'message (aref choices 0)))))))
        (unless (stringp content)
          (user-error "org-learn-ai: no message content in the reply from %s (check org-learn-ai-endpoint)"
                      org-learn-ai-endpoint))
        content))))

(defun org-learn-ai--complete (messages)
  "Send MESSAGES to the endpoint and return the assistant text.
MESSAGES is a list of message alists in conversation order, each
like ((role . \"user\") (content . \"...\")).  The request POSTs
{model, messages} JSON to `org-learn-ai-endpoint' through
`url-retrieve-synchronously'; the Authorization header is sent
only when `org-learn-ai-api-key' is non-nil.  Connection, timeout
and HTTP failures raise `user-error' with the status and an
endpoint hint.  This is the single network choke point of the AI
features (spec 004): every model call goes through here, which is
what tests mock with `cl-letf'."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          (cons '("Content-Type" . "application/json")
                (when org-learn-ai-api-key
                  (list (cons "Authorization"
                              (format "Bearer %s" org-learn-ai-api-key))))))
         (url-request-data
          (json-encode `((model . ,org-learn-ai-model)
                         (messages . ,messages)))))
    (let ((buffer
           (condition-case err
               (url-retrieve-synchronously org-learn-ai-endpoint
                                           t nil
                                           org-learn-ai-timeout-seconds)
             (error
              (user-error "org-learn-ai: request to %s failed: %s (check org-learn-ai-endpoint)"
                          org-learn-ai-endpoint
                          (error-message-string err))))))
      (unless (buffer-live-p buffer)
        (user-error "org-learn-ai: no response from %s within %d seconds (check org-learn-ai-endpoint)"
                    org-learn-ai-endpoint org-learn-ai-timeout-seconds))
      (unwind-protect
          (with-current-buffer buffer
            (org-learn-ai--parse-response))
        (kill-buffer buffer)))))

;;; Subtree context.

(defun org-learn-ai--cap-context (text)
  "Return TEXT capped at `org-learn-ai-context-max-chars'.
Head-safe: the first N characters are kept and the cut is noted in
a trailing line, so the model knows the context is incomplete."
  (if (<= (length text) org-learn-ai-context-max-chars)
      text
    (concat (substring text 0 org-learn-ai-context-max-chars)
            (format "\n\n[org-learn-ai: study context truncated to %d characters]"
                    org-learn-ai-context-max-chars))))

(defun org-learn-ai--context ()
  "Return the current subtree text, capped for the model.
The text runs from the heading at point through the end of its
subtree and is capped per `org-learn-ai--cap-context'.  Outside
Org, or outside any heading, the context is the empty string."
  (let ((text
         (if (not (derived-mode-p 'org-mode))
             ""
           (save-excursion
             (if (ignore-errors (org-back-to-heading t))
                 (buffer-substring-no-properties
                  (point) (org-end-of-subtree t))
               "")))))
    (org-learn-ai--cap-context (or text ""))))

;;; Tutor chat.

(defvar-local org-learn-ai--chat-history nil
  "Buffer-local conversation history of the chat buffer.
A list of message alists (see `org-learn-ai--complete') holding
the user and assistant turns in order.")

(defvar-local org-learn-ai--chat-context nil
  "Buffer-local study context captured at chat start.")

(defvar-local org-learn-ai--chat-context-sent nil
  "Non-nil once the study context was embedded in a user turn.")

(defvar-local org-learn-ai--chat-input-marker nil
  "Marker at the start of the chat input prompt.")

(defun org-learn-ai--quote-turn (text)
  "Return TEXT quoted as a chat user turn (\"> \" per line)."
  (mapconcat (lambda (line) (concat "> " line))
             (split-string (string-trim text) "\n")
             "\n"))

(defun org-learn-ai--chat-prompt ()
  "Append a fresh input prompt and aim the input marker at it."
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n"))
  (insert "> ")
  (setq org-learn-ai--chat-input-marker (copy-marker (point))))

(defun org-learn-ai--chat-welcome ()
  "Initialize the chat buffer contents and the input prompt."
  (erase-buffer)
  (insert "#+TITLE: org-learn AI tutor\n\n"
          "Type a message after the prompt; RET or C-c C-c sends, "
          "C-c C-n starts a new chat, q quits when the input is "
          "empty.\n\n")
  (org-learn-ai--chat-prompt))

(defun org-learn-ai--chat-user-content (input)
  "Return the user message content for INPUT.
The study context is embedded the first time it is used, so the
tutor sees the subtree; later turns send the raw input, because
the context is already part of the history."
  (if (and org-learn-ai--chat-context
           (not (string-empty-p org-learn-ai--chat-context))
           (not org-learn-ai--chat-context-sent))
      (progn
        (setq org-learn-ai--chat-context-sent t)
        (format "[Study context]\n%s\n\n[Message]\n%s"
                org-learn-ai--chat-context input))
    input))

(defun org-learn-ai--chat-send ()
  "Send the chat input to the tutor and insert the reply.
The input is the text after the input prompt.  The buffer is only
touched after a successful reply, so a failure leaves no litter."
  (interactive)
  (unless (markerp org-learn-ai--chat-input-marker)
    (user-error "org-learn-ai: no chat input here"))
  (let ((input (string-trim (buffer-substring-no-properties
                             org-learn-ai--chat-input-marker
                             (point-max)))))
    (when (string= input "")
      (user-error "org-learn-ai: type a message first"))
    (let* ((content (org-learn-ai--chat-user-content input))
           (reply (org-learn-ai--complete
                   (append (list (org-learn-ai--system-message
                                  org-learn-ai--tutor-prompt))
                           org-learn-ai--chat-history
                           (list (org-learn-ai--user-message content))))))
      (goto-char org-learn-ai--chat-input-marker)
      (beginning-of-line)
      (delete-region (point) (point-max))
      (goto-char (point-max))
      (insert (org-learn-ai--quote-turn input) "\n"
              (string-trim reply) "\n\n")
      (setq org-learn-ai--chat-history
            (append org-learn-ai--chat-history
                    (list (org-learn-ai--user-message content)
                          (org-learn-ai--assistant-message reply))))
      (set-marker org-learn-ai--chat-input-marker nil)
      (org-learn-ai--chat-prompt))))

(defun org-learn-ai--chat-new ()
  "Start a new chat: clear the history and the buffer."
  (interactive)
  (setq org-learn-ai--chat-history nil
        org-learn-ai--chat-context-sent nil)
  (org-learn-ai--chat-welcome)
  (goto-char (point-max)))

(defun org-learn-ai--chat-quit-or-self-insert ()
  "Quit the chat when the input is empty, else self-insert Q.
Typing \"q\" only quits at the fresh prompt; C-q q inserts a
literal q anywhere."
  (interactive)
  (if (and (eobp)
           (or (not (markerp org-learn-ai--chat-input-marker))
               (= (point) (marker-position
                           org-learn-ai--chat-input-marker))))
      (quit-window)
    (self-insert-command 1)))

(defvar org-learn-ai-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'org-learn-ai--chat-send)
    (define-key map (kbd "C-c C-c") 'org-learn-ai--chat-send)
    (define-key map (kbd "C-c C-n") 'org-learn-ai--chat-new)
    (define-key map (kbd "q") 'org-learn-ai--chat-quit-or-self-insert)
    map)
  "Keymap for the org-learn AI tutor chat buffer.")

(define-derived-mode org-learn-ai-chat-mode org-mode "Org-Learn-AI-Chat"
  "Major mode for the org-learn AI tutor chat buffer.
RET and C-c C-c send the message after the prompt, C-c C-n starts
a new chat and q quits when the input is empty.
\\{org-learn-ai-chat-mode-map}")

;;;###autoload
(defun org-learn-ai-ask (&optional refresh)
  "Chat with the AI tutor about the current subtree.
The chat lives in the buffer named by `org-learn-ai--chat-buffer'.
With prefix REFRESH, or when the chat is new, the current subtree
text (capped per `org-learn-ai-context-max-chars') is re-captured
as the study context.  In the chat buffer RET or C-c C-c sends the
message and C-c C-n starts a new chat."
  (interactive "P")
  ;; The context is captured in the invoking buffer, before the chat
  ;; buffer becomes current.
  (let* ((fresh (not (get-buffer org-learn-ai--chat-buffer)))
         (context (when (or refresh fresh)
                    (org-learn-ai--context))))
    (with-current-buffer (get-buffer-create org-learn-ai--chat-buffer)
      (unless (derived-mode-p 'org-learn-ai-chat-mode)
        (org-learn-ai-chat-mode))
      (when context
        (setq org-learn-ai--chat-context context
              org-learn-ai--chat-context-sent nil))
      (when fresh
        (org-learn-ai--chat-welcome))
      (goto-char (point-max))
      (pop-to-buffer (current-buffer)))))

;;; Flashcard generation.

(defun org-learn-ai--extract-json-array (text)
  "Return the first JSON array in TEXT as a list, or nil.
Tolerates prose around the array: each \"[\" position is tried as
the array start until one parses into a non-empty vector of {q, a}
objects."
  (let ((pos 0)
        found)
    (while (and (null found)
                (setq pos (string-search "[" text pos)))
      (let ((parsed (condition-case nil
                        (json-read-from-string (substring text pos))
                      (error nil))))
        (if (and (vectorp parsed)
                 (> (length parsed) 0)
                 (let ((ok t))
                   (dotimes (i (length parsed))
                     (let ((item (aref parsed i)))
                       (unless (and (listp item)
                                    (stringp (cdr (assq 'q item)))
                                    (stringp (cdr (assq 'a item))))
                         (setq ok nil))))
                   ok))
            (setq found (append parsed nil))
          (setq pos (1+ pos)))))
    found))

(defun org-learn-ai--cards-from-array (items)
  "Convert a parsed JSON array ITEMS to (Q . A) conses.
Items missing a usable q or a are dropped; when nothing usable
remains, `user-error' reports the garbage reply."
  (let (cards)
    (dolist (item items)
      (let ((q (and (listp item) (cdr (assq 'q item))))
            (a (and (listp item) (cdr (assq 'a item)))))
        (when (and (stringp q) (stringp a)
                   (not (string-blank-p q)) (not (string-blank-p a)))
          (push (cons (string-trim q) (string-trim a)) cards))))
    (unless cards
      (user-error "org-learn-ai: no usable {q, a} flashcards in the model reply"))
    (nreverse cards)))

(defun org-learn-ai--format-flashcard (level q a created)
  "Return the Org text of one twosided drill card at LEVEL.
Q is the question (the card title), A the answer; both also land
under the Question and Answer child headings.  CREATED is the
ISO-8601 timestamp for the :CREATED: property."
  (let ((stars (make-string level ?*))
        (child (make-string (1+ level) ?*))
        (indent (make-string (1+ level) ?\s))
        (body (make-string (+ level 2) ?\s)))
    (concat stars " " q " :drill:\n"
            indent ":PROPERTIES:\n"
            indent ":DRILL_CARD_TYPE: twosided\n"
            indent ":CREATED_BY: ai\n"
            indent ":CREATED: " created "\n"
            indent ":END:\n"
            child " Question\n"
            body q "\n"
            child " Answer\n"
            body a "\n")))

(defun org-learn-ai--insert-flashcards (cards)
  "Insert CARDS as drill children of the entry at point.
Each card lands at the end of the current subtree; the number of
inserted cards is returned."
  (org-back-to-heading t)
  (let ((level (1+ (org-current-level)))
        (created (org-learn--iso8601-now)))
    (org-end-of-subtree)
    (unless (bolp)
      (insert "\n"))
    (pcase-dolist (`(,q . ,a) cards)
      (insert (org-learn-ai--format-flashcard level q a created)))
    (length cards)))

(defun org-learn-ai--flashcards-messages (context)
  "Return the request messages asking for flashcards over CONTEXT."
  (list (org-learn-ai--system-message org-learn-ai--flashcards-prompt)
        (org-learn-ai--user-message
         (format "Study context:\n%s\n\nGenerate the flashcard JSON array now."
                 (if (string-empty-p context)
                     "(no subtree at point)"
                   context)))))

;;;###autoload
(defun org-learn-ai-generate-flashcards ()
  "Turn the current subtree into twosided drill flashcards.
Asks the model for a JSON array of {q, a} objects about the
subtree and inserts each pair as a child heading tagged :drill:
with a twosided DRILL_CARD_TYPE, Question and Answer child
headings and CREATED_BY/CREATED properties.  A garbage reply
inserts nothing and raises `user-error'."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-learn-ai-generate-flashcards: not in an Org buffer"))
  (unless (ignore-errors (org-back-to-heading t))
    (user-error "org-learn-ai-generate-flashcards: point is not on a heading"))
  (let* ((context (org-learn-ai--context))
         (reply (org-learn-ai--complete
                 (org-learn-ai--flashcards-messages context)))
         (items (or (org-learn-ai--extract-json-array reply)
                    (user-error "org-learn-ai: no JSON flashcard array in the model reply")))
         (cards (org-learn-ai--cards-from-array items))
         (count (org-learn-ai--insert-flashcards cards)))
    (message "org-learn-ai: inserted %d flashcard%s"
             count (if (= count 1) "" "s"))))

;;; Quiz.

(defun org-learn-ai--grade-quality (reply)
  "Map the grading REPLY to an SM-2 quality: 5, 3 or 1.
The reply must state correct, partly or wrong; \"partly\" and
\"incorrect\" are checked before the bare word \"correct\", so a
\"partly correct\" grade maps to 3 and an \"incorrect\" one to 1.
Without any grade word, `user-error' reports the garbage reply."
  (cond
   ((string-match-p "\\bpartly\\b" reply) 3)
   ((string-match-p "\\b\\(?:incorrect\\|wrong\\)\\b" reply) 1)
   ((string-match-p "\\bcorrect\\b" reply) 5)
   (t (user-error "org-learn-ai-quiz: no correct/partly/wrong grade in the reply"))))

(defun org-learn-ai--quiz-messages (context history turns)
  "Return the quiz request messages.
CONTEXT is the study context, HISTORY the accumulated turns and
TURNS the extra user/assistant message alists for this request."
  (append (list (org-learn-ai--system-message org-learn-ai--tutor-prompt)
                (org-learn-ai--user-message
                 (format "Quiz me about this study context, one question at a time.  Grade each answer with exactly one of the words correct, partly or wrong.\n\nStudy context:\n%s"
                         (if (string-empty-p context)
                             "(no subtree at point)"
                           context))))
          history
          turns))

;;;###autoload
(defun org-learn-ai-quiz ()
  "Quiz the current subtree with the AI tutor, one question at a time.
The model asks a question about the study context, the answer is
read from the minibuffer and the model grades it correct, partly
or wrong (SM-2 quality 5, 3 or 1).  After each grade the command
offers to schedule a review of the entry through
`org-learn-review--process-entry'.  The loop continues until the
user says stop; C-g exits at any point."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-learn-ai-quiz: not in an Org buffer"))
  (unless (ignore-errors (org-back-to-heading t))
    (user-error "org-learn-ai-quiz: point is not on a heading"))
  (let* ((context (org-learn-ai--context))
         (source-buffer (current-buffer))
         (history nil)
         (continue t))
    (while continue
      (let* ((question
              (org-learn-ai--complete
               (org-learn-ai--quiz-messages
                context history
                (list (org-learn-ai--user-message
                       "Ask me ONE quiz question about the study context.  Reply with the question only.")))))
             (answer
              (read-string (format "AI quiz: %s\nYour answer: "
                                   (string-trim question))))
             (grade-turn
              (list (org-learn-ai--assistant-message question)
                    (org-learn-ai--user-message
                     (format "My answer: %s\n\nGrade it with exactly one of the words correct, partly or wrong, then explain briefly."
                             answer))))
             (reply
              (string-trim
               (org-learn-ai--complete
                (org-learn-ai--quiz-messages context history grade-turn))))
             (quality (org-learn-ai--grade-quality reply)))
        (message "org-learn-ai quiz grade: %s" reply)
        (setq history
              (append history
                      (list (org-learn-ai--user-message
                             "Ask me ONE quiz question about the study context.  Reply with the question only.")
                            (org-learn-ai--assistant-message question)
                            (nth 1 grade-turn)
                            (org-learn-ai--assistant-message reply))))
        (when (yes-or-no-p "Schedule a spaced review of this entry? ")
          (with-current-buffer source-buffer
            (org-learn-review--process-entry quality)
            (when (buffer-file-name)
              (save-buffer))))
        (setq continue (yes-or-no-p "Another question? "))))))

(provide 'org-learn-ai)
;;; org-learn-ai.el ends here
