# 5-a — Software Engineer — TODO-002 (spec 001 capture) + TODO-003 (spec 002 review scheduler)

Branch: ai/turn-20261101-1 (off ai/main @ 78166c1). No push. agent-team/ untouched.

## RESULT

- TODO-002: DONE — org-learn-capture + org-learn-capture-from-region in
  org-learn.el, spec 001 acceptance tests green.
- TODO-003: DONE — org-learn-review.el (pure org-learn-sm2, process-entry,
  review session, agenda view) + tests/test-org-learn-review.el, spec 002
  acceptance tests green.

## Gate evidence (gate emacs /tmp/emacs-env/bin/emacs 31.1, -Q --batch)

- Byte-compile: `emacs -Q --batch -L . -f batch-byte-compile org-learn.el
  org-learn-review.el` -> exit 0, zero Elisp warnings (only startup noise:
  arch-dependent data dir notice, not a compile diagnostic).
- ERT: `emacs -Q --batch -L . -l tests/test-org-learn.el
  -f ert-run-tests-batch-and-exit` -> exit 0, "Ran 21 tests, 21 results as
  expected, 0 unexpected". Both boundary tests
  (org-learn-keeps-the-org-roadmap-boundary,
  org-learn-review-keeps-the-org-roadmap-boundary) green; org-roadmap is
  never required (grep-verified: comments/tests only).
- checkdoc: org-learn.el, org-learn-review.el, tests/test-org-learn.el,
  tests/test-org-learn-review.el — all CLEAN (batch harness with
  checkdoc-create-error-function collector, no interactive prompts).

## Commits

- 0db5486 feat: spec 001 capture — org-learn-capture + region variant (TODO-002)
- cddce43 feat: spec 002 SM-2 review scheduler + review sessions + agenda (TODO-003)

(Green per commit: commit 1 stands alone at 11/11 tests; commit 2 brings
the suite to 21/21.)

## Fixups applied while making the red-by-design tests pass (no weakening)

1. org-learn.el (impl bug): the minimal file header is now also written when
   the notes file EXISTS but is EMPTY (make-temp-file case) — spec 001
   "header written once" now holds for empty files; idempotence unchanged.
2. tests/test-org-learn.el (latent test bug): `[\s\S]` is a PCRE-ism that in
   Emacs regex matches only "S" and backslash (verified empirically), so the
   ordering assertion could never match any multi-line org content. Replaced
   with the exact any-char equivalent `\(.\\|\n\)`; comment added. Every
   other assertion untouched.
3. tests/test-org-learn-review.el (authored here, corrected against the
   normative SM-2 law before landing): expected values that contradicted the
   spec formulas were fixed — q=1/q=2 ease drop is the flat -0.20 (not
   scaled), and q=4 leaves ease/interval unchanged (delta = 0).
4. org-learn-review.el: round-trip test asserts `^[ \t]*:LEARN_REPS:` etc.
   because org-entry-put writes the properties drawer at the heading's own
   indentation (column 0 under a column-0 heading) — Org API output is the
   law; values are still asserted exactly.
5. Spec-law tightening in org-learn-review--agenda-items: new-items bucket
   (LEARN_REPS missing/0) excludes future-scheduled entries (behavior law:
   due = SCHEDULED <= today). Existing agenda ordering test unchanged.
6. checkdoc-clean pass on all four files: docstring rephrasings (M-x,
   capitalized first lines, quoted symbols), escaped column-0 paren in a
   docstring, proper provide/footer lines for both test files.

## Notes for the next seat

- SM-2 interval for reps >= 2 multiplies the UPDATED ease (ease update is
  applied first) — the test table pins this; keep the two in sync.
- org-entry-put handles "SCHEDULED" specially in this Org (writes the
  planning line); process-entry relies on it — verified on the gate Emacs.
- tests/test-org-learn.el loads tests/test-org-learn-review.el at the end
  (single CI entry); the review test file also requires both features.
