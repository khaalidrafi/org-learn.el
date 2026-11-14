EMACS ?= emacs

# Pure Org elisp: org-learn.el bootstraps its own load-path so the
# sibling feature files find each other; -L . covers batch use.  No
# -Q here either, so a distribution's site-lisp is still found (same
# law as the rest of the family).
LOAD = -L .

SRCS = org-learn.el org-learn-review.el org-learn-drill.el \
  org-learn-dashboard.el org-learn-roadmap.el org-learn-ai.el \
  org-learn-mode.el org-learn-quickstart.el

all: compile

compile:
	$(EMACS) --batch $(LOAD) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRCS)

# Suite law: tests/test-org-learn.el is the single entry point -- it
# loads every other suite itself.  The jit-compilation eval mirrors
# the CI ERT step exactly (deterministic on native-comp builds,
# ignored elsewhere).  Run from the repo root; the entry point loads
# its sub-suites relative to default-directory.
test:
	$(EMACS) --batch $(LOAD) \
	  --eval '(setq native-comp-jit-compilation nil)' \
	  -l tests/test-org-learn.el -f ert-run-tests-batch-and-exit

clean:
	rm -f $(SRCS:.el=.elc)

.PHONY: all compile test clean
