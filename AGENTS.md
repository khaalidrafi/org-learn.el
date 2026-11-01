# AGENTS.md — org-learn.el local law

Global law (binds everywhere):
https://github.com/khaalidrafi/agent-team-ci/blob/main/AGENTS.md

This file adds project specifics only; it repeats nothing from the
global law.

## Project

- org-learn.el is an Org-based learning helper for Emacs.
- Dependency (Owner order 2026-11-01 18:28Z #hq, supersedes the old
  boundary law): org-learn DEPENDS on org-roadmap for planning and
  progress (specs/006).  The dependency is real but graceful: core
  org-learn loads without org-roadmap present; roadmap entry points
  (require 'org-roadmap nil t) and user-error with an install hint
  when missing.  Tests stub org-roadmap via tests/support/.

## Conventions

- Every Elisp file carries `-*- lexical-binding: t; -*-`; one feature
  per file; all public symbols use the `org-learn-` prefix.
- ERT tests live in `tests/`; new features land with tests.
- Header style passes `M-x checkdoc` (Author: Khalid,
  GPL-3.0-or-later).
- Working branch: ai/main.
