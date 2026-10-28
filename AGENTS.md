# AGENTS.md — org-learn.el local law

Global law (binds everywhere):
https://github.com/khaalidrafi/agent-team-ci/blob/main/AGENTS.md

This file adds project specifics only; it repeats nothing from the
global law.

## Project

- org-learn.el is an Org-based learning helper for Emacs.
- Dependency: org-roadmap (a package under the khaalidrafi profile) is
  not completed or published yet.  It stays at the boundary: declared in
  the package header, never hard-required in code until its API lands.
  Do not block on it.

## Conventions

- Every Elisp file carries `-*- lexical-binding: t; -*-`; one feature
  per file; all public symbols use the `org-learn-` prefix.
- ERT tests live in `tests/`; new features land with tests.
- Header style passes `M-x checkdoc` (Author: Khalid,
  GPL-3.0-or-later).
- Working branch: ai/main.
