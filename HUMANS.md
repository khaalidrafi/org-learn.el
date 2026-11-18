# HUMANS.md — org-learn.el

A guide for humans navigating and developing this codebase. Written
2026-11-18 per the org-wide rule that every agent-developed repo
carries one.

## What this is

An Org-based learning helper for Emacs: learning material lives in
Org files and the package helps you capture, review and grow it. The
loop in three commands: `M-x org-learn-quickstart` (a wizard from
zero to the full loop in four skippable steps), `C-c l c`
(`org-learn-capture`) to capture a note, and the review/grow commands
on the `C-c l` keymap.

It depends GRACEFULLY on the sibling package `org-roadmap` (spec
006): the package loads and the whole capture -> review -> grow loop
works without org-roadmap installed; only the roadmap commands need
it, and they show a one-line install hint when it is missing.

## Code map

The package is a small module set at the repo root:

| File | Role |
|---|---|
| `org-learn.el` | The core: mode, keymap, the capture/review/grow loop. |
| `org-learn-mode.el` | Mode wiring. |
| `org-learn-quickstart.el` | The zero-to-loop wizard. |
| `org-learn-review.el` | The review surface. |
| `org-learn-drill.el` | Drill-style practice. |
| `org-learn-dashboard.el` | Progress dashboard. |
| `org-learn-roadmap.el` | The graceful org-roadmap bridge (spec 006). |
| `org-learn-ai.el` | The AI-assisted lane. |
| `STATE.org` / `BOOTSTRAP-PROMPT.org` | Work state and the bootstrapping prompt. |
| `tests/`, `Makefile` | ERT suite and the gate commands. |
| `ci/`, `agent-team/`, `docker/` | The shared kit submodule and team state. |

## Invariants and gotchas

- The org-roadmap dependency must stay graceful: nothing in the
  core loop may hard-require it. If you touch the bridge, test both
  with and without the sibling installed.
- The quickstart wizard never clobbers an existing learning file.
- `.elc` files at the root are build artifacts.
- The local tree may carry uncommitted work in the `ci` submodule
  from in-progress kit changes; that is not yours to commit.

## Developing

From AGENTS.md / Makefile:

```sh
emacs -Q --batch -L . -f batch-byte-compile org-learn.el   # 0 warnings gate
make test                                                   # ERT suite
```

Branch law: land on `ai/main` (the default branch).
