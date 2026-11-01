#!/usr/bin/env bash
# turn-lanes.sh — per-seat lane wiring for the agent-team chain (LEAD-OWNED,
# agents: do not edit). Extracted from .github/workflows/agent-team.yml by
# the docneural-lead, 2026-11-01: the seg step's run block crossed GitHub's
# 21000-char step-script cap on org-roadmap (run 23 parse failure), so the
# lane tables live here. Source this file, then:
#   seat_batch_config "$SEAT"          # -> BATCH_PROVIDER / BATCH_MODEL / BATCH_CMD
#   seat_direct_run "$SEAT" "$BRIEF"   # direct CLI lane chain; echoes TURN_RESULT
set -u

seat_batch_config() {
  case "$SEAT" in
            principal-engineer)
              BATCH_PROVIDER=opencode
              BATCH_MODEL="openrouter/poolside/laguna-s-2.1:free"
              BATCH_CMD="opencode acp" ;;
            software-engineer|release-engineer)
              BATCH_PROVIDER=goose
              BATCH_MODEL="glm-4.7-flash"
              BATCH_CMD="env GOOSE_PROVIDER=openai GOOSE_MODEL=glm-4.7-flash OPENAI_API_KEY=$ZAI_API_KEY OPENAI_HOST=https://api.z.ai OPENAI_BASE_PATH=/api/paas/v4/chat/completions goose acp" ;;
            quality-assurance-engineer)
              BATCH_PROVIDER=opencode
              BATCH_MODEL="zai/glm-4.7-flash"
              BATCH_CMD="opencode acp" ;;
            platform-engineer)
              BATCH_PROVIDER=qoder
              BATCH_MODEL=""
              BATCH_CMD="env HOME=/usr/local/lib/qoder-home qoder --acp" ;;
            port-engineer)
              # Port Engineer (hired 2026-11-01 00:08 #hq): antling direct
              # lane — ANTLING_API_KEY, burn-verified live (CANDIDATES).
              BATCH_PROVIDER=goose
              BATCH_MODEL="Ling-3.1-flash"
              BATCH_CMD="env GOOSE_PROVIDER=openai GOOSE_MODEL=Ling-3.1-flash OPENAI_API_KEY=$ANTLING_API_KEY OPENAI_HOST=https://api.ant-ling.com OPENAI_BASE_PATH=/v1/chat/completions goose acp" ;;
            *) BATCH_PROVIDER="" ;;
          esac
}

seat_direct_run() {
  local SEAT="$1" BRIEF="$2"
  local BATCH_DONE
  case "$SEAT" in
            principal-engineer)
              # OpenCode body + poolside: OpenCode cannot host the model
              # (absent from its models.dev catalog — demo/01 evidence) and
              # OpenRouter's free quota is exhausted. Attempt order:
              # 1. opencode (in case the catalog/defect is repaired);
              # 2. goose via OpenRouter poolside — SELF-HEALS the moment the
              #    Owner adds ~10 credits (harness/01 item 1/2); OpenRouter
              #    normalizes Goose's developer role server-side (demo/01
              #    evidence: qwen3.8-27b worked via OpenRouter, Groq didn't).
              if opencode run --print-logs --model "openrouter/poolside/laguna-s-2.1:free" "$BRIEF"; then
                echo "TURN_RESULT=opencode run OK"
              elif env GOOSE_PROVIDER=openai GOOSE_MODEL="poolside/laguna-s-2.1:free" \
                   OPENAI_API_KEY="$OPENROUTER_API_KEY" \
                   OPENAI_HOST="https://openrouter.ai/api" \
                   goose run --no-session -t "$BRIEF"; then
                echo "TURN_RESULT=goose run OK (lane: openrouter/poolside@goose — credits landed)"
              elif env GOOSE_PROVIDER=openai GOOSE_MODEL=minimax/minimax-m3.1-flash:free \
                   OPENAI_API_KEY="$OPENROUTER_API_KEY" \
                   OPENAI_HOST="https://openrouter.ai/api" \
                   goose run --no-session -t "$BRIEF"; then
                echo "TURN_RESULT=goose run OK (lane: minimax-m3.1-flash:free@goose — promo hire 05:33Z)"
              elif codebuddy -p "$BRIEF"; then
                echo "TURN_RESULT=codebuddy run OK (lane: fallback codebuddy@cli — poolside dead)"
              elif env HOME=/usr/local/lib/qoder-home \
                   qoder -p "$BRIEF" --output-format json \
                   --permission-mode accept_edits --max-turns 100; then
                echo "TURN_RESULT=qoder run OK (lane: fallback qwen3.8@qoder — poolside dead)"
              else
                echo "TURN_RESULT=BLOCKED (poolside dead on opencode+goose, fallbacks codebuddy+qoder dead too — harness/01)"
              fi
              ;;
            quality-assurance-engineer|software-engineer|release-engineer)
              # LANE (mesh/01 repair, Lead, 2026-10-20 15:45Z): every lane on
              # the hired/fallback brains is dead — Groq rejects the
              # `developer` role Goose sends (400, req_01m5cqbghke9),
              # OpenRouter free-models-per-day exhausted (chain evidence:
              # "Add 10 credits"), OpenCode body headless-defective. The ONE
              # live lane with its own quota: ZAI glm-4.7-flash — same
              # OpenAI-compatible schema. Goose goes through its openai
              # provider with OPENAI_HOST pointed at ZAI. Fallback: the
              # rostered nemotron lane (in case OpenRouter quota resets).
              if env GOOSE_PROVIDER=openai GOOSE_MODEL=glm-4.7-flash \
                   OPENAI_API_KEY="$ZAI_API_KEY" \
                   OPENAI_HOST="https://api.z.ai" \
                   OPENAI_BASE_PATH="/api/paas/v4/chat/completions" \
                   goose run --no-session -t "$BRIEF"; then
                echo "TURN_RESULT=goose run OK (lane: zai/glm-4.7-flash@goose)"
              elif env GOOSE_PROVIDER=openai GOOSE_MODEL=minimax/minimax-m3.1-flash:free \
                   OPENAI_API_KEY="$OPENROUTER_API_KEY" \
                   OPENAI_HOST="https://openrouter.ai/api" \
                   goose run --no-session -t "$BRIEF"; then
                echo "TURN_RESULT=goose run OK (lane: minimax-m3.1-flash:free@goose — OpenRouter promo hire 05:33Z)"
              elif goose run --no-session -t "$BRIEF"; then
                echo "TURN_RESULT=goose run OK (lane: ${GOOSE_MODEL} — OpenRouter quota recovered)"
              elif codebuddy -p "$BRIEF"; then
                echo "TURN_RESULT=codebuddy run OK (lane: fallback codebuddy@cli — zai+openrouter dead)"
              elif env HOME=/usr/local/lib/qoder-home \
                   qoder -p "$BRIEF" --output-format json \
                   --permission-mode accept_edits --max-turns 100; then
                echo "TURN_RESULT=qoder run OK (lane: fallback qwen3.8@qoder — zai+openrouter dead)"
              else
                echo "TURN_RESULT=BLOCKED (all rostered lanes dead: zai+openrouter+codebuddy+qoder — harness/01)"
              fi
              ;;
            platform-engineer)
              # Acting Platform Engineer qwen3.8-flash@qoder — Qoder-on-CI
              # PROOF dispatch (AUTH-READY 2026-10-21: Owner delivered the
              # PAT; repo secret QODER_PERSONAL_ACCESS_TOKEN). The CLI
              # auto-detects CI and uses the PAT — no browser. HOME is
              # pinned to the install dir: the `qoder` launcher resolves
              # its CLI under ITS HOME (proof run 39467719908 failed with
              # the default /home/agent — "Qoder CLI is not installed").
              # Fallback lane per roster: zai/glm-4.7-flash@goose (acting).
              if env HOME=/usr/local/lib/qoder-home \
                   qoder -p "$BRIEF" --output-format json \
                   --permission-mode accept_edits --max-turns 100; then
                echo "TURN_RESULT=qoder run OK (lane: qwen3.8-flash@qoder — body PROVEN on CI)"
              elif env GOOSE_PROVIDER=openai GOOSE_MODEL=glm-4.7-flash \
                   OPENAI_API_KEY="$ZAI_API_KEY" \
                   OPENAI_HOST="https://api.z.ai" \
                   OPENAI_BASE_PATH="/api/paas/v4/chat/completions" \
                   goose run --no-session -t "$BRIEF"; then
                echo "TURN_RESULT=goose run OK (acting lane: zai/glm-4.7-flash@goose — qoder CLI failed)"
              elif env GOOSE_PROVIDER=openai GOOSE_MODEL=minimax/minimax-m3.1-flash:free \
                   OPENAI_API_KEY="$OPENROUTER_API_KEY" \
                   OPENAI_HOST="https://openrouter.ai/api" \
                   goose run --no-session -t "$BRIEF"; then
                echo "TURN_RESULT=goose run OK (lane: minimax-m3.1-flash:free@goose — promo hire 05:33Z)"
              elif codebuddy -p "$BRIEF"; then
                echo "TURN_RESULT=codebuddy run OK (lane: fallback codebuddy@cli — qoder+zai dead)"
              else
                echo "TURN_RESULT=BLOCKED (qoder+zai+codebuddy all failed — harness/01)"
              fi
              ;;
            port-engineer)
              # Port Engineer antling/Ling-3.1-flash (hired 2026-11-01 00:08
              # #hq after the poolside delisting). PRIMARY: antling direct
              # (goose openai provider, same shape as the proven zai lane;
              # OPENAI_HOST = origin, OPENAI_BASE_PATH carries the /v1 path —
              # the openrouter double-/v1 lesson, run 41290677537).
              if env GOOSE_PROVIDER=openai GOOSE_MODEL=Ling-3.1-flash \
                   OPENAI_API_KEY="$ANTLING_API_KEY" \
                   OPENAI_HOST="https://api.ant-ling.com" \
                   OPENAI_BASE_PATH="/v1/chat/completions" \
                   goose run --no-session -t "$BRIEF"; then
                echo "TURN_RESULT=goose run OK (lane: antling/Ling-3.1-flash direct@goose)"
              elif opencode run --print-logs --model "openrouter/antling/ling-3.1-flash" "$BRIEF"; then
                echo "TURN_RESULT=opencode run OK (lane: antling/Ling-3.1-flash@opencode — rostered body)"
              elif env GOOSE_PROVIDER=openai GOOSE_MODEL=minimax/minimax-m3.1-flash:free \
                   OPENAI_API_KEY="$OPENROUTER_API_KEY" \
                   OPENAI_HOST="https://openrouter.ai/api" \
                   goose run --no-session -t "$BRIEF"; then
                echo "TURN_RESULT=goose run OK (lane: minimax-m3.1-flash:free@goose)"
              elif env GOOSE_PROVIDER=openai GOOSE_MODEL=glm-4.7-flash \
                   OPENAI_API_KEY="$ZAI_API_KEY" \
                   OPENAI_HOST="https://api.z.ai" \
                   OPENAI_BASE_PATH="/api/paas/v4/chat/completions" \
                   goose run --no-session -t "$BRIEF"; then
                echo "TURN_RESULT=goose run OK (lane: zai/glm-4.7-flash@goose)"
              elif codebuddy -p "$BRIEF"; then
                echo "TURN_RESULT=codebuddy run OK (lane: fallback codebuddy@cli)"
              else
                echo "TURN_RESULT=BLOCKED (antling+opencode+openrouter+zai+codebuddy all failed — harness/01)"
              fi
              ;;
          esac
}

