# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build / Test / Check

```bash
# Full test suite
Rscript -e 'devtools::test()'

# Single test file
Rscript -e 'devtools::test_active_file("tests/testthat/test-chat_llm.R")'

# Load package for interactive development
Rscript -e 'devtools::load_all(".")'

# Full R CMD check
Rscript -e 'devtools::check()'

# Generate documentation (after roxygen comment changes)
Rscript -e 'devtools::document()'

# Install from local
Rscript -e 'devtools::install()'
```

Mock patterns for tests: `local_mocked_bindings(chat_llm = function(...) "...", .package = "llmjoin")` — replaces a function binding within the package namespace for the duration of the test. Use for offline tests that don't need a live LLM.

## Architecture

LLM-powered fuzzy join for R data.frames. Three layers:

**Config layer** (`R/connection.R`): `set_llm()` writes `~/.LLMJOIN.yml` (YAML config with provider/URL/key/model). `validate_llm_config()` reads, validates, caches with `VERIFIED` flag. `test_llm_service_minimal()` sends a one-token probe.

**Provider layer** (`R/providers.R`): `.providers` registry maps provider name → base_url, endpoint, default_model, auth_type, thinking config. Four internal helpers: `provider_headers()` (auth), `provider_body()` (request body + thinking injection), `provider_parse()` (response extraction + thinking filtering), `provider_url()` (endpoint construction). Claude uses `x-api-key` auth and `anthropic-version` header; OpenAI/Gemini use Bearer auth.

**API layer** (`R/connection.R`): `chat_llm(.message, .model, .temperature, .max_tokens, .timeout, .verbose, .thinking)` — the single entry point for LLM calls. Default `.thinking = TRUE` enables max reasoning/thinking intensity for all providers (Claude: `thinking: {type: enabled, budget_tokens: 16000}` with temperature removed; OpenAI/Gemini: `reasoning_effort: "high"`). Error handling: 400 + `.thinking=TRUE` suggests `.thinking=FALSE`.

**Join layer** (`R/llmjoin.R`): `tbl2md()` → data.frame to markdown table. `joint_prompt()` → builds matching prompt from two key columns. `build_joint()` → sends prompt to LLM, parses CSV response. `check_joint()` → asks LLM to validate mapping, filters problematic rows. `llm_join()` → end-to-end fuzzy join orchestrating build_joint + optional check_joint + merge.

**Utilities** (`R/utils.R`): pipe re-export, `%||%` null-coalesce, httr/jsonlite imports, global variable declarations.

## Key patterns

- **Provider extensibility**: Add a new provider by adding one entry to `.providers` list + cases in the four `provider_*()` functions.
- **Thinking**: Always-on by default (no model name matching). User disables with `.thinking = FALSE` if model returns 400. Claude parse iterates content blocks filtering `type == "thinking"`, keeps only `type == "text"`.
- **Config caching**: Once validated, config gets `VERIFIED: true` written back to YAML, so subsequent calls skip network checks.
- **Test gating**: `skip_if_no_llm()` (in `tests/testthat/helper.R`) skips integration tests when no LLM service is configured.

## Workflow

This repo follows BDD/TDD. When implementing:

1. Confirm Given-When-Then scenario matrix with user (normal/error/boundary cases).
2. Generate test file skeleton with import/fixture/mock stubs, test bodies left as G-W-T comments.
3. Iterate with user to confirm scenarios, then split by test function into independent sub-tasks.
4. Sub-agents fill in test code + implementation code, verify own tests pass, then run full regression.
5. Review sub-agent code: all tests pass + all G-W-T covered + no side effects. Fix or reject if not.
6. Max 4 concurrent sub-agents per round.
