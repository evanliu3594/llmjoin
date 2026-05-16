# llmjoin 0.2.2

## Breaking changes
- `build_joint()` now takes a raw LLM response string instead of data.frames.
  Signature changed from `build_joint(x, y, key1, key2, ...)` to
  `build_joint(llm_response, key1, key2)`. The LLM call and prompt construction
  are now orchestrated by `llm_join()`.

## New features
- `llm_join()` explicitly orchestrates the full pipeline: `joint_prompt()` →
  `chat_llm()` → `build_joint()`.

## Fixes
- Config file moved from `~/.LLMJOIN.yml` to `tools::R_user_dir("llmjoin",
  "config")`, complying with CRAN policy on writing to the user home directory.
- Added missing `@examples` to `set_llm()` and `check_joint()`.
- Set `LazyData: false` in DESCRIPTION to avoid CRAN NOTE about missing data
  directory.
- Removed deprecated `%>%` re-export, `validate_llm_config()`, and
  `test_llm_service_minimal()`.

# llmjoin 0.2.1

## New features
- Thinking/reasoning support for all providers. `chat_llm()` enables max
  reasoning intensity by default. Claude: `thinking: {type: enabled,
  budget_tokens: 16000}`. OpenAI / Gemini: `reasoning_effort: "high"`.
- Provider layer (`R/providers.R`) with pluggable `.providers` registry
  supporting OpenAI, Claude (Anthropic), and Gemini. Four internal helpers
  handle per-provider auth, request body, response parsing, and URL construction.
- Config caching: `validate_llm_config()` writes `VERIFIED: true` into the YAML
  after a successful probe, skipping redundant network checks.
- `test_llm_service_minimal()` for one-token connectivity probing.
- `%||%` null-coalescing operator.

## Fixes
- Replaced tidy-R dependency calls with native R functions, removing `magrittr`
  from Imports.
- Fixed `chat_llm()` input validation: missing `.message` now properly errors.
- Config validation errors now include the file path and suggest `set_llm()`.
- Modularized auth header construction per provider (`provider_headers()`).

## Internal
- Renamed `R/network-utils.R` → `R/connection.R`, `R/main.R` → `R/llmjoin.R`.
- Added `R/utils.R` with pipe re-export, `%||%`, httr/jsonlite imports.
- Added comprehensive test infrastructure using `testthat` (>= 3.0.0) with
  `local_mocked_bindings` mock patterns for offline testing.
- Added `CLAUDE.md` for AI-assisted development guidance.

# llmjoin 0.2.0

## New features
- `check_joint()` for LLM-based joint validation. Asks the LLM to identify
  problematic mappings and filters them from the result.
- `joint_prompt()` for generating structured matching prompts from two key
  columns.
- `tbl2md()` for converting data.frames to markdown tables.
- `build_joint()` for constructing fuzzy-join mapping tables via LLM.
- `llm_join()` for end-to-end fuzzy join: build_joint + optional check_joint +
  merge.
- `set_llm()` for configuring LLM service credentials (provider, URL, key,
  model) stored in `~/.LLMJOIN.yml`.
- `chat_llm()` as the single entry point for LLM API calls, supporting OpenAI,
  Claude, and Gemini providers.

# llmjoin 0.1.0

- Initial package structure with basic LLM calling capabilities.
