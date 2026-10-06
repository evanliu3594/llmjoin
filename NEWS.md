# llmjoin 0.3.1

## changes
- Removed `readr` dependency. Replaced `readr::read_csv()` with base R `utils::read.csv()` in `parse_joint()`, reducing transitive dependencies from ~30 to ~7.
- Added LLM fabrication defense in `parse_joint()`. New optional parameters `x_keys` and `y_keys` validate parsed values against original key columns — fabricated values are dropped with a warning. `build_joint()` and `llm_join()` enable this automatically.

## Fixes
- `llm_join()` now merges on the explicit keys: `key1` first, then `key2`. Previously both merges relied on the intersection of same-named columns, so same-named non-key columns in `x` and `y` silently mis-joined or dropped `y` data, and a column named `key2` in `x` silently matched nothing. The README manual workflow now shows the equivalent explicit-`by` merges.
- `parse_joint()` header detection now recognizes quoted (`"id","month"`) and generic (`value1,value2`, `column1,column2`, ...) header rows and strips them instead of leaking them into the parsed data.
- `tbl2md()` no longer silently renders an empty table for factor vectors.
- `provider_parse()` (claude) now concatenates all text blocks in order; long replies split across multiple blocks are no longer truncated after the first.
- `parse_joint()` no longer flags the string `NA` echoed by the LLM for rows whose key value is actual `NA` (rendered as `NA` in the prompt) as a fabrication. The whitelist is lifted only when the key set contains actual `NA` values; key sets without `NA` keep the strict filtering.
- `chat_llm()` now accepts any non-empty `.message`: elements are coerced via `as.character()`, `NA` elements become blank lines, and a length > 1 vector is pasted into one newline-separated string before sending. Missing, NULL, zero-length or all-blank input raises an error naming `.message` with fix guidance (previously a length > 1 vector leaked an internal "condition has length > 1" error).
- `build_joint()` (and therefore `llm_join()`) validates `key1`/`key2` up front: a misspelled key now errors naming the argument, the offending value and the available columns instead of the opaque "undefined columns selected"; passing a non-data.frame `x`/`y` also errors naming the argument. Validation runs before the LLM call.
- README: the manual workflow example no longer uses the Windows-only `writeClipboard()`; the prompt prints to the console on all platforms.

# llmjoin 0.3.0

## changes
- Removed `check_joint()` — LLM-based joint validation was unreliable (circular LLM self-validation) and its parsing logic was fragile.
- Removed `magrittr` dependency. The `%>%` re-export is no longer provided. Use the native R pipe `|>` (available since R 4.1.0).

## Fixes
- Changed `cat()` to `message()` for user-facing output in `set_llm()` and `chat_llm()`, complying with CRAN best practices.
- Removed unconditional `cat()` preview from `parse_joint()`.
- Fixed `joint_prompt()` `@returns` tag (incorrectly stated `data.frame`; now correctly states `character string`).
- Added missing `@returns` tag for `set_llm()`.


# llmjoin 0.2.2

## changes
- remove `thinking` mode support, the author does not have the energy to set up individual thinking adaptation interfaces for the countless LLMs.
- `build_joint()` now takes a raw LLM response string instead of data.frames. Signature changed from `build_joint(x, y, key1, key2, ...)` to `build_joint(llm_response, key1, key2)`. The LLM call and prompt construction are now orchestrated by `llm_join()`.

## New features
- `llm_join()` explicitly orchestrates the full pipeline: `joint_prompt()` → `chat_llm()` → `build_joint()`.

## Fixes
- Config file moved from `~/.LLMJOIN.yml` to `tools::R_user_dir("llmjoin", "config")`,  complying with CRAN policy on writing to the user home directory.
- Added missing `@examples` to `set_llm()` and `check_joint()`.
- Set `LazyData: false` in DESCRIPTION to avoid CRAN NOTE about missing data
  directory.
- Removed deprecated `%>%` re-export, `validate_llm_config()`, and
  `test_llm_service_minimal()`.

# llmjoin 0.2.1

## New features
- Thinking/reasoning support for all providers. `chat_llm()` enables max reasoning intensity by default. Claude: `thinking: {type: enabled, budget_tokens: 16000}`. OpenAI / Gemini: `reasoning_effort: "high"`.
- Provider layer (`R/providers.R`) with pluggable `.providers` registry supporting OpenAI, Claude (Anthropic), and Gemini. Four internal helpers handle per-provider auth, request body, response parsing, and URL construction.
- Config caching: `validate_llm_config()` writes `VERIFIED: true` into the YAML after a successful probe, skipping redundant network checks.
- `test_llm_service_minimal()` for one-token connectivity probing.
- `%||%` null-coalescing operator.

## Fixes
- Replaced tidy-R dependency calls with native R functions, removing `magrittr` from Imports.
- Fixed `chat_llm()` input validation: missing `.message` now properly errors.
- Config validation errors now include the file path and suggest `set_llm()`.
- Modularized auth header construction per provider (`provider_headers()`).

## Internal
- Renamed `R/network-utils.R` → `R/connection.R`, `R/main.R` → `R/llmjoin.R`.
- Added `R/utils.R` with pipe re-export, `%||%`, httr/jsonlite imports.
- Added comprehensive test infrastructure using `testthat` (>= 3.0.0) with `local_mocked_bindings` mock patterns for offline testing.

# llmjoin 0.2.0

## New features
- `check_joint()` for LLM-based joint validation. Asks the LLM to identify problematic mappings and filters them from the result.
- `joint_prompt()` for generating structured matching prompts from two key columns.
- `tbl2md()` for converting data.frames to markdown tables.
- `build_joint()` for constructing fuzzy-join mapping tables via LLM.
- `llm_join()` for end-to-end fuzzy join: build_joint + optional check_joint + merge.
- `set_llm()` for configuring LLM service credentials (provider, URL, key, model) stored in `~/.LLMJOIN.yml`.
- `chat_llm()` as the single entry point for LLM API calls, supporting OpenAI, Claude, and Gemini providers.

# llmjoin 0.1.0

- Initial package structure with basic LLM calling capabilities.
