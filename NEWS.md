# llmjoin 0.3.2

## changes
- Added `get_llm()`: shows the configuration written by `set_llm()` — provider, model, endpoint and the config file path — without opening the YAML file. The API key is masked by default: keys longer than 8 characters show only their last 4, shorter keys show `****`, and a blank stored key shows `<empty>`. `get_llm(show_key = TRUE)` is the only way to print or return the full key, and the returned list carries exactly what was printed, so `get_llm()$key` cannot leak the credential into a log or a screenshot. `chat_llm()` still authenticates with the stored key as-is.
- Documented how to keep the API key out of your `.Rhistory`: `set_llm()` and the README now point at passing `Sys.getenv("LLMJOIN_API_KEY")` instead of typing the key as a string literal, which is written verbatim into your history, screenshots and any script you commit. The package's own channels — `set_llm()` progress messages, `chat_llm()` verbose lines and every error text — never contain the stored key, and that property is now pinned by tests.

## fixes
- `set_llm()` now rejects every unusable `key` shape (missing, `NULL`, `""`, `character(0)`, length > 1, `NA`) with a single message that names the argument and says what to check — an empty `key` almost always means the environment variable you passed is unset, so the message points at `nzchar(Sys.getenv("LLMJOIN_API_KEY"))` and at restarting R to load `.Renviron`. These calls previously failed with `'key' must be provided` or leaked an internal error (`condition has length > 1`, `argument is of length zero`) that said nothing about the fix.
- `chat_llm()` now stops before sending a request when the stored key is an empty string (`LLM_key: ''` in the config file). It used to go ahead with a blank credential, so the failure surfaced as the provider's authentication error instead of pointing at the real cause. The new message names the file and says to fill the key in or run `set_llm()`. Reachable mainly after hand-editing the config; `get_llm()` still shows `<empty>` for such a key rather than erroring.

# llmjoin 0.3.1

## changes
- Added DeepSeek as a first-class provider: `set_llm(provider = "deepseek")` defaults to `https://api.deepseek.com/chat/completions` with Bearer authentication and the `deepseek-flash` model.
- README now links the DeepSeek open platform so users can obtain an API key from the setup section.
- Default models updated: OpenAI now defaults to `gpt-6-luna` and Gemini to `gemini-3.8-flash`.
- `provider_parse()` now warns when the LLM reply is truncated — OpenAI/Gemini/DeepSeek `finish_reason == "length"`, Claude `stop_reason == "max_tokens"` — and still returns the parsed text. Increase `.max_tokens` and retry.
- `parse_joint()` now reports keys the LLM never mapped: with `x_keys`/`y_keys` provided, keys absent from the parsed result raise one informational warning per side naming the column, the count and up to 5 sample values. Rows are never dropped by this feedback; actual `NA` keys never count as unmatched (the prompt tells the LLM to leave unmappable cells empty).
- Removed `readr` dependency. Replaced `readr::read_csv()` with base R `utils::read.csv()` in `parse_joint()`, reducing transitive dependencies from ~30 to ~7.
- Added LLM fabrication defense in `parse_joint()`. New optional parameters `x_keys` and `y_keys` validate parsed values against original key columns — fabricated values are dropped with a warning. `build_joint()` and `llm_join()` enable this automatically.

## Fixes
- OpenAI gpt-5+/o-series models (`gpt-6-luna`, `gpt-5.4-mini`, `o4-mini`, ...) now receive `max_completion_tokens` instead of `max_tokens`, and `temperature` is omitted — the official API rejects both parameters for these models. A non-zero `.temperature` on such models warns that it is ignored. All other models, and third-party OpenAI-compatible endpoints routed through `provider = "openai"`, keep the classic `max_tokens` + `temperature` body.
- `llm_join()` now merges on the explicit keys: `key1` first, then `key2`. Previously both merges relied on the intersection of same-named columns, so same-named non-key columns in `x` and `y` silently mis-joined or dropped `y` data, and a column named `key2` in `x` silently matched nothing. The README manual workflow now shows the equivalent explicit-`by` merges.
- `parse_joint()` header detection now recognizes quoted (`"id","month"`) and generic (`value1,value2`, `column1,column2`, ...) header rows and strips them instead of leaking them into the parsed data.
- `tbl2md()` no longer silently renders an empty table for factor vectors.
- `provider_parse()` (claude) now concatenates all text blocks in order; long replies split across multiple blocks are no longer truncated after the first.
- `parse_joint()` no longer flags the string `NA` echoed by the LLM for rows whose key value is actual `NA` (rendered as `NA` in the prompt) as a fabrication. The whitelist is lifted only when the key set contains actual `NA` values; key sets without `NA` keep the strict filtering.
- `chat_llm()` now accepts any non-empty `.message`: elements are coerced via `as.character()`, `NA` elements become blank lines, and a length > 1 vector is pasted into one newline-separated string before sending. Missing, NULL, zero-length or all-blank input raises an error naming `.message` with fix guidance (previously a length > 1 vector leaked an internal "condition has length > 1" error).
- `build_joint()` (and therefore `llm_join()`) validates `key1`/`key2` up front: a misspelled key now errors naming the argument, the offending value and the available columns instead of the opaque "undefined columns selected"; passing a non-data.frame `x`/`y` also errors naming the argument. Validation runs before the LLM call.
- README: the manual workflow example no longer uses the Windows-only `writeClipboard()`; the prompt prints to the console on all platforms.
- Examples that need an API key are now guarded with `\examplesIf` conditions instead of bare `\donttest{}` blocks. `R CMD check --as-cran` executes `\donttest` examples, and these ones wrote a literal placeholder key into your `LLMJOIN.yml` and sent real requests to your configured provider. The guard is `nzchar(Sys.getenv("LLMJOIN_API_KEY"))`: the examples are skipped unless you set that variable, and the `set_llm()` example now reads the key from it, so running it deliberately can no longer overwrite your stored credential. Set `LLMJOIN_API_KEY` before `devtools::check()` to exercise them.

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
