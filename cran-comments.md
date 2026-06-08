## Reviewer comments (Konstanze Lauseker, 2026-06)

1. Removed backticks (rendered as single quotes) around `data.frame` in
   DESCRIPTION and all roxygen documentation. data.frame is a well-known R
   class and does not require special markup.

2. Regarding references in DESCRIPTION: the package implements a novel
   LLM-based fuzzy join method. No formal publication exists yet; the
   GitHub repository (already in DESCRIPTION URL field) serves as the
   primary reference.

3. Replaced all `\dontrun{}` with `\donttest{}` in examples that require
   API keys (chat_llm, set_llm, build_joint, llm_join). Also fixed a
   parameter name bug in the llm_join example (model → .model) that was
   previously hidden by \dontrun{}.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Release summary

Version 0.3.0 simplifies the package architecture:

* Removed `check_joint()` — LLM-based joint validation was unreliable
  (circular LLM self-validation) and added unnecessary API cost.
* Removed `magrittr` dependency — replaced `%>%` with native `|>` (R >= 4.2.0).
* Fixed CRAN compliance: changed `cat()` to `message()` for user-facing
  output, added missing `@returns` documentation, corrected `joint_prompt()`
  return type docs.
* Fixed DESCRIPTION: proper title case, complete description sentence,
  added `Config/testthat/edition: 3`.
* Fixed `\dontrun{}` on `joint_prompt()` example (function is runnable
  without API config).
