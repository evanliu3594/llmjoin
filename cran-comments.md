## R CMD check results

0 errors | 0 warnings | 0 notes

## Release summary

Version 0.3.0 simplifies the package architecture:

* Removed `check_joint()` — LLM-based joint validation was unreliable (circular LLM self-validation) and added unnecessary API cost.
* Removed `magrittr` dependency — replaced `%>%` with native `|>` (R >= 4.2.0).
* Fixed CRAN compliance: changed `cat()` to `message()` for user-facing output, added missing `@returns` documentation, corrected `joint_prompt()` return type docs.
* Fixed DESCRIPTION: proper title case, complete description sentence, added `Config/testthat/edition: 3`.
* Fixed `\dontrun{}` on `joint_prompt()` example (function is runnable without API config).
