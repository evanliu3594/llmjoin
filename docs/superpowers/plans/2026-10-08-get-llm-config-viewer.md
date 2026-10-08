# get_llm() 配置查看功能（0.3.2）实施计划

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 新增 `get_llm()`，让用户在不打开 YAML 文件的前提下查看当前 LLM 配置，API key 默认脱敏。

**Architecture:** 把 `chat_llm()` 内联的配置读取段（R/connection.R:125-158）提取为私有 `.read_config()`，`chat_llm()` 与新的 `get_llm()` 共用；脱敏由私有 `.mask_key()` 完成，只作用于展示与 `get_llm()` 返回值，请求路径始终使用明文 key。

**Tech Stack:** R >= 4.2.0，base R + `config`（读 YAML）+ `httr`/`jsonlite`；测试 testthat edition 3（`describe`/`it`）+ `withr::local_envvar(R_USER_CONFIG_DIR=...)` 隔离配置目录。

**Spec:** 本会话用户指令——key 默认脱敏、纳入 0.3.2、按 GWT 矩阵写测试并执行到 tag。

## Global Constraints

- 依赖极简：Imports 只有 httr / jsonlite / config，不得新增（根 AGENTS.md P0.1）。
- 用户可见输出只用 `message()` / `warning()` / `stop()`，禁止 `cat()`（根 P0.1）。
- 不向用户主目录写文件；配置只经 `tools::R_user_dir("llmjoin", "config")`（根 P0.1）。
- base R 优先：管道 `|>`，匿名函数 `\(x)`，禁止 tidyverse / magrittr / readr（根 P0.5）。
- 函数命名 snake_case；内部辅助函数 `.` 前缀且 `@noRd`（根 P3）。
- `stop()` 错误信息必须给修复指引（根 P2.2）。
- roxygen 变更后跑 `devtools::document()`，`man/` 不手改（根 P2.1）。
- 每个用户可见变更写入 NEWS.md 对应版本段（根 P2.2）。
- 测试红线：`testthat::test_local()` 全绿，0 失败；测试不得触网，全部打桩（根 P0.2）。
- 断言含正则元字符（`|` `.` `(` `\`）的字符串用 `fixed = TRUE`；反向断言用 `expect_false(grepl(...))`，本机 `expect_match(invert=TRUE)` 会报错（tests/AGENTS.md）。
- 需要凭据的示例统一用 `@examplesIf nzchar(Sys.getenv("LLMJOIN_API_KEY"))` 守卫，不得移除（根 §已拍板口径⑦）。
- 版本号 0.3.2；GitHub tag 命名沿用 `v0.3.2`（现存 tag 为 `v0.3.1`）。
- 跑 R 命令前清 locale 变量：`env -u LC_ALL -u LANG -u LC_CTYPE -u LC_COLLATE`（根 §常用命令）。

---

## 设计定案（写死，实现时不再自由发挥）

### 公开接口

```r
get_llm(show_key = FALSE)
```

- 返回值：**不可见**（`invisible()`）的 named list，五个元素，顺序固定：
  `provider` / `url` / `model` / `key` / `config_path`，全部 character(1)。
- `key` 元素：默认是**脱敏后**的字符串；只有 `show_key = TRUE` 才是明文。
  「看到什么就拿到什么」——脚本里 `get_llm()$key` 意外打印/写日志不会泄露明文。
- 打印：五（脱敏时六）条 `message()` 行，与 `set_llm()` 的输出风格一致。

### 脱敏规则 `.mask_key(key)`

| 输入 | 输出 | 理由 |
|---|---|---|
| `""`（手改 YAML 可造出，`chat_llm` 的 `is.null` 校验放行空串） | `"<empty>"` | 让用户看见真实故障 |
| `nchar(key) <= 8` | `"****"` | 短 key 不做部分泄露（泄露尾巴会过半） |
| `nchar(key) > 8` | `paste0("****", substr(key, nchar(key) - 3L, nchar(key)))` | 只露末 4 位，够区分多把 key |

示例：`"sk-1234567890abcdef"`(19) → `"****cdef"`；`"test-key"`(8) → `"****"`。

### 打印格式（message 行，逐条）

```
LLM config read from `<config_path>`.
  Provider: openai
  Model: gpt-6-luna
  URL: https://api.openai.com/v1/chat/completions
  Key: ****cdef
  (pass show_key = TRUE to print the full key)     <- 仅脱敏分支输出
```

### `.read_config()` 契约（私有，行为与现 chat_llm 内联段逐字一致）

校验顺序不得改变：文件不存在 → YAML 解析失败 → 缺 URL/key → provider 未知。
返回 `list(provider, url, key, model, config_path)`，其中
`model = cfg$LLM_model %||% .providers[[provider]]$default_model`（把原来散在
chat_llm 的默认模型兜底收进来，`get_llm()` 因此显示的是生效模型）。

### GWT 场景矩阵

| # | Given | When | Then |
|---|---|---|---|
| A1 | 临时配置目录，`set_llm(provider="openai", key="sk-1234567890abcdef", model="test-model")` | `get_llm()` | 返回 list 的 provider/url/model/config_path 为明文期望值；message 含 `Provider: openai` |
| A2 | 同 A1 | `get_llm()` | 返回 `key == "****cdef"`；message 文本不含明文 key，且含提示行 `show_key = TRUE` |
| A3 | 同 A1 | `get_llm(show_key = TRUE)` | 返回 `key` 为明文；message 含明文 key，且不含提示行 |
| A4 | 同 A1 | `get_llm()` | 返回值不可见（`capture.output` stdout 为空）——只有 message 输出 |
| A5 | 同 A1 | 连续两次 `get_llm()` | 配置文件内容逐字不变（无写副作用） |
| B1 | 手改配置 key=`"test-key"`(8) | `get_llm()` | `key == "****"` |
| B2 | key=`"123456789"`(9) | `get_llm()` | `key == "****6789"` |
| B3 | key ∈ {`"a"`,`"abcd"`,`"12345678"`} | `get_llm()` | 每个都 `== "****"` |
| B4 | 手改配置 `LLM_key: ''` | `get_llm()` | `key == "<empty>"`，message 含 `<empty>` |
| C1 | 全新临时配置目录，未写配置 | `get_llm()` | 报错含 `LLM service not configured` 且点名 `set_llm()` |
| C2 | 手改配置为非法 YAML（制表符缩进） | `get_llm()` | 报错含 `Invalid config file`、含配置路径、含 `Use set_llm() to reconfigure.` |
| C3 | 手改配置缺 `LLM_key` 行 | `get_llm()` | 报错含 `Config is missing URL or key` |
| C4 | 手改配置 `LLM_provider: 'nope'` | `get_llm()` | 报错含 `Unknown provider 'nope' in config` |
| D1 | `get_llm(show_key = "yes")` | | 报错点名 `'show_key'`，含 `TRUE` 指引 |
| D2 | `get_llm(show_key = NA)` | | 同 D1 |
| D3 | `get_llm(show_key = c(TRUE, TRUE))` | | 同 D1 |
| D4 | 手改配置缺 `LLM_model` 行，provider=openai | `get_llm()` | `model == "gpt-6-luna"`（注册表默认） |
| E1 | 无配置目录 | `chat_llm("hi")` | 仍报 `LLM service not configured`（重构前锁死、重构后不回归） |
| E2 | 合法配置 + httr/provider_headers 全打桩 | `chat_llm("hi")` | `provider_headers` 收到的 key 是**明文**（脱敏只在展示层） |
| E3 | 非法 YAML | `chat_llm("hi")` | 仍报 `Invalid config file` + reconfigure 指引 |

E1/E3 属 Task 1 的「重构前特征测试」，其余在 `test-get_llm.R`（Task 3/4）。

---

### Task 1: 重构前锁死 chat_llm 的配置读取行为（特征测试）

**Files:**
- Modify: `tests/testthat/test-chat_llm.R`（在文件末尾的顶层 `describe("chat_llm", ...)` 块内追加一个子 describe）

**Interfaces:**
- Consumes: 现行 `chat_llm()`（R/connection.R:95）、`tools::R_user_dir()`
- Produces: 无新接口；产出的是 Task 2 重构的回归护栏

- [ ] **Step 1: 追加失败/通过性测试**

在 `tests/testthat/test-chat_llm.R` 顶层 `describe("chat_llm", { ... })` 内、
`describe("message coercion ...")` 块之后插入：

```r
  describe("config read errors (characterization before the .read_config() refactor)", {

    it("errors when no config file exists", {
      # Given: 一个全新的临时配置目录（无任何 LLMJOIN.yml）
      # When:  chat_llm(.message = "hi")
      # Then:  报错点名 set_llm()；不打 HTTP 桩即证明未走到网络层
      withr::local_envvar(R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"))
      expect_error(
        chat_llm(.message = "hi"),
        "LLM service not configured. Use `set_llm()` to set up your API key and endpoint."
      )
    })

    it("errors when the config file is not valid YAML", {
      # Given: 手工写入制表符缩进的非法 YAML
      # When:  chat_llm(.message = "hi")
      # Then:  报错含 'Invalid config file'、配置路径、reconfigure 指引
      withr::local_envvar(R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"))
      cfg_dir <- tools::R_user_dir("llmjoin", "config")
      dir.create(cfg_dir, showWarnings = FALSE, recursive = TRUE)
      writeLines("\t- not: a mapping:\tabc", file.path(cfg_dir, "LLMJOIN.yml"))
      err <- tryCatch(chat_llm(.message = "hi"), error = function(e) conditionMessage(e))
      expect_match(err, "Invalid config file", fixed = TRUE)
      expect_match(err, "LLMJOIN.yml", fixed = TRUE)
      expect_match(err, "Use set_llm() to reconfigure.", fixed = TRUE)
    })

  })
```

> 非法 YAML 的载荷（`"\t- not: a mapping:\tabc"`）是**待验证假设**：Step 2 若该
> 载荷未让 `config::get()` 抛错，就换成缺 `default:` 顶层键的载荷重试；确认后把
> 最终载荷写回本计划与测试注释。

- [ ] **Step 2: 运行新测试确认通过（旧实现下应当已通过）**

Run: `env -u LC_ALL -u LANG -u LC_CTYPE -u LC_COLLATE Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-chat_llm.R")'`
Expected: FAIL 0。若 `Invalid config file` 断言 FAIL，按上条注记换载荷后重跑。

- [ ] **Step 3: 全量回归**

Run: `env -u LC_ALL -u LANG -u LC_CTYPE -u LC_COLLATE Rscript -e 'testthat::test_local(".")'`
Expected: 全绿；记录 PASS 断言数（基线 281 + 本次新增）。

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test-chat_llm.R
git commit -m "test: pin chat_llm config read errors before extracting the shared reader"
```

---

### Task 2: 提取私有 `.read_config()` 并让 chat_llm 使用

**Files:**
- Modify: `R/connection.R`（在 `set_llm()` 之后、`chat_llm()` 之前插入新函数；改写 `chat_llm()` 的 124-159 行段）

**Interfaces:**
- Consumes: `tools::R_user_dir()`、`config::get()`、`.providers`（R/providers.R:2）、`%||%`（R/utils.R）
- Produces: `.read_config()` → `list(provider=character, url=character, key=character, model=character, config_path=character)`

- [ ] **Step 1: 写实现**

在 `R/connection.R` 中 `set_llm()` 定义之后插入：

```r
#' Read and validate the stored LLM configuration
#' @noRd
.read_config <- function() {
  config_dir <- tools::R_user_dir("llmjoin", "config")
  config_path <- file.path(config_dir, "LLMJOIN.yml")
  if (!file.exists(config_path)) {
    stop(
      "LLM service not configured. Use `set_llm()` to set up your API key and endpoint."
    )
  }
  raw <- tryCatch(
    config::get(file = config_path, use_parent = FALSE),
    error = \(e) {
      stop(
        "Invalid config file (",
        config_path,
        "): ",
        e$message,
        "\nUse set_llm() to reconfigure."
      )
    }
  )
  if (is.null(raw$LLM_URL) || is.null(raw$LLM_key)) {
    stop("Config is missing URL or key. Use set_llm() to reconfigure.")
  }

  provider <- raw$LLM_provider %||% "openai"
  if (!provider %in% names(.providers)) {
    stop(
      "Unknown provider '",
      provider,
      "' in config. Run set_llm() to reconfigure."
    )
  }

  list(
    provider = provider,
    url = raw$LLM_URL,
    key = raw$LLM_key,
    model = raw$LLM_model %||% .providers[[provider]]$default_model,
    config_path = config_path
  )
}
```

- [ ] **Step 2: 改写 `chat_llm()` 的消费端**

把 `chat_llm()` 里从 `# Load and validate config` 到
`url <- LLMJOIN_CONFIG$LLM_URL`（原 R/connection.R:124-159）整段替换为：

```r
  # Load and validate config
  cfg <- .read_config()
  provider <- cfg$provider
  model <- .model %||% cfg$model
  url <- cfg$url
```

并把后续两处 `LLMJOIN_CONFIG$LLM_key` 改为 `cfg$key`（原 173 行
`provider_headers(provider, LLMJOIN_CONFIG$LLM_key)`）。
其余逻辑（`.verbose` 提示、body、headers、错误信息）一字不动。

- [ ] **Step 3: 全量回归（含 Task 1 的特征测试）**

Run: `env -u LC_ALL -u LANG -u LC_CTYPE -u LC_COLLATE Rscript -e 'testthat::test_local(".")'`
Expected: 全绿，断言数与 Task 1 Step 3 相同（纯重构不增测试）。

- [ ] **Step 4: Commit**

```bash
git add R/connection.R
git commit -m "refactor: extract the config read path into a private .read_config()"
```

---

### Task 3: `get_llm()` 正常路径与脱敏（A1-A5, B1-B4, D4）

**Files:**
- Create: `tests/testthat/test-get_llm.R`（先写，确认失败）
- Modify: `R/connection.R`（新增 `.mask_key()` 与导出的 `get_llm()`）

**Interfaces:**
- Consumes: `.read_config()`（Task 2）
- Produces: `get_llm(show_key = FALSE)` → invisible named list `provider/url/model/key/config_path`；`.mask_key(key)` → character(1)

- [ ] **Step 1: 写失败测试**

新建 `tests/testthat/test-get_llm.R`：

```r
# TDD/BDD: get_llm() — read the stored LLM config without opening the YAML file
# Contract: returns an invisible named list (provider, url, model, key, config_path)
#   and reports the same fields through message(). The API key is masked by
#   default; show_key = TRUE is the only way to get the plaintext back.
# Contract: masking rule (.mask_key) — empty -> "<empty>"; nchar <= 8 -> "****";
#   nchar > 8 -> "****" + last 4 characters.
# Contract: config read failures reuse the exact chat_llm() error texts.
# All tests run against a temp R_USER_CONFIG_DIR (never the real user config)
# and touch no network.

# Helper: fresh config dir for this test only; returns the LLMJOIN.yml path.
.local_config_dir <- function() {
  withr::local_envvar(
    R_USER_CONFIG_DIR = tempfile(pattern = "llmjoin-test-"),
    .local_envir = parent.frame()
  )
  file.path(tools::R_user_dir("llmjoin", "config"), "LLMJOIN.yml")
}

# Helper: run expr, muffling messages; returns list(value = <result>, messages = <one string>)
.messages_of <- function(expr) {
  collected <- character()
  value <- withCallingHandlers(
    expr,
    message = function(m) {
      collected <<- c(collected, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, messages = paste(collected, collapse = ""))
}

describe("get_llm", {

  describe("normal read (masked by default)", {

    it("reports provider, model, url and config path", {
      # Given: set_llm(provider="openai", key="sk-1234567890abcdef", model="test-model")
      # When:  get_llm()
      # Then:  list fields are the stored values; messages name the provider
      cfg_path <- .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      out <- .messages_of(get_llm())
      expect_identical(out$value$provider, "openai")
      expect_identical(out$value$model, "test-model")
      expect_identical(
        out$value$url,
        "https://api.openai.com/v1/chat/completions"
      )
      expect_identical(out$value$config_path, cfg_path)
      expect_match(out$messages, "Provider: openai", fixed = TRUE)
      expect_match(out$messages, "Model: test-model", fixed = TRUE)
    })

    it("masks the key in the value and in the messages", {
      # Given: a 19-character key
      # When:  get_llm()
      # Then:  value$key == "****cdef"; the plaintext appears nowhere, and the
      #   hint about show_key is shown
      cfg_path <- .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      out <- .messages_of(get_llm())
      expect_identical(out$value$key, "****cdef")
      expect_match(out$messages, "Key: ****cdef", fixed = TRUE)
      expect_false(grepl("sk-1234567890abcdef", out$messages, fixed = TRUE))
      expect_match(out$messages, "show_key = TRUE", fixed = TRUE)
      expect_false(grepl(cfg_path, out$value$key, fixed = TRUE))
    })

    it("returns the plaintext key only when show_key = TRUE", {
      # Given: the same stored config
      # When:  get_llm(show_key = TRUE)
      # Then:  value$key is the full key, the messages carry it, and the
      #   show_key hint is absent
      cfg_path <- .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      out <- .messages_of(get_llm(show_key = TRUE))
      expect_identical(out$value$key, "sk-1234567890abcdef")
      expect_match(out$messages, "sk-1234567890abcdef", fixed = TRUE)
      expect_false(grepl("show_key = TRUE", out$messages, fixed = TRUE))
    })

    it("prints nothing but the messages (returns invisibly)", {
      # Given: the same stored config
      # When:  get_llm() is captured on standard output
      # Then:  stdout stays empty
      cfg_path <- .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      stdout <- capture.output(get_llm(), type = "output")
      expect_identical(length(stdout), 0L)
    })

    it("does not modify the config file", {
      # Given: the same stored config
      # When:  get_llm() twice
      # Then:  LLMJOIN.yml content is byte-identical before and after
      cfg_path <- .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      before <- paste(readLines(cfg_path, warn = FALSE), collapse = "\n")
      suppressMessages(get_llm())
      suppressMessages(get_llm())
      after <- paste(readLines(cfg_path, warn = FALSE), collapse = "\n")
      expect_identical(before, after)
    })

    it("falls back to the provider default model when the config omits it", {
      # Given: a hand-written config with no LLM_model line
      # When:  get_llm()
      # Then:  model is the .providers registry default for openai
      cfg_path <- .local_config_dir()
      dir.create(dirname(cfg_path), showWarnings = FALSE, recursive = TRUE)
      writeLines(
        paste0(
          "default:\n",
          "  LLM_provider: 'openai'\n",
          "  LLM_URL: 'https://api.openai.com/v1/chat/completions'\n",
          "  LLM_key: 'sk-1234567890abcdef'"
        ),
        cfg_path
      )
      out <- .messages_of(suppressMessages(get_llm()))
      expect_identical(out$value$model, .providers$openai$default_model)
      expect_match(out$messages, "Provider: openai", fixed = TRUE)
    })

  })

  describe("key masking boundaries", {

    it("fully masks keys of 8 characters or fewer", {
      # Given: keys "test-key"(8), "1234567"(7), "a"(1)
      # When:  get_llm() for each
      # Then:  masked value is exactly "****" (no tail leaked)
      for (k in c("test-key", "1234567", "a")) {
        cfg_path <- .local_config_dir()
        dir.create(dirname(cfg_path), showWarnings = FALSE, recursive = TRUE)
        writeLines(
          paste0(
            "default:\n  LLM_provider: 'openai'\n",
            "  LLM_URL: 'https://api.openai.com/v1/chat/completions'\n",
            "  LLM_key: '", k, "'\n  LLM_model: 'test-model'"
          ),
          cfg_path
        )
        out <- .messages_of(suppressMessages(get_llm()))
        expect_identical(out$value$key, "****")
      }
    })

    it("reveals only the last 4 characters of a 9-character key", {
      # Given: key "123456789"(9) — the first length that may show a tail
      # When:  get_llm()
      # Then:  masked value is "****6789"
      cfg_path <- .local_config_dir()
      dir.create(dirname(cfg_path), showWarnings = FALSE, recursive = TRUE)
      writeLines(
        paste0(
          "default:\n  LLM_provider: 'openai'\n",
          "  LLM_URL: 'https://api.openai.com/v1/chat/completions'\n",
          "  LLM_key: '123456789'\n  LLM_model: 'test-model'"
        ),
        cfg_path
      )
      out <- .messages_of(suppressMessages(get_llm()))
      expect_identical(out$value$key, "****6789")
    })

    it("marks a blank stored key as <empty>", {
      # Given: hand-edited config with LLM_key: '' (chat_llm's is.null guard
      #   lets an empty string through, so this state is reachable)
      # When:  get_llm()
      # Then:  value$key == "<empty>" and the messages show it
      cfg_path <- .local_config_dir()
      dir.create(dirname(cfg_path), showWarnings = FALSE, recursive = TRUE)
      writeLines(
        paste0(
          "default:\n  LLM_provider: 'openai'\n",
          "  LLM_URL: 'https://api.openai.com/v1/chat/completions'\n",
          "  LLM_key: ''\n  LLM_model: 'test-model'"
        ),
        cfg_path
      )
      out <- .messages_of(get_llm())
      expect_identical(out$value$key, "<empty>")
      expect_match(out$messages, "Key: <empty>", fixed = TRUE)
    })

  })

})
```

- [ ] **Step 2: 运行确认失败**

Run: `env -u LC_ALL -u LANG -u LC_CTYPE -u LC_COLLATE Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-get_llm.R")'`
Expected: FAIL —— `get_llm` 不存在（`could not find function "get_llm"`）。

- [ ] **Step 3: 写最小实现**

在 `R/connection.R` 的 `chat_llm()` 之前插入：

```r
#' Mask a stored API key for display
#' @noRd
.mask_key <- function(key) {
  if (!nzchar(key)) return("<empty>")
  if (nchar(key) <= 8L) return("****")
  paste0("****", substr(key, nchar(key) - 3L, nchar(key)))
}

#' Show the current LLM service configuration
#' @description Reads the configuration written by \code{\link{set_llm}()} and
#'   reports it without opening the YAML file. The API key is masked by default;
#'   pass \code{show_key = TRUE} to print it in full.
#'
#' @param show_key logical, print and return the API key in full instead of a
#'   masked form. Default \code{FALSE}.
#'
#' @returns A named list invisibly, with elements \code{provider}, \code{url},
#'   \code{model}, \code{key} (masked unless \code{show_key = TRUE}) and
#'   \code{config_path}. The same fields are reported through \code{message()}.
#' @examplesIf nzchar(Sys.getenv("LLMJOIN_API_KEY"))
#' # reads the key from the environment so the example cannot store a placeholder
#' set_llm(provider = "openai", key = Sys.getenv("LLMJOIN_API_KEY"))
#' get_llm()
#' get_llm(show_key = TRUE)
#' @export
get_llm <- function(show_key = FALSE) {
  if (
    length(show_key) != 1L ||
    !is.logical(show_key) ||
    is.na(show_key)
  ) {
    stop(
      "'show_key' must be a single TRUE or FALSE. ",
      "Use get_llm(show_key = TRUE) to print the full key."
    )
  }

  cfg <- .read_config()
  key_display <- if (isTRUE(show_key)) cfg$key else .mask_key(cfg$key)

  message("LLM config read from `", cfg$config_path, "`.")
  message("  Provider: ", cfg$provider)
  message("  Model: ", cfg$model)
  message("  URL: ", cfg$url)
  message("  Key: ", key_display)
  if (!isTRUE(show_key)) {
    message("  (pass show_key = TRUE to print the full key)")
  }

  cfg$key <- key_display
  invisible(cfg)
}
```

> 示例必须受 `LLMJOIN_API_KEY` 守卫：`get_llm()` 在无配置的机器（CRAN 检查机）上
> 会报错，裸 `\examples{}` 会让 `R CMD check` 直接 ERROR。

- [ ] **Step 4: 运行确认通过**

Run: `env -u LC_ALL -u LANG -u LC_CTYPE -u LC_COLLATE Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-get_llm.R")'`
Expected: FAIL 0。

- [ ] **Step 5: 全量回归**

Run: `env -u LC_ALL -u LANG -u LC_CTYPE -u LC_COLLATE Rscript -e 'testthat::test_local(".")'`
Expected: 全绿，记录新断言数。

- [ ] **Step 6: Commit**

```bash
git add R/connection.R tests/testthat/test-get_llm.R
git commit -m "feat: add get_llm() to show the stored configuration with the key masked"
```

---

### Task 4: 异常路径、参数校验、请求路径明文回归（C1-C4, D1-D3, E2, E3）

**Files:**
- Modify: `tests/testthat/test-get_llm.R`（追加两个子 describe）
- Modify: `tests/testthat/test-chat_llm.R`（追加 `it("sends the unmasked key ...")`）

**Interfaces:**
- Consumes: Task 3 的 `get_llm()`；Task 2 的 `.read_config()`；`provider_headers()`（R/providers.R:35）
- Produces: 无新接口

- [ ] **Step 1: 追加测试**

`tests/testthat/test-get_llm.R`，插入到顶层 `describe("get_llm", { ... })` 内：

```r
  describe("config failures report the stored path", {

    it("errors when no config file exists", {
      # Given: 空的临时配置目录
      # When:  get_llm()
      # Then:  报错与 chat_llm 逐字一致，点名 set_llm()
      cfg_path <- .local_config_dir()
      expect_error(
        get_llm(),
        "LLM service not configured. Use `set_llm()` to set up your API key and endpoint."
      )
      expect_false(file.exists(cfg_path))
    })

    it("errors when the config file is not valid YAML", {
      # Given: 制表符缩进的非法 YAML（载荷与 test-chat_llm.R 同源）
      # When:  get_llm()
      # Then:  报错含 Invalid config file / 路径 / reconfigure 指引
      cfg_path <- .local_config_dir()
      dir.create(dirname(cfg_path), showWarnings = FALSE, recursive = TRUE)
      writeLines("\t- not: a mapping:\tabc", cfg_path)
      err <- tryCatch(get_llm(), error = function(e) conditionMessage(e))
      expect_match(err, "Invalid config file", fixed = TRUE)
      expect_match(err, "LLMJOIN.yml", fixed = TRUE)
      expect_match(err, "Use set_llm() to reconfigure.", fixed = TRUE)
    })

    it("errors when the stored config has no key", {
      # Given: 手改配置，缺 LLM_key 行
      # When:  get_llm()
      # Then:  报错含 Config is missing URL or key
      cfg_path <- .local_config_dir()
      dir.create(dirname(cfg_path), showWarnings = FALSE, recursive = TRUE)
      writeLines(
        paste0(
          "default:\n  LLM_provider: 'openai'\n",
          "  LLM_URL: 'https://api.openai.com/v1/chat/completions'\n",
          "  LLM_model: 'test-model'"
        ),
        cfg_path
      )
      expect_error(get_llm(), "Config is missing URL or key. Use set_llm() to reconfigure.")
    })

    it("errors when the stored config names an unknown provider", {
      # Given: 手改配置 LLM_provider: 'nope'
      # When:  get_llm()
      # Then:  报错点名该 provider 并给出 set_llm() 指引
      cfg_path <- .local_config_dir()
      dir.create(dirname(cfg_path), showWarnings = FALSE, recursive = TRUE)
      writeLines(
        paste0(
          "default:\n  LLM_provider: 'nope'\n",
          "  LLM_URL: 'https://example.org/chat'\n",
          "  LLM_key: 'sk-1234567890abcdef'\n  LLM_model: 'test-model'"
        ),
        cfg_path
      )
      expect_error(get_llm(), "Unknown provider 'nope' in config. Run set_llm() to reconfigure.")
    })

  })

  describe("show_key argument validation", {

    it("rejects non-logical, NA, and length > 1 input", {
      # Given: 已写好的合法配置
      # When:  get_llm(show_key = "yes") / NA / c(TRUE, TRUE)
      # Then:  每种都报错点名 'show_key' 并给 TRUE 的用法
      cfg_path <- .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      for (bad in list("yes", NA, c(TRUE, TRUE))) {
        err <- tryCatch(get_llm(show_key = bad), error = function(e) conditionMessage(e))
        expect_match(err, "'show_key' must be a single TRUE or FALSE.", fixed = TRUE)
        expect_match(err, "get_llm(show_key = TRUE)", fixed = TRUE)
      }
    })

  })
```

`tests/testthat/test-chat_llm.R`，插入到顶层 `describe("chat_llm", { ... })` 内
（Task 1 那个子 describe 之后）：

```r
  describe("request path keeps the plaintext key (refactor guard)", {

    it("passes the unmasked key to provider_headers", {
      # Given: 合法配置（key 19 字符）+ provider_headers / httr 全打桩
      # When:  chat_llm(.message = "hi")
      # Then:  provider_headers 收到的 key 是明文——脱敏只发生在展示层
      cfg_path <- .local_config_dir()
      suppressMessages(
        set_llm(provider = "openai", key = "sk-1234567890abcdef", model = "test-model")
      )
      captured <- new.env()
      fake_response <- structure(list(status_code = 200L), class = "response")
      local_mocked_bindings(
        provider_headers = function(provider, key) {
          captured$key <- key
          list(`Content-Type` = "application/json")
        },
        .package = "llmjoin"
      )
      local_mocked_bindings(
        POST = function(url, ...) { fake_response },
        content = function(x, ...) '{"choices":[{"message":{"content":"01,January"}}]}',
        status_code = function(x) 200L,
        .package = "httr"
      )
      chat_llm(.message = "hi")
      expect_identical(captured$key, "sk-1234567890abcdef")
      expect_false(identical(captured$key, "****cdef"))
    })

  })
```

> `test-chat_llm.R` 需要 `.local_config_dir()` 助手；把它从 `test-get_llm.R` 移到
> `tests/testthat.R`？——**不要**。testthat 各文件独立执行，助手要么复制、要么放
> `tests/testthat/setup.R`。本步按 tests/AGENTS.md「夹具内联」的约定，在
> `test-chat_llm.R` 顶部复制同一份 `.local_config_dir()` 定义（4 行），并在两处注释
> 说明彼此同步。

- [ ] **Step 2: 运行确认失败或通过**

Run: `env -u LC_ALL -u LANG -u LC_CTYPE -u LC_COLLATE Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-get_llm.R"); testthat::test_file("tests/testthat/test-chat_llm.R")'`
Expected: 全通过（实现已在 Task 2/3 落地）。若某条 FAIL，按 systematic-debugging 定位后修实现，不得改断言迁就代码。

- [ ] **Step 3: 全量回归**

Run: `env -u LC_ALL -u LANG -u LC_CTYPE -u LC_COLLATE Rscript -e 'testthat::test_local(".")'`
Expected: 全绿，记录最终断言数。

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test-get_llm.R tests/testthat/test-chat_llm.R
git commit -m "test: cover get_llm failure paths, show_key validation, and plaintext passthrough"
```

---

### Task 5: 版本、文档、总纲同步（0.3.2）

**Files:**
- Modify: `DESCRIPTION`（Version）
- Modify: `NEWS.md`（新增顶部 0.3.2 段）
- Modify: `README.md:28`
- Create: `man/get_llm.Rd`（由 `devtools::document()` 生成，不手改）
- Modify: `NAMESPACE`（document 生成）
- Modify: `AGENTS.md`（§架构 config 层、P0.2 基线数、§已知问题 2 的示例计数、§目录结构加 docs/）
- Modify: `R/AGENTS.md`（`connection.R` 行描述）
- Modify: `tests/AGENTS.md`（清单加 `test-get_llm.R`、基线数）
- Modify: `.Rbuildignore`（`^docs$`）
- Create: `handoff/261008_新增get_llm配置查看.md`

- [ ] **Step 1: 版本号**

`DESCRIPTION` 第 4 行：`Version: 0.3.1` → `Version: 0.3.2`

- [ ] **Step 2: NEWS.md 顶部新增段**

在 `# llmjoin 0.3.1` 之前插入（标题层级与既有段一致）：

```markdown
# llmjoin 0.3.2

## changes
- Added `get_llm()`: shows the configuration written by `set_llm()` (provider, model, URL, config file path) without opening the YAML file. The API key is masked by default — keys longer than 8 characters show only their last 4, shorter keys show `****` — and `get_llm(show_key = TRUE)` is the only way to print or return the full key. The returned list carries the same masking as the printed output, so `get_llm()$key` cannot leak the credential into a log. `chat_llm()` still authenticates with the full key.
```

- [ ] **Step 3: README.md**

把第 28 行的说明改为：

```markdown
> Please note that all information is stored strictly locally in your system configuration, and is never uploaded or shared. Run `get_llm()` at any time to see the active provider, model, endpoint and config file path (the API key is masked; use `get_llm(show_key = TRUE)` to reveal it).
```

在 `set_llm()` 代码块后追加一行说明：

````markdown
Check what is currently configured with:
```R
get_llm()
```
````

- [ ] **Step 4: 生成文档**

Run: `env -u LC_ALL -u LANG -u LC_CTYPE -u LC_COLLATE Rscript -e 'devtools::document()'`
Expected: 新增 `man/get_llm.Rd`；`NAMESPACE` 出现 `export(get_llm)`；`set_llm.Rd` / `chat_llm.Rd` 无计划外改动（`git diff man/` 核对）。

- [ ] **Step 5: AGENTS 体系同步**

- `AGENTS.md` §架构「配置层」条目：补 `get_llm()` 与 `.read_config()` / `.mask_key()` 的职责与签名；说明 `chat_llm()` 现在经 `.read_config()` 读配置。
- `AGENTS.md` P0.2 基线：`281 项断言` → Task 4 实测数。
- `AGENTS.md` §已知问题 2：「四个需要凭据的示例」→ 五个（`get_llm` 示例同样受守卫），并点名 get_llm 在无配置机器上会 ERROR，所以不能裸 `\examples{}`。
- `AGENTS.md` §目录结构：加一行 `docs/superpowers/plans/`（计划文档，`.Rbuildignore` 排除）。
- `R/AGENTS.md` `connection.R` 行：`set_llm()` / `get_llm()` / `.read_config()` / `chat_llm()`。
- `tests/AGENTS.md`：基线数 + 清单新增 `test-get_llm.R`（覆盖面一句话：脱敏边界、配置故障路径、`show_key` 校验）。

- [ ] **Step 6: 写 handoff（按根 §交接记录约定 的 7 节结构）**

`handoff/261008_新增get_llm配置查看.md`，元信息「上一份交接」指向
`handoff/261008_放弃托管端点并修示例守卫.md`；§2 验证结果必须填**实测数字**
（test_local 断言数、R CMD check Status、脱敏边界样本）；§4 遗留问题写明：
默认模型/端点真实 API 验证仍未做（继承根 §已知问题 1）、get_llm 示例在 CRAN
机永不执行（§已知问题 2 的延伸）、cran-comments.md 待重写（§已知问题 3）。

- [ ] **Step 7: `.Rbuildignore`**

追加一行 `^docs$`。

- [ ] **Step 8: 完整检查**

Run: `env -u LC_ALL -u LANG -u LC_CTYPE -u LC_COLLATE Rscript -e 'devtools::check(cran = TRUE)'`
Expected: `Status: OK`，0 errors / 0 warnings / 0 notes。**不设** `LLMJOIN_API_KEY`（避免受守卫示例真实写配置并发请求）。

- [ ] **Step 9: Commit**

```bash
git add DESCRIPTION NAMESPACE NEWS.md README.md man/ AGENTS.md R/AGENTS.md tests/AGENTS.md .Rbuildignore handoff/ docs/
git commit -m "feat: document get_llm() and release-prep 0.3.2"
```

---

### Task 6: 打 tag 并推送

- [ ] **Step 1: 确认工作树干净、HEAD 即 0.3.2 内容**

Run: `git status --short && git log --oneline -6`
Expected: 无未提交改动（`.zcodeignore` 维持未追踪，本任务不动它）。

- [ ] **Step 2: 打轻量沿用现有命名的 tag**

Run: `git tag v0.3.2 && git tag`
Expected: 列出 `v0.3.1` `v0.3.2`

- [ ] **Step 3: 推送提交与 tag（用户已授权）**

```bash
git push origin master
git push origin v0.3.2
```
Expected: 两个 ref 均成功；`backup/pre-history-rewrite-261005` 分支**永不推送**（口径⑤）。

- [ ] **Step 4: 提醒维护者做 release**

GitHub Release 需网页端填 changelog，不由本代理创建；提醒用户：
以 tag `v0.3.2` 创建 release，正文取 NEWS.md 的 `# llmjoin 0.3.2` 段。

---

## Self-Review

1. **Spec coverage**：脱敏默认（A2/B1-B4 ✓）、不用找 YAML（A1 打印 config_path ✓）、
   新特性入 0.3.2（Task 5 DESCRIPTION/NEWS ✓）、GWT 计划（本文件 ✓）、
   测试先行（Task 1/3/4 RED 步骤 ✓）、修复与验证（Task 4 Step 2、Task 5 Step 8 ✓）、
   tag 0.3.2（Task 6 ✓）、提醒 release（Task 6 Step 4 ✓）。
2. **Placeholder scan**：唯一待验证项是非法 YAML 载荷（Task 1 已给替代方案与核验步骤），
   不是占位符；其余代码块均为可直接落地的完整内容。
3. **Type consistency**：`.read_config()` 五字段（provider/url/key/model/config_path）
   在 Task 2 定义、Task 3 `get_llm()` 原样透传并只替换 `key`；`.mask_key()` 返回
   character(1)；`get_llm(show_key=)` 签名三处（实现、测试、NEWS/README/roxygen）一致。
