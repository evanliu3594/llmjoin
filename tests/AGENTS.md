# AGENTS.md — tests/ 测试层

> 本目录存放 testthat(edition 3)测试。总体约束见根目录 `AGENTS.md`;运行方式见根 §常用命令——一律从源码加载(`test_local`),勿用 `library(llmjoin)`(本机安装包落后于源码,会假失败)。

## 规则

- 红线:交付前 `testthat::test_local()` 全绿,当前基线 **371 项断言、0 失败**(随改动更新本行与根 P0.2)。
- 测试不得依赖真实 LLM 服务,一律 `local_mocked_bindings()` 打桩(根 P0.2)。
- 新场景先写骨架(G-W-T 注释 + 夹具 + mock 桩)再填断言与实现;失败信息必须能定位到场景。
- 断言 markdown / 含 `|` 的字符串用 `expect_match(..., fixed = TRUE)`(261004 曾因 `|` 是正则
  元字符写出恒真的空交替正则)。
- 本机 testthat 的 `expect_match()` / `expect_error()` 传 `invert = TRUE` 会因 grepl 无该参数
  而报错(261005 实测);反向断言用 `expect_false(grepl(..., fixed = TRUE))`。

## 当前清单(261006)

| 文件 | 覆盖面(一句话) | 状态 |
|---|---|---|
| `test-parse_joint.R` | CSV 解析、表头探测(引号/通用)、错误处理、防伪造过滤、NA 回显、未匹配键提示(E2) | 现行 |
| `test-llm_join.R` | 显式合并键:回归 / 同名非键列 / x 含 key2 同名列 / NA 键保留 / NA 回显不误告警 | 现行 |
| `test-tbl2md.R` | factor 向量、单列 factor data.frame、NA 渲染 | 现行 |
| `test-providers.R` | 注册表默认模型、openai reasoning 请求体、E1 截断警告、deepseek 管线、provider_parse(claude) text block | 现行 |
| `test-chat_llm.R` | chat_llm 消息校验与强转拼接(临时配置目录 + httr 打桩)、配置读取四类错误、**空串 key 拦截(证明未发请求)**、请求路径拿到**明文** key | 现行 |
| `test-get_llm.R` | get_llm 正常读取与打印、脱敏边界(≤8 全遮 / >8 露末 4 / 空串 `<empty>`)、配置故障路径、`show_key` 参数校验、无写副作用 | 现行 |
| `test-set_llm.R` | set_llm 的 key 入参护栏(missing/NULL/`""`/零长/长度>1/NA/非字符 共用一条指引)、provider 校验先于 key、正常写入与报告 | 现行 |
| `test-no_key_leak.R` | 凭据不回显护栏(根 P0.1):`set_llm` 消息、`chat_llm` 三类错误文案与 `.verbose` 过程消息均不含明文 key,末条为正向对照 | 现行 |
| `test-build_joint.R` | build_joint 键名/data.frame 入口校验 | 现行 |

## 本目录特有要点

- mock 模式:`local_mocked_bindings(chat_llm = function(...) "<CSV>", .package = "llmjoin")`;
  夹具直接内联在测试体内,不设独立 fixture 文件。
- **`local_mocked_bindings()` 只能在 `it()` 体内直接调用**,不得包进 helper:它的 teardown 绑在调用帧上,包进函数会在 helper 返回时就把 mock 撤掉,后续 `httr::POST` 会**打出真实请求**(261008 写 `test-no_key_leak.R` 时差点踩中,故该文件里留了注释)。
- 负向断言("某文本里不该出现 X")必须配一条**正向对照**证明检测器认得出 X,否则 collector 坏掉时整组断言空过(261008 `test-no_key_leak.R` 末条即为此设)。
- 配置隔离助手 `.local_config_dir()`(4 行)在 `test-chat_llm.R` 与 `test-get_llm.R`
  **各存一份**(现为 `test-chat_llm.R`、`test-get_llm.R`、`test-no_key_leak.R`、`test-set_llm.R` 四处):testthat 给每个测试文件独立环境,helper 无法跨文件复用(261008 实测决定)。
  改一处必须同步另一处。它用 `withr::local_envvar(.local_envir = parent.frame())`,
  只能在测试体内调用;在别的函数里调用会让临时目录提前恢复。
- 收集 message 做断言时用 `.messages_of(expr)`(`test-get_llm.R` 顶部);**不要**在
  其内部再套 `suppressMessages()`——会把要断言的文本一起吞掉(261008 踩过一次)。
- 断言"不可见返回"用 `withVisible(withCallingHandlers(f(), message = ...))`:
  `withCallingHandlers` 保留可见性(261008 实测),而测试体内本不会自动打印,
  用 `capture.output()` 捕获赋值只会得到恒真结果。
