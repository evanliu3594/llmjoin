# AGENTS.md — tests/ 测试层

本目录是 testthat（edition 3）测试。总体约束见根 `AGENTS.md`，运行方式见根 §常用命令。

## 规则

1. 交付前 `testthat::test_local()` 全绿。基线 416 项断言、0 失败；改动后同步更新本行与根 P0.2。
2. 测试不调用真实 LLM 服务，一律用 `local_mocked_bindings()` 打桩（根 P0.2）。
3. 新场景先写骨架：G-W-T 注释、夹具、mock 桩齐备，再填断言与实现。失败信息要能定位到场景。
4. 从源码加载测试，用 `test_local()`；本机安装的 llmjoin 落后于源码，`library(llmjoin)` 会给出假失败。

## 文件清单

| 文件 | 覆盖面 |
|---|---|
| `test-parse_joint.R` | CSV 解析、表头探测（引号式与通用式）、错误处理、防伪造过滤、NA 回显、未匹配键提示（E2） |
| `test-llm_join.R` | 显式合并键：回归、同名非键列、x 含 `key2` 同名列、NA 键保留、NA 回显不误告警 |
| `test-tbl2md.R` | factor 向量、单列 factor data.frame、NA 渲染 |
| `test-providers.R` | 注册表默认模型、openai reasoning 请求体、E1 截断警告、deepseek 管线、claude 的 text block 拼接 |
| `test-chat_llm.R` | 消息校验与强转拼接、`.read_config()` 四类错误、空串 key 拦截（并证明未发请求）、请求路径拿到明文 key |
| `test-get_llm.R` | 正常读取与打印、脱敏三档边界、配置故障路径、`show_key` 参数校验、无写副作用 |
| `test-set_llm.R` | `key` 入参护栏（missing、NULL、`""`、零长、长度 > 1、NA、非字符共用一条指引）、provider 校验先于 key、正常写入与报告 |
| `test-no_key_leak.R` | 凭据不回显护栏（根 P0.1）：`set_llm` 消息、`chat_llm` 三类错误文案、`.verbose` 过程消息均不含明文 key；末条为正向对照 |
| `test-joint_prompt.R` | 输出承诺（逐字复制、恰好 N 行、禁管道表与制表符）与两种入参形态（data.frame、向量） |
| `test-build_joint.R` | 键名与 data.frame 入口校验 |

## 本目录特有的事实

1. **mock 写法**：`local_mocked_bindings(chat_llm = function(...) "<CSV>", .package = "llmjoin")`。夹具内联在测试体内，不设独立 fixture 文件。
2. **`local_mocked_bindings()` 只在 `it()` 体内直接调用，不包进 helper。** teardown 绑在调用帧上，包进函数会在 helper 返回时撤销 mock，之后的 `httr::POST` 会发出真实请求（261008 写 `test-no_key_leak.R` 时差点踩中，该文件留有注释）。
3. **负向断言必须配一条正向对照。** 断言"某文本里不该出现 X"时，另加一条证明检测器认得出 X 的断言。否则 collector 失效时整组负向断言空过（261008 `test-no_key_leak.R` 末条即为此设）。
4. **配置隔离助手 `.local_config_dir()` 在 4 个文件里各存一份**：`test-chat_llm.R`、`test-get_llm.R`、`test-no_key_leak.R`、`test-set_llm.R`。testthat 给每个测试文件独立环境，helper 无法跨文件复用（261008 实测决定）。改一处要同步其余三处。该助手用 `withr::local_envvar(.local_envir = parent.frame())`，只能在测试体内调用；在别的函数里调用会让临时目录提前恢复。
5. **断言含 `|` 或 markdown 的文本用 `expect_match(..., fixed = TRUE)`。** `|` 在正则里是交替元字符，261004 曾因此写出恒真的空交替正则。
6. **本机 testthat 的 `expect_match()` 与 `expect_error()` 不支持 `invert = TRUE`**，会因 `grepl` 无该参数报错（261005 实测）。反向断言用 `expect_false(grepl(pattern, text, fixed = TRUE))`。
7. **收集 message 用 `.messages_of(expr)`**（定义在 `test-get_llm.R` 顶部）。在该函数内部再套 `suppressMessages()` 会吞掉待断言的文本（261008 踩过一次）。
8. **断言不可见返回用 `withVisible(withCallingHandlers(f(), message = ...))`。** `withCallingHandlers` 保留可见性（261008 实测）；测试体内本来不会自动打印，用 `capture.output()` 捕获赋值结果只会得到恒真值。
