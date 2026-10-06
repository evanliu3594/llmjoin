# AGENTS.md — tests/ 测试层

> 本目录存放 testthat(edition 3)测试。总体约束见根目录 `AGENTS.md`;运行方式见根 §常用命令——一律从源码加载(`test_local`),勿用 `library(llmjoin)`(本机安装包落后于源码,会假失败)。

## 规则

- 红线:交付前 `testthat::test_local()` 全绿,当前基线 **205 项断言、0 失败**(随改动更新本行与根 P0.2)。
- 测试不得依赖真实 LLM 服务,一律 `local_mocked_bindings()` 打桩(根 P0.2)。
- 新场景先写骨架(G-W-T 注释 + 夹具 + mock 桩)再填断言与实现;失败信息必须能定位到场景。
- 断言 markdown / 含 `|` 的字符串用 `expect_match(..., fixed = TRUE)`(261004 曾因 `|` 是正则
  元字符写出恒真的空交替正则)。
- 本机 testthat 的 `expect_match()` / `expect_error()` 传 `invert = TRUE` 会因 grepl 无该参数
  而报错(261005 实测);反向断言用 `expect_false(grepl(..., fixed = TRUE))`。

## 当前清单(261005)

| 文件 | 覆盖面(一句话) | 状态 |
|---|---|---|
| `test-parse_joint.R` | CSV 解析、表头探测(引号/通用)、错误处理、防伪造过滤、NA 回显 | 现行 |
| `test-llm_join.R` | 显式合并键:回归 / 同名非键列 / x 含 key2 同名列 / NA 键保留 / NA 回显不误告警 | 现行 |
| `test-tbl2md.R` | factor 向量、单列 factor data.frame、NA 渲染 | 现行 |
| `test-providers.R` | provider_parse(claude):单/多 text block、仅 thinking 报错 | 现行 |
| `test-chat_llm.R` | chat_llm 消息校验与强转拼接(临时配置目录 + httr 打桩) | 现行 |
| `test-build_joint.R` | build_joint 键名/data.frame 入口校验 | 现行 |

## 本目录特有要点

- mock 模式:`local_mocked_bindings(chat_llm = function(...) "<CSV>", .package = "llmjoin")`;
  夹具直接内联在测试体内,不设独立 fixture 文件。
