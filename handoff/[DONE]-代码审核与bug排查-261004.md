# [DONE] 代码审核与bug排查 (261004)

## 背景
对 v0.3.1(CRAN 重新提交前)做全面代码审核:通读 4 个 R 源文件、测试、README、NEWS,并在 R 4.6.1 下逐项复现疑似问题。

## 做了什么
- 通读 R/connection.R、R/providers.R、R/llmjoin.R、R/utils.R 及 tests/。
- 从源码运行测试:`testthat::test_local()` 全绿(78 通过 / 0 失败)。注意本机 `library(llmjoin)` 装的是 0.3.0 旧版,直接加载旧包跑新测试会报假失败(`unused arguments (x_keys...)`),需 `devtools::install()` 同步。
- 用 /tmp 验证脚本逐项复现 12 个疑点,确认 8 个真实问题(见下)。

## 确认的问题(按严重度)
1. 【高】`llm_join()`(R/llmjoin.R:229-230)两次 `merge()` 未指定 `by`,按同名列交集连接:
   - x、y 有同名非键列(如都有 `value`)→ 第二段 merge 连错键,y 数据静默丢失/错配;
   - x 已含名为 `key2` 的列 → 第一段 merge 按 key1+key2 联合键,静默一行都匹配不上。
   修复:`by = key1` / `by = key2`;README:126 的 `Reduce(merge)` 示例同样要改。
2. 【中】`parse_joint()`(R/llmjoin.R:111-118)表头探测不识别带引号表头(`"id","month"`)与
   通用表头(`value1,value2`),原表头行混入数据行。build_joint 路径有防伪造兜底,README 手动
   parse_joint 直连流程会拿到垃圾行。
3. 【中】`tbl2md()`(R/llmjoin.R:20-26)对 factor 向量静默输出空表(`is.vector(factor)` 为 FALSE)。
4. 【中】`provider_parse()` claude 分支(R/providers.R:99)只取 `text_blocks[[1]]`,Claude 长回复
   拆多个 text block 时被静默截断。应 `paste(vapply(...), collapse="")`。
5. 【中低】README:112 `writeClipboard()` 仅 Windows 存在,macOS/Linux 报错,与"跨平台"承诺不符。
6. 【低】x 键列含 NA 时,LLM 回显的字符串 `"NA"` 被防伪造校验当伪造值丢弃(R/llmjoin.R:145)。
7. 【低】`chat_llm()` 收到长度>1 的向量消息报 `'length = 2' in coercion to 'logical(1)'`
   (R/connection.R:97-99),错误信息不指向参数。
8. 【低】key1/key2 拼错时报 `undefined columns selected`,来自 `[.data.frame` 深处,不指名参数;
   建议入口显式校验。

## 待现场验证 / 增强
- OpenAI 默认模型 gpt-5.4-mini + `max_tokens`/`temperature` 请求体可能被官方 API 拒绝
  (GPT-5/o 系要求 `max_completion_tokens`、拒 temperature),需实测。
- 可在 `provider_parse()` 检查 `finish_reason=="length"` 截断并警告。
- 可在 `parse_joint()` 末尾对未匹配的 x_keys 提示"N 个键未匹配"。

## 变更文件
- 无(纯审核,未改代码)。

## 验证
testthat::test_local():FAIL 0 | WARN 0 | SKIP 0 | PASS 78。所有 bug 结论均有最小复现脚本佐证。

## 遗留与下一步
- 上述 8 个问题均未修复;优先修 #1(一行改动、影响核心语义)。
- 原协作说明 的架构描述当时已大面积过时(.thinking、validate_llm_config 等已在 0.2.2 移除),
  已在下一轮换新 AGENTS.md 时修正。
