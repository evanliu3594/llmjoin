# AGENTS.md — llmjoin 协作约定

本文件是 AI 助手(Agent)与维护者在本仓库协作的最高约定。
约束按效力分四级,冲突时以高级别为准:**P0 > P1 > P2 > P3**。

| 级别 | 含义 | 违反后果 |
|------|------|----------|
| P0 | 硬约束(MUST),无条件遵守 | 变更必须回滚或拒绝 |
| P1 | 流程约束(MUST),实现类任务的强制流程 | 任务视为未完成,不得交付 |
| P2 | 指导约定(SHOULD),默认遵守 | 允许偏离,但须说明理由 |
| P3 | 风格偏好(MAY),参考执行 | 自由裁量 |

## 项目定位与当前状态

llmjoin:用 LLM 做数据框模糊连接(拼写变体、跨语言、精度差异),卖点是**依赖极简**
(Imports: httr、jsonlite、config)。当前 0.3.2:0.3.1 及之前已有 GitHub tag/release,
0.3.2 的 tag 由 261008 会话打上(release 待维护者创建);CRAN **尚未提交**
(CRAN-SUBMISSION 最后记录 0.3.0,2026-06-08)。261008 以 0.3.2 内容跑本机
`devtools::check(cran = TRUE)` 为 `Status: OK`(0 errors / 0 warnings / 0 notes),
投稿前仍须按 §已知问题 3 重写 cran-comments.md。测试红线见 P0.2。

## 目录结构(要点;R/ 与 tests/ 明细见各自 AGENTS.md)

| 目录 / 文件 | 职责 | 明细位置 |
|---|---|---|
| `R/` | 包实现(唯一事实源) | `R/AGENTS.md` |
| `tests/` | testthat 测试 | `tests/AGENTS.md` |
| `man/` | roxygen 生成文档,不手改 | R/ 源文件内嵌 roxygen 注释 |
| `handoff/` | 交接记录,兼任项目时间线 | 根 §交接记录约定 |
| `DESCRIPTION` / `NAMESPACE` / `NEWS.md` / `README.md` | 包元数据 / 导出表 / 用户可见变更 / 使用说明 | — |

## 已拍板口径(用户确认,勿再改)

| # | 口径 | 决定 | 日期 |
|---|---|---|---|
| ① | 时间线载体 | `handoff/` 兼任项目时间线,**不建 HISTORY.md**(261004 拍板;261007 起交接协议全面对齐 project-conventions 技能——该技能已以 `handoff/` 交接目录取代 HISTORY.md 单文件,口径收敛,无冲突) | 261007 |
| ② | 目录级文档 | 仅建 `R/`、`tests/` 两份 AGENTS.md;其余目录不建(man/ 由工具生成,NEWS.md 承载用户可见变更)。**不新建顶层目录放开发辅助产物**——实施计划、场景矩阵、审查记录一律进 `handoff/`;目录与文件名亦不得点名开发辅助工具(与口径③同向)。维护者 261008 就 `docs/superpowers/plans/` 的出现重申本条 | 261004(261008 重申) |
| ③ | 开发辅助工具 | 提交信息、用户可见文档、交接记录**不点名开发辅助工具**;provider 功能语境除外(接入新 LLM 服务时如实描述) | 261004 |
| ④ | 版本管理 | AGENTS 体系(根 + `R/`、`tests/`)与 `handoff/` **纳入 git 追踪**;`.Rbuildignore` 维持排除,不进 R CMD 构建(261005 维护者拍板;历史记录的措辞中性化以维护者 261004 就地修订为先例) | 261005 |
| ⑤ | 历史重写 | 261005 已对全历史执行痕迹清除重写并 force-push:b9b1c4b 起的提交 SHA 均已改变(映射见 handoff/261005_全量清除开发助手痕迹.md),旧记录中的 SHA 引用以该映射为准;归档于仓库外 bundle 与本地 `backup/` 分支,永不推送 | 261005 |
| ⑥ | claude 字样范围 | provider 注册表与用户文档保留 claude 配置入口(provider 功能语境,口径③除外条款适用);261005 清除的仅为协作者身份痕迹,「provider 语境 claude 除名」问题就此关闭 | 261007 |
| ⑦ | 不做托管免费端点 | 不提供包内默认可用的托管转发服务端(不建 server/、不持上游 key、不承诺第三方免费额度);新用户路径改为 README 指向服务商 key 申请页,需要凭据的示例用 `@examplesIf nzchar(Sys.getenv("LLMJOIN_API_KEY"))` 守卫。动机:托管端点要求维护者承担域名续费、证书续期、刷屏处置与宕机值守的无限期责任,且包内硬编码 URL 在 CRAN 发布后几乎不可更改;维护者 261008 拍板放弃(决策链见 handoff/261008_示例守卫与DeepSeek端点修正与托管端点放弃.md §3;261008 二次会话修正本指针——原指向的 `261008_放弃托管端点并修示例守卫.md` 是同会话的未入库草稿) | 261008 |
| ⑧ | 凭据显示口径 | `get_llm()` **默认脱敏** API key:`.mask_key()` 把空串显示为 `<empty>`、`nchar <= 8` 全遮为 `****`、更长的只露末 4 位;返回 list 的 `key` 与打印内容一致,取明文只能显式 `show_key = TRUE`。动机:控制台输出会进 `.Rhistory`、截屏和 CI 日志,默认打印明文等于把凭据外泄做成包的标准动作。请求路径不受影响——`chat_llm()` 始终用存储的明文 key,该行为由测试固定;维护者 261008 拍板。**输入侧边界**(同轮追问的结论):脱敏原则不对称——包管得住的是"自己不把明文凭据推到不可控渠道"(已入 P0.1),管不住"用户在控制台敲字面量因而进了 `.Rhistory`",那只能改获取渠道,故 README 与 `set_llm()` 文档推荐 `Sys.getenv("LLMJOIN_API_KEY")`。`ask_key()` 隐藏输入助手(依赖 `getPasswd()`,RStudio 控制台有已知限制)与配置文件 `Sys.chmod(0600)`(Windows 上只切只读位)**两项均未实测**,将来要做先出探针再写码 | 261008 |

## P0 硬约束

1. **CRAN 合规**
   - 不向用户主目录写任何文件;配置只写 `tools::R_user_dir("llmjoin", "config")`。
   - 新增依赖必须证明必要性并评估传递依赖成本;本包卖点是依赖极简(Imports: httr、jsonlite、config)。
   - 用户可见输出用 `message()` / `warning()` / `stop()`,禁止 `cat()`。
   - 严禁提交 API key、密钥或真实配置文件。
   - **包自身不得把明文凭据推到任何用户可见渠道**:`message()` / `warning()` / `stop()` 的文案只可含 provider / model / URL / 配置路径,不得含 `LLMJOIN_key` 的值;由 `tests/testthat/test-no_key_leak.R` 固定(该文件另含一条"检测器确实认得出明文"的正向对照,防止五条负向断言空过)。文档与示例教用户从环境变量传 key,不教字面量。
2. **测试红线**:交付前 `testthat::test_local()` 必须全绿(当前基线:367 项断言,0 失败);测试不得依赖真实 LLM 服务,一律用 `local_mocked_bindings()` 打桩。
3. **防伪造校验是安全特性**:`parse_joint()` 的 `x_keys` / `y_keys` 白名单过滤不得移除或弱化;`build_joint()` / `llm_join()` 必须默认传键值集合。
4. **API 兼容**:导出函数的签名或语义变更必须记入 NEWS.md 当前版本段,并说明迁移方式。
5. **base R 优先**:禁止引入 tidyverse / magrittr / readr;管道用 `|>`,匿名函数用 `\(x)`,字符串处理优先 base 函数。

## P1 流程约束(实现类任务)

本仓库遵循 BDD/TDD:

0. 口径不明、存在多个可选方案且取舍影响结果、或需改动"已拍板口径"时,先问后做
   (选项 + 影响 + 推荐,一次问清);纯机械改动不问,但执行前后须声明预期并核对。
1. 先与用户确认 Given-When-Then 场景矩阵(正常 / 异常 / 边界)。
2. 生成测试文件骨架:导入、夹具、mock 桩齐全,测试体留 G-W-T 注释。
3. 场景确认后,按测试函数拆分为独立子任务。
4. 子代理填写测试与实现,自验通过后跑全量回归。
5. 审核标准:全部测试通过 + G-W-T 全覆盖 + 无副作用;不合格则修复或拒绝。
6. 每轮并发子代理不超过 4 个;子代理无活动超时连续两次即由编排者按已商定方案接手,
   不再重试(261004 曾发生:同一任务两个代理先后 600 秒零产出超时)。

## P2 指导约定

1. roxygen 注释变更后运行 `devtools::document()`;`man/` 由工具生成,不手改。
2. 每个用户可见变更(修复 / 新特性 / 破坏性)在 NEWS.md 对应版本段记录。
3. `stop()` 的错误信息必须包含"如何修复"的指引(如 `Use set_llm() to reconfigure.`)。
4. 代码行为与本文件"架构"一节发生漂移时,随该次变更一并更新本文件。
5. 疑难结论、审查结果、不可复现的问题 → 写入 handoff/(见下节约定)。

## P3 风格偏好

- 函数命名 snake_case;内部辅助函数加 `.` 前缀或 `@noRd`。
- 错误 / 警告信息用英文(与 CRAN 包惯例一致),面向开发者的文档用中文。
- 提交信息用英文祈使句,前缀 `feat:` / `fix:` / `docs:` / `chore:`。
- 交接记录与用户沟通:直白朴素、先结论后证据,证据带可核对数值;未验证的结论注明"未验证"。

## 架构(2026-10-08 与 v0.3.2 代码同步;2026-10-04 曾与 v0.3.1 同步)

- **配置层** `R/connection.R`:`set_llm()` 写 YAML 配置(单引号转义为 `''`)到
  `tools::R_user_dir("llmjoin", "config")/LLMJOIN.yml`;`key` 入参护栏把 missing / NULL /
  空串 / 零长 / 长度 > 1 / NA / 非字符统一成一条点名参数并给出检查方法的错误(旧实现会漏出
  `condition has length > 1` 这类内部错误);校验顺序 provider → key → url → model 未变。
  `get_llm(show_key = FALSE)`
  读回同一份配置,用 `message()` 报告 provider / model / URL / 配置文件路径,并返回
  不可见 list(`provider` / `url` / `model` / `key` / `config_path`)。key 默认经内部
  `.mask_key()` 脱敏:空串 → `<empty>`、`nchar <= 8` → `****`、更长 → `****` + 末 4 位;
  返回值与打印内容一致,取明文必须显式 `show_key = TRUE`(脱敏是安全特性,不得弱化)。
  `chat_llm(.message, .model, .temperature, .max_tokens, .timeout, .verbose)` 是唯一
  LLM 调用入口,经私有 `.read_config()` 读取并校验配置(文件缺失 → YAML 非法 → 缺 URL/key
  → provider 未知,四类错误顺序与文案沿用 0.3.1,一字未改),请求路径始终用**明文** key;
  `.message` 接受任意可强转输入,元素 as.character 后
  NA 置空、向量按换行 paste 成单串,missing/NULL/零长/全空白报错点名参数。
  默认 `.max_tokens = 30000`、`.timeout = 300`、
  `.temperature = 0`(越界自动截断并警告;openai gpt-5+/o 系模型不发 temperature,
  非 0 时警告被忽略,见 Provider 层)。包内 thinking / reasoning 输出模式已在
  0.2.2 移除,不再支持。
- **Provider 层** `R/providers.R`:`.providers` 注册表(openai / claude / gemini / deepseek)→
  base_url、endpoint、default_model、auth_type。四个内部函数:
  `provider_headers()`(openai/gemini/deepseek 走 Bearer;claude 走 `x-api-key` + `anthropic-version`)、
  `provider_body()`(请求体;openai 模型名匹配 `^(o[0-9]|gpt-[5-9])` 的 GPT-5+/o 系发
  `max_completion_tokens` 并省略 temperature,其余模型维持 `max_tokens`+temperature——
  第三方兼容端点经 provider="openai" 接入,不得整体切换)、
  `provider_parse()`(响应解析;claude 过滤 thinking block,按序拼接全部 text block;
  截断回复警告——openai/gemini/deepseek `finish_reason=="length"`、claude
  `stop_reason=="max_tokens"`)、
  `provider_url()`。新增 provider = 注册表加一项 + 四个函数各加一个 case。
- **Join 层** `R/llmjoin.R`:
  `tbl2md()`(data.frame/向量 → markdown 表)→
  `joint_prompt()`(两键列 → 匹配提示词)→
  `build_joint(x, y, key1, key2, ...)`(入口校验 x/y 为 data.frame、键名为存在的列,辅助函数
  `.validate_key()` → 造 prompt → `chat_llm()` → `parse_joint()`,自动传
  `x_keys`/`y_keys`)→
  `parse_joint(llm_response, key1, key2, x_keys, y_keys)`(剥 markdown fence → rle 取最长含逗号
  行块 → 表头探测/补齐 → `utils::read.csv` → 防伪造过滤 → 未匹配键提示
  `.warn_unmatched()`,x/y 双侧各一条 warning,真实 NA 不计,只提示不删行)→
  `llm_join()`(build_joint + 两次显式 `merge()`;x 已含名为 `key2` 的列时靠 merge 后缀
  探测定位 joint 键列)。
- **工具** `R/utils.R`:`%||%`、`globalVariables`、NAMESPACE imports(httr/jsonlite)。
- **测试** `tests/testthat/`:`test-parse_joint.R`、`test-llm_join.R`、`test-tbl2md.R`、
  `test-providers.R`、`test-chat_llm.R`、`test-build_joint.R`;mock 模式
  `local_mocked_bindings(chat_llm = function(...) "...", .package = "llmjoin")`。

### 已知问题(待修复,详见 handoff/)

2026-10-04 审核的 1-4 项(llm_join 合并键、parse_joint 表头探测、tbl2md factor、
claude 多 text block)与遗留的 4 项低优先级问题(README writeClipboard、"NA" 键回显、
chat_llm 消息校验、键名入口校验)均已修复(后者见
handoff/261005_修复遗留问题5至8.md)。261006 完成 DeepSeek provider、默认模型
调整(openai gpt-6-luna / gemini gemini-3.8-flash / deepseek deepseek-flash)、openai
reasoning 请求体修复与 E1/E2 增强(见
handoff/261006_默认模型与DeepSeek及E1E2.md)。剩余待办:

1. 【部分核实(261008)】默认模型与端点路径的真实 API 验证仍未做。已按官方文档核实:
   deepseek 模型实名 `deepseek-flash` 属实;`base_url` 已改为官方 curl 示例的
   `https://api.deepseek.com`(去掉了 `/v1`),但**这条新路径未发过真实请求**。
   openai gpt-6-luna / gemini gemini-3.8-flash 与 openai reasoning 请求体修复(`max_completion_tokens`、
   省略 temperature)仍未验证。重启冒烟的方式见 §常用命令:设 `LLMJOIN_API_KEY`
   后跑 `devtools::check(cran = TRUE)`,前提是先用 `set_llm()` 恢复本地配置。
2. 【新增(261008),0.3.2 增至五个】五个需要凭据或需要已有配置的示例(`set_llm` /
   `chat_llm` / `build_joint` / `llm_join` / `get_llm`)在 CRAN 检查机上永不执行——
   `@examplesIf` 守卫比 `\donttest` 更严。`get_llm()` 不读环境变量里的凭据,但无配置时
   会报错,裸 `\examples{}` 会让 `R CMD check` 直接 ERROR,故沿用同一守卫(示例先 `set_llm()`)。
   审核人 2026-06 第 3 条要求换掉 `\dontrun` 的理由(隐藏 bug 不被发现)在此以新形式
   回归:0.3.0 那次正是靠执行示例才发现 `llm_join` 里 `model` → `.model` 的参数名错误。
   缓解:`joint_prompt()` 与 `parse_joint()` 两个不依赖 key 的示例仍是普通 `\examples{}`,
   每次 check 都执行;`get_llm()` 的读取与脱敏行为另有 testthat 断言覆盖
   (`tests/testthat/test-get_llm.R`),不依赖示例执行。
3. 【待重写(261008)】`cran-comments.md` 现内容仍是 0.3.0 的发布摘要,未覆盖 DeepSeek
   provider、`readr` 移除、防伪造校验、示例守卫、0.3.2 的 `get_llm()` 五项;投稿前必须重写。
4. 【已提交待响应(261007,工单 #4830894)】GitHub 服务端缓存:261007 实测 6 个旧
   提交 SHA 中 5 个已不可达,仅首变更提交 b9b1c4b 仍按 SHA 直链可访问(未 GC);
   工单经支持门户 AI 预检转人工提交(路径见 handoff/261007_支持工单提交.md),草稿
   见 handoff/261007_GitHub缓存支持请求.md 附录;截至 261008 12:00 无回复,按拍板
   不重复开票。Support 回复后:执行则复测 6 个 SHA 并关闭本项,以非敏感数据为由
   拒绝则回退为等待服务端 GC。

## 常用命令

```bash
Rscript -e "testthat::test_local('.')"          # 全量测试(从源码加载,勿用 library(llmjoin))
Rscript -e 'devtools::test_active_file("tests/testthat/test-parse_joint.R")'
Rscript -e 'devtools::load_all(".")'            # 交互开发加载
Rscript -e 'devtools::check()'                  # 完整 R CMD check
Rscript -e 'devtools::check(cran = TRUE)'       # 投稿口径(= R CMD check --as-cran)
LLMJOIN_API_KEY=<your-key> Rscript -e 'devtools::check(cran = TRUE)'   # 带 key 冒烟:五个受守卫的示例会真实执行
Rscript -e 'devtools::document()'               # 生成 man/ 文档
Rscript -e 'devtools::install()'                # 本地安装(保持与源码同步)
```

**跑 check 前先清 locale 变量**:Git Bash 导出的 `LC_ALL` / `LC_CTYPE` 等 `C.UTF-8`
值会让 Windows R 启动报错,被 check 记成假的 ERROR/WARNING。用
`env -u LC_ALL -u LANG -u LC_CTYPE -u LC_COLLATE Rscript ...`。

`--as-cran` **会执行示例**(含 `\donttest`,261008 实测确认),所以 §已拍板口径⑦ 的
`@examplesIf` 守卫不得移除——否则会把维护者本地的 `LLMJOIN.yml` 覆盖成占位符,
并向已配置的 provider 发出真实请求。

## 交接记录约定(handoff/)

遵循 project-conventions 技能的 handoff 协议(261007 起;协议原型见
mattpocock/skills 的 handoff 技能):**一份交接文档 = 一个工作会话的收口快照**,
让下一个 agent 不带上下文也能接续。`handoff/` 兼任项目时间线,不建 HISTORY.md
(见 §已拍板口径①)。

**命名**:`<yyMMdd>_<主题>.md`。日期领衔,按文件名排序即时间序。
- `<主题>`:简短中文短语,不含空格,词间用连字符;同日多份靠主题区分,仍冲突加 `-HHmm`。
- 示例:`261011_修复llm_join合并键.md`。
- 历史兼容:2026-10-06 前的记录原以 `[状态]-主题-yymmdd.md` 命名,261007 已批量重命名
  为日期领衔格式,**内容一字未改**。

**时机**:开工即建当次文档,随做随记;会话收口补全定稿。定稿后**只增不改**;
勘误写在下一份 handoff 的「更正」里。

**结构**(7 节,节名可按需合并但不可缺验证结果):

```markdown
# Handoff <yyMMdd> — <主题>

## 0. 元信息
- 收口时间:<yyyy-MM-dd HH:mm>
- 上一份交接:handoff/<yyMMdd>_<主题>.md(指针以实际前一份为准)
- 下一会话焦点:<未指定写「未指定」>

## 1. 改动清单(每改动一行)
- **<一句话动作>**:<做了什么>;动机:<为什么>;影响面:<文件/产物清单>。

## 2. 验证结果
- <测试/QC/审计结果,带数值;未验证如实标注「未验证」>。

## 3. 口径变化
- <无则写「无」;有则新旧对照一行,并注明总纲已同步>。

## 4. 遗留问题与下一步
- <缺口/未收口事项/风险/下一步动作>。

## 5. 建议技能(下一会话调用)
- `<skill-name>`:<一句为什么>。

## 6. 指针
- <产物路径、commit 哈希、相关文档——只给指针,不复制内容>。
```

**内容纪律**:
- 引用而不复制:总纲、其他记录、commit diff 里已有的内容给路径或哈希,不重述;
- 敏感信息一律脱敏(API key、口令不落 handoff,凭据只留占位与指针);
- 写不出验证结果 = 改动尚未收口,先回验证再定稿。

**git 纪律**:收口 handoff 必须提交入库(与实施改动同一提交,或紧随其后的独立提交)。
`.gitignore` 不得排除 `handoff/`;`.Rbuildignore` 维持排除(不进 R CMD 构建)。
收口时同步核对:根 §已知问题、`R/` 与 `tests/` AGENTS.md 的清单与基线数字是否需要更新。

**接续纪律**:下一会话开工先读根总纲 + `handoff/` 文件名排序最后一份(同日多份时以
元信息「上一份交接」指针回溯为准);发现记载与现实冲突,以现实为准,冲突点记入下一份
handoff。悬而未决、待维护者拍板的事项必须登记进根 §已知问题或 §已拍板口径,
不允许只活在 handoff 遗留节。
