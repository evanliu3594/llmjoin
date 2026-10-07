# [DONE] 引入project-conventions分层约束 (261004)

## 背景
维护者要求参考 project-conventions 技能推动项目多层级约束。该技能的 brownfield 程序要求:
考古盘点 → 与用户对齐 → 登记现状,旧 AGENTS.md 按"验证后继承"处置,禁止模板覆盖。

## 做了什么
- **考古盘点**(一手证据:本轮通读源码/测试、审核记录、git log):根 AGENTS.md 与代码同步、
  handoff/ 已承载留痕、.Rbuildignore 已排除根 AGENTS.md 与 handoff/、旧协作说明文件已按
  单一事实源处置——判定为"验证后继承,增量维护"。
- **与用户对齐(三项拍板,已登记总纲 §已拍板口径)**:
  1. 时间线载体:handoff/ 兼任项目时间线,**不建 HISTORY.md**(偏离技能单一时间线设计,
     理由:轮次记录已承载留痕职能,避免双写);
  2. 目录级文档:建 R/、tests/ 两份 AGENTS.md,其余目录不建;
  3. 口径登记:"提交/文档/交接记录不点名开发辅助工具"入总纲(provider 功能语境除外)。
- **判定不适用**(经维护者确认可不考虑):目录契约(data/output/pics 等)、deprecated/ 归档
  目录(NEWS.md + git 承载)、diagnosis/ 考古报告(已有审核 handoff 记录)、HDD/闸门体系
  (P1 BDD 等价承载)。
- **经验晋升**(技能管线:留痕 → 会再咬人的晋升为带疤规则):
  - merge NA 键按字符串匹配(已验证技术事实)→ R/AGENTS.md 要点;
  - 子代理超时两次接手 → 根 P1.6 带疤规则;
  - expect_match 需 fixed=TRUE(| 元字符)→ tests/AGENTS.md 规则。
- 根 AGENTS.md 增量维护:§项目定位与当前状态、§目录结构表、§已拍板口径(①②③)、
  P1.0 先问后做、P1.6 带疤、P3 写作语言、架构 llm_join 行同步、交接约定注时间线职能。

## 变更文件
- AGENTS.md:新增三节 + P1/P3 补规则 + 架构行同步(增量,未覆盖既有骨架)
- R/AGENTS.md(新)、tests/AGENTS.md(新):职责/规则/当前清单(三态)/特有要点
- .Rbuildignore:排除 ^R/AGENTS\.md$、^tests/AGENTS\.md$(CRAN 构建洁净)

## 验证
- 文档变更,无代码改动:测试基线不变(121 断言全绿,本轮未重跑,上轮 253f900 后未动代码)。
- 总纲与目录级文件交叉核对:无双写(管线细节只在根 §架构,目录级只放职责/清单/要点)。
- .Rbuildignore 模式与实际文件名逐一核对。

## 遗留与下一步
- AGENTS 体系三份文件目前均不入 git 追踪(沿用根 AGENTS.md 未跟踪的现状);是否纳入版本
  库由维护者决定(涉及口径③的边界——文件含"AI 助手"字样但不含具体工具名)。
- 后续每轮收尾:除 handoff 记录外,同步更新 R//tests/AGENTS.md 的当前清单与基线数字
  (清单随建随记,不攒)。
- project-conventions 技能的适配结论以本记录 + 总纲口径表为准,助手私有记忆仅作索引。
