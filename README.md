# 管理统计与机器学习实验

本仓库整理六个课程实验的 R 代码。实验一、三、四、五、六由脚本生成模拟数据，实验二读取本地问卷 CSV。

| 实验 | 主脚本 | 内容 |
| --- | --- | --- |
| 一 | [test_1.R](管理统计与机器学习实验/test_1.R) | 电商 A/B 测试，Welch t 检验、置换检验和 Bootstrap 区间 |
| 二 | [test_2.R](管理统计与机器学习实验/test_2.R) | 电商用户满意度的均值和高满意度比例估计 |
| 三 | [test3.R](管理统计与机器学习实验/test3.R) | 贝叶斯线性回归，Gibbs 采样和后验预测 |
| 四 | [test4.R](管理统计与机器学习实验/test4.R) | 高端宠物营养品定价，模拟用户支付、订单营收和五轮两折预测 |
| 五 | [test5.R](管理统计与机器学习实验/test5.R) | 商圈价格接受度，单因素方差分析、残差诊断与 Tukey 比较 |
| 六 | [test6.R](管理统计与机器学习实验/test6.R) | 数字化投入与 ROA，模拟企业面板、固定效应及异质性分析 |

## 运行

代码以 R 4.4.1 验证。将 RStudio 的工作目录设为 `管理统计与机器学习实验`，以 UTF-8 打开所需脚本并点击 **Source**。也可以在该目录下用命令行运行，例如 `Rscript --encoding=UTF-8 test4.R`。

实验一至五使用基础 R。实验六需要安装以下扩展包。

```r
install.packages(c("plm", "glmnet", "sandwich", "lmtest"))
```

实验二需要包含 200 条记录的 `ecommerce_survey.csv`。其中 `satisfaction_score` 应为 1 至 10 的数值，输入表应无缺失值。可以把 CSV 放在当前工作目录，也可以在运行前指定路径。

```r
Sys.setenv(ECOMMERCE_SURVEY_FILE = "data/ecommerce_survey.csv")
source("test_2.R", encoding = "UTF-8")
```

实验三的诊断图写入当前工作目录的 `test3_results`，可用环境变量 `EXPERIMENT3_OUTPUT_DIR` 指定其他目录。更多说明见[实验三说明](管理统计与机器学习实验/README_test3.md)。

实验四的 `test4.R` 对应课堂终稿的逐用户模拟与预测流程。样本订单营收按模拟支付记录计算，五轮两折的预测金额采用综合评分换算，t 值和右尾概率沿用案例参考计算。RStudio 的 Plots 面板可查看图形。

实验五和六默认将结果保存到脚本旁的 `test5_results`、`test6_results`。实验六自动调用 [test6_figures.R](管理统计与机器学习实验/test6_figures.R)，请将两个脚本放在同一目录。

## 报告图件

先运行实验五与六，再运行 [report_figures.R](管理统计与机器学习实验/report_figures.R)。该脚本执行实验四代码，复用实验五与六的结果数据，并导出实验四至六的独立 PDF、600 dpi PNG 和实验四数据表。已安装 `svglite` 时还会导出 SVG。绘图需要 R 的 Cairo 支持。

```bash
Rscript --encoding=UTF-8 test5.R
Rscript --encoding=UTF-8 test6.R
Rscript --encoding=UTF-8 report_figures.R
```

图件位于 `Latex_paper/figures_exp4`、`Latex_paper/figures_exp5` 和 `Latex_paper/figures_exp6`，实验四的数据表位于 `test4_results_final`。固定种子与分析步骤在相应脚本中。
