# 实验五：消费电子品牌不同城市商圈的价格接受度分析。
# 数据为独立生成的模拟样本，评分范围为0—100，分数越高表示价格接受度越高。
# 分析流程：生成数据 -> 描述统计 -> 前提检查 -> 平方和分解 -> F检验 -> Tukey比较。
# 运行环境：Windows、R 4.4.1、RStudio；全部计算与绘图只使用R自带功能。

if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) { # 检查中文编码环境。
  invisible(Sys.setlocale("LC_CTYPE", "Chinese (Simplified)_China.utf8")) # 设置Windows中文UTF-8区域。
} # 结束编码设置。

# -------------------- 0. 设置结果保存位置 --------------------
args <- commandArgs(trailingOnly = TRUE) # 读取可选的输出目录参数。
file_arg <- grep("^--file=", commandArgs(), value = TRUE) # 查找命令行脚本路径。
source_files <- lapply(sys.frames(), function(frame) frame$ofile) # 获取Source运行时的文件路径。
source_files <- Filter(function(x) is.character(x) && length(x) == 1L, source_files) # 保留有效路径。
script_path <- if (length(file_arg)) { # 判断是否从命令行运行。
  sub("^--file=", "", file_arg[1]) # 提取命令行中的R文件名。
} else if (length(source_files)) { # 判断是否使用RStudio的Source功能。
  source_files[[length(source_files)]] # 读取当前源文件名。
} else { # 兼容在控制台选中代码运行的情况。
  file.path(getwd(), "test5.R") # 将工作目录作为默认脚本目录。
} # 结束脚本路径选择。
script_dir <- dirname(normalizePath(script_path, mustWork = FALSE)) # 得到脚本所在目录。
output_dir <- if (length(args)) args[1] else file.path(script_dir, "test5_results") # 确定输出目录。
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE) # 创建结果文件夹。
alpha <- 0.05 # 预先设置显著性水平。

# -------------------- 1. 生成240名模拟消费者的数据 --------------------
RNGkind("Mersenne-Twister", "Inversion", "Rejection") # 固定随机数算法。
set.seed(20261023) # 固定一次随机种子，便于复现。
n_each <- 80L # 设置每个商圈的样本量。
district_levels <- c("A", "B", "C") # 定义三个城市商圈的编码。
population_means <- c(A = 70, B = 66, C = 62) # 预先设定三组总体均值。
population_sd <- 10 # 预先设定共同总体标准差。
city_district <- factor(rep(district_levels, each = n_each), levels = district_levels) # 构造分组变量。
price_acceptance <- round(rnorm(3 * n_each, mean = rep(population_means, each = n_each), # 生成各商圈对应的正态随机评分。
                                sd = population_sd), 1) # 独立抽取正态评分并保留一位小数。
survey <- data.frame(user_id = sprintf("U%03d", seq_along(price_acceptance)), # 创建包含唯一消费者编号的原始数据表。
                     city_district, price_acceptance) # 将编号、商圈与评分整理成长表。
stopifnot(nrow(survey) == 240L, !anyNA(survey), !anyDuplicated(survey$user_id), # 检查样本量、缺失值和编号唯一性。
          all(table(survey$city_district) == n_each), # 逐组核对80人的样本量。
          all(survey$price_acceptance >= 0 & survey$price_acceptance <= 100)) # 核对样本数、唯一编号和取值。
# 范围检查只负责发现错误；程序没有按检验结果筛选样本、重抽数据或更换随机种子。
write.csv(survey, file.path(output_dir, "price_acceptance_simulated.csv"), # 将全部原始数据写入CSV文件。
          row.names = FALSE, fileEncoding = "UTF-8") # 保存全部模拟观测。
generation <- data.frame(city_district = district_levels, n = n_each, # 整理三个商圈的预设生成参数。
                          population_mean = population_means, population_sd) # 记录事先设定的参数。

# -------------------- 2. 描述各组评分 --------------------
group_scores <- split(survey$price_acceptance, survey$city_district) # 按商圈拆分评分。
group_n <- vapply(group_scores, length, integer(1)) # 计算各组实际样本量。
group_mean <- vapply(group_scores, mean, numeric(1)) # 计算各组样本均值。
group_sd <- vapply(group_scores, sd, numeric(1)) # 计算各组样本标准差。
descriptive <- data.frame(city_district = names(group_n), n = unname(group_n), # 整理各组商圈编码与实际人数。
  mean = unname(group_mean), sd = unname(group_sd), # 添加各组样本均值与标准差。
  median = vapply(group_scores, median, numeric(1)), # 添加各组样本中位数。
  min = vapply(group_scores, min, numeric(1)), # 添加各组最小评分。
  max = vapply(group_scores, max, numeric(1)), row.names = NULL) # 汇总集中趋势与离散程度。
N <- nrow(survey) # 计算总样本量。
k <- nlevels(survey$city_district) # 计算因素水平数。
grand_mean <- mean(survey$price_acceptance) # 计算所有消费者的总均值。
anova_model <- aov(price_acceptance ~ city_district, data = survey) # 拟合单因素方差分析模型。

# -------------------- 3. 检查分析前提 --------------------
# 独立性来自数据生成设计：每行代表一名不同消费者，每个评分独立抽取。
normality <- do.call(rbind, lapply(names(group_scores), function(g) { # 依次检查三个组。
  sw <- shapiro.test(group_scores[[g]]) # 对该组进行Shapiro-Wilk正态性检验。
  data.frame(city_district = g, W = unname(sw$statistic), p_value = sw$p.value) # 整理该组结果。
})) # 合并三组的检验输出。
group_median <- ave(survey$price_acceptance, survey$city_district, FUN = median) # 计算每行所属组的中位数。
survey$absolute_deviation <- abs(survey$price_acceptance - group_median) # 计算到组中位数的绝对离差。
levene_model <- aov(absolute_deviation ~ city_district, data = survey) # 比较绝对离差的组间均值。
levene_table <- summary(levene_model)[[1]] # 提取以中位数为中心的Levene检验结果。
homogeneity <- data.frame(test = "Median-centered Levene", df_between = k - 1L, # 整理方差齐性检验名称与分子自由度。
  df_within = N - k, F = levene_table[1, "F value"], # 添加分母自由度和Levene检验统计量。
  p_value = levene_table[1, "Pr(>F)"]) # 整理方差齐性检验。

# -------------------- 4. 手工分解平方和并计算F检验 --------------------
SST <- sum((survey$price_acceptance - grand_mean)^2) # 计算总平方和。
SSA <- sum(group_n * (group_mean - grand_mean)^2) # 按样本量加权计算组间平方和。
fitted_mean <- group_mean[as.character(survey$city_district)] # 找到每名消费者所属组的均值。
SSE <- sum((survey$price_acceptance - fitted_mean)^2) # 计算组内误差平方和。
df_between <- k - 1L # 计算组间自由度。
df_within <- N - k # 计算组内自由度。
MSA <- SSA / df_between # 用组间平方和除以组间自由度。
MSE <- SSE / df_within # 用组内平方和除以组内自由度。
F_value <- MSA / MSE # 计算组间均方与组内均方的比值。
F_critical <- qf(1 - alpha, df_between, df_within) # 求F分布的右尾临界值。
p_anova <- pf(F_value, df_between, df_within, lower.tail = FALSE) # 计算右尾p值。
reject_H0 <- p_anova < alpha # 判断是否拒绝三组总体均值相等的零假设。
eta_squared <- SSA / SST # 计算组间平方和占总平方和的比例。
software_table <- summary(anova_model)[[1]] # 读取aov的标准输出以便核对。
stopifnot(isTRUE(all.equal(SST, SSA + SSE, tolerance = 1e-10)), # 核对平方和分解恒等式。
          isTRUE(all.equal(F_value, unname(software_table[1, "F value"]), tolerance = 1e-10)), # 核对手工F值与aov输出一致。
          isTRUE(all.equal(p_anova, unname(software_table[1, "Pr(>F)"]), tolerance = 1e-10))) # 核对平方和及软件计算。
anova_table <- data.frame(source = c("Between", "Within", "Total"), # 按组间、组内和总体组织方差分析表。
  SS = c(SSA, SSE, SST), df = c(df_between, df_within, N - 1L), # 添加平方和与自由度。
  MS = c(MSA, MSE, NA_real_), F = c(F_value, NA_real_, NA_real_), # 添加均方与F统计量。
  p_value = c(p_anova, NA_real_, NA_real_)) # 按变异来源整理标准方差分析表。
overall_result <- data.frame(N, k, grand_mean, F_value, df_between, df_within, # 汇总样本设定与总体检验统计量。
  F_critical, p_value = p_anova, reject_H0, eta_squared) # 汇总检验决策与变异比例。

# -------------------- 5. 显著后进行Tukey多重比较 --------------------
tukey <- data.frame() # 为总体检验不显著的情况保留空结果。
q_critical <- qtukey(1 - alpha, nmeans = k, df = df_within) # 求学生化极差分布的临界值。
HSD <- q_critical * sqrt(MSE / n_each) # 计算本平衡设计的统一差值界限。
if (reject_H0) { # 总体F检验显著时再定位差异。
  tukey_matrix <- TukeyHSD(anova_model, "city_district", conf.level = 1 - alpha)$city_district # 计算全部组对。
  tukey <- data.frame(comparison = rownames(tukey_matrix), # 按组对名称建立事后比较结果表。
    difference = tukey_matrix[, "diff"], lower = tukey_matrix[, "lwr"], # 提取均值差与区间下限。
    upper = tukey_matrix[, "upr"], p_adjusted = tukey_matrix[, "p adj"], # 提取区间上限与校正后的p值。
    reject_equal_means = tukey_matrix[, "p adj"] < alpha, row.names = NULL) # 保存差值、同时区间和校正p值。
  stopifnot(all(abs((tukey$upper - tukey$lower) / 2 - HSD) < 1e-8)) # 核对所有区间半宽均等于HSD。
} # 结束事后比较。

# -------------------- 6. 保存表格与文字结果 --------------------
tables <- list(generation_parameters = generation, descriptive = descriptive, # 汇总数据生成与描述统计结果表。
  normality = normality, homogeneity = homogeneity, anova = anova_table, # 添加前提检查与方差分析表。
  overall = overall_result, tukey = tukey, # 添加总体判断与多重比较结果。
  tukey_threshold = data.frame(q_critical, HSD)) # 将不同分析阶段的结果分别命名。
for (name in names(tables)) { # 逐个保存结果表。
  write.csv(tables[[name]], file.path(output_dir, paste0(name, ".csv")), # 指定当前结果表的CSV保存路径。
            row.names = FALSE, fileEncoding = "UTF-8", na = "") # 导出UTF-8格式CSV。
} # 结束结果表保存。
result_text <- capture.output({ # 同时生成便于阅读的控制台记录。
  cat("数据性质：模拟数据；A/B/C分别代表高消费力、中等消费力、下沉市场商圈。\n") # 说明分组含义。
  cat("设定：独立抽样；每组80人；均值70/66/62；共同标准差10；seed=20261023。\n\n") # 记录生成规则。
  for (name in names(tables)) { # 按分析顺序列出各表。
    cat("\n", name, "\n", sep = "") # 打印结果标题。
    print(tables[[name]], row.names = FALSE, digits = 9) # 打印实际计算值。
  } # 结束结果循环。
  cat("\n标准aov输出：\n") # 标记软件原始输出。
  print(summary(anova_model)) # 输出标准方差分析表。
  cat("\n结论：", if (reject_H0) "拒绝三组总体均值相等的零假设。" else "不拒绝三组总体均值相等的零假设。", "\n") # 输出检验结论。
}) # 结束输出记录。
writeLines(result_text, file.path(output_dir, "results.txt"), useBytes = TRUE) # 保存可读结果。
cat(paste(result_text, collapse = "\n"), "\n") # 在RStudio控制台同步显示结果。
writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt")) # 保存R版本与环境信息。

# -------------------- 7. 绘制结果概览与诊断图 --------------------
# 图形合同：正文170 mm宽；颜色用于商圈区分；保留全部观测；PDF文字可编辑。
# a展示组间位置与组内离散；b展示经过Tukey校正的组对均值差及同时95%区间。
palette <- c(A = "#327D92", B = "#9A759D", C = "#BE8554") # 设置三个商圈的固定配色。
ink <- "#283444" # 设置文字和坐标轴颜色。
grid_col <- "#E4E9EE" # 设置浅灰参考线。
font_family <- if (.Platform$OS.type == "windows") "Microsoft YaHei" else "sans" # 选择中文字体。
figure_width_mm <- 170 # 使图像宽度与报告正文一致。
figure_height_mm <- 92 # 设置两个横向面板的高度。
set.seed(20261024) # 固定散点横向抖动，与数据生成种子分开。
jitter_x <- as.integer(survey$city_district) + runif(N, -0.16, 0.16) # 仅移动横坐标以显示重叠观测。

plot_bbox <- function() { # 测量当前面板的最终绘图区。
  f <- par("fig"); p <- par("plt"); d <- dev.size("in") * 72 # 获取设备与绘图区参数。
  c((f[1] + p[1] * diff(f[1:2])) * d[1], (f[3] + p[3] * diff(f[3:4])) * d[2], # 将左、下边界转换为物理点坐标。
    (f[1] + p[2] * diff(f[1:2])) * d[1], (f[3] + p[4] * diff(f[3:4])) * d[2]) # 转为物理点坐标。
} # 结束坐标测量。
save_alignment <- function(boxes, stem) { # 保存后续排版检查所需的实际几何尺寸。
  panels <- vapply(seq_along(boxes), function(i) { # 为每个面板整理几何信息。
    b <- boxes[[i]] # 读取面板边界。
    sprintf('{"id":"%s","bbox_pt":[%s],"panel_label_anchor_pt":[%.4f,%.4f],"grid_id":"main","row_start":0,"row_stop":1,"col_start":%d,"col_stop":%d}', # 建立含面板边界与布局信息的JSON格式。
      letters[i], paste(sprintf("%.4f", b), collapse = ","), b[1], b[4] + 17, i - 1, i) # 记录边界与标签锚点。
  }, character(1)) # 将每个面板转换为JSON文本。
  manifest <- sprintf('{"schema_version":1,"figure":{"width_pt":%.4f,"height_pt":%.4f},"panels":[%s],"row_groups":[["a","b"]],"column_groups":[]}', # 建立图像尺寸与面板分组的JSON格式。
    dev.size("in")[1] * 72, dev.size("in")[2] * 72, paste(panels, collapse = ",")) # 组合两个横向面板。
  writeLines(manifest, file.path(output_dir, paste0(stem, ".alignment-layout.json"))) # 保存测量记录。
} # 结束几何信息保存。
setup_panels <- function() { # 统一两个面板的字号与留白。
  par(mfrow = c(1, 2), mar = c(3.8, 4.4, 3.4, 1.1), oma = c(0.2, 0, 0, 0), # 设置一行两列布局及相同面板边距。
      family = font_family, cex = 0.87, cex.axis = 0.9, cex.lab = 1, # 设置中文字体及正文和轴标签的相对字号。
      col.axis = ink, col.lab = ink, fg = ink, las = 1, tcl = -0.22, # 设置坐标轴颜色、刻度方向和长度。
      mgp = c(2.5, 0.65, 0), xaxs = "i", yaxs = "i", bty = "l") # 固定相同的绘图区边距。
} # 结束面板设置。
draw_overview <- function(record_geometry = FALSE) { # 绘制全部数据与组对比较。
  setup_panels() # 应用统一样式。
  boxes <- list() # 准备保存两个面板的实际边界。
  y_range <- range(survey$price_acceptance) + c(-4, 7) # 为原始观测与图内说明预留空间。
  plot(NA, xlim = c(0.45, 3.55), ylim = y_range, xaxt = "n", yaxt = "n", # 设置评分分布面板的坐标范围。
       xlab = "城市商圈", ylab = "价格接受度评分") # 建立分布面板坐标轴。
  yt <- pretty(y_range); yt <- yt[yt >= y_range[1] & yt <= y_range[2]] # 选择评分轴刻度。
  abline(h = yt, col = grid_col, lwd = 0.6) # 添加水平阅读参考线。
  boxplot(group_scores, at = 1:3, add = TRUE, axes = FALSE, outline = FALSE, # 叠加三个商圈的箱线图。
          col = adjustcolor(palette, 0.18), border = palette, boxwex = 0.52, # 设置箱体填充、边框与宽度。
          medlwd = 1.4, whisklty = 1, staplewex = 0.5) # 展示中位数、四分位数与须线。
  points(jitter_x, survey$price_acceptance, pch = 16, cex = 0.43, # 叠加每名消费者的原始评分。
         col = adjustcolor(palette[as.character(survey$city_district)], 0.38)) # 绘制全部240个观测。
  points(1:3, group_mean, pch = 23, bg = "white", col = palette, cex = 1.2, lwd = 1.3) # 用空心菱形突出样本均值。
  axis(1, at = 1:3, labels = c("A", "B", "C")) # 标出三个商圈编码。
  axis(2, at = yt, labels = yt) # 标出评分刻度。
  mtext("a  各商圈的评分分布", side = 3, line = 1.6, adj = 0, font = 2, cex = 1.02) # 添加面板标题。
  mtext("每组 n = 80；菱形为样本均值", side = 3, line = 0.25, adj = 0, cex = 0.84, col = "#607080") # 说明样本与中心统计量。
  boxes[[1]] <- plot_bbox() # 测量第一个绘图区。
  if (nrow(tukey)) { # 总体检验显著时绘制校正后的比较区间。
    xr <- range(c(tukey$lower, tukey$upper, 0)) # 根据全部区间确定横轴范围。
    xr <- xr + c(-1, 1) * diff(xr) * 0.13 # 为端点留出空间。
    plot(NA, xlim = xr, ylim = c(0.5, 3.5), xaxt = "n", yaxt = "n", # 设置均值差区间图的坐标范围。
         xlab = "前一组减后一组的均值差", ylab = "组对") # 建立均值差面板。
    xt <- pretty(xr); xt <- xt[xt >= xr[1] & xt <= xr[2]] # 选择均值差刻度。
    abline(v = xt[xt != 0], col = grid_col, lwd = 0.6) # 添加浅灰竖向参考线。
    abline(v = 0, col = "#8B96A2", lty = 2, lwd = 1) # 将零差值标记为判断参照。
    positions <- 3:1 # 按CSV顺序自上而下排列三个比较。
    segments(tukey$lower, positions, tukey$upper, positions, col = ink, lwd = 1.6) # 绘制同时95%区间。
    segments(tukey$lower, positions - 0.065, tukey$lower, positions + 0.065, col = ink) # 绘制区间左端帽。
    segments(tukey$upper, positions - 0.065, tukey$upper, positions + 0.065, col = ink) # 绘制区间右端帽。
    points(tukey$difference, positions, pch = 16, cex = 0.9, col = ink) # 标出均值差点估计。
    axis(1, at = xt, labels = xt) # 标出均值差刻度。
    axis(2, at = positions, labels = tukey$comparison, tick = FALSE) # 标出差值的方向。
  } else { # 总体检验不显著时保留明确说明。
    plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1)) # 创建空面板。
    text(0.5, 0.5, "总体 F 检验未显著\n本次不进行事后比较", col = ink) # 如实说明分析停止位置。
  } # 结束事后比较绘图。
  mtext("b  差异来自哪些商圈", side = 3, line = 1.6, adj = 0, font = 2, cex = 1.02) # 添加面板标题。
  mtext("Tukey 校正的同时 95% 区间", side = 3, line = 0.25, adj = 0, cex = 0.84, col = "#607080") # 说明区间控制口径。
  boxes[[2]] <- plot_bbox() # 测量第二个绘图区。
  if (record_geometry) save_alignment(boxes, "Rplot_test5") # 在最终PDF设备上保存实际几何尺寸。
} # 结束概览图函数。
draw_diagnostics <- function(record_geometry = FALSE) { # 绘制正态性与方差稳定性诊断。
  setup_panels() # 应用相同的两面板布局。
  boxes <- list() # 保存两个诊断面板的边界。
  residual <- residuals(anova_model) # 获取消费者评分相对所属组均值的残差。
  qq <- qqnorm(residual, plot.it = FALSE) # 计算正态Q-Q图的理论与实际分位数。
  qq_xlim <- range(qq$x) + c(-1, 1) * diff(range(qq$x)) * 0.05 # 为两端分位数预留边距。
  qq_ylim <- range(qq$y) + c(-1, 1) * diff(range(qq$y)) * 0.06 # 保留最大与最小残差的完整标记。
  plot(qq$x, qq$y, xlim = qq_xlim, ylim = qq_ylim, pch = 16, cex = 0.47, col = adjustcolor(palette[1], 0.65), # 绘制带边距的正态分位数散点。
       xlab = "标准正态理论分位数", ylab = "模型残差") # 绘制全部残差的Q-Q点。
  qqline(residual, col = "#BE8554", lwd = 1.3) # 根据四分位数添加正态参考直线。
  mtext("a  残差正态 Q-Q 图", side = 3, line = 1.6, adj = 0, font = 2, cex = 1.02) # 添加标题。
  mtext("点接近直线表示正态近似较合理", side = 3, line = 0.25, adj = 0, cex = 0.84, col = "#607080") # 说明读图方法。
  boxes[[1]] <- plot_bbox() # 记录Q-Q面板边界。
  plot(NA, xlim = c(0.45, 3.55), ylim = range(residual) * 1.12, xaxt = "n", # 设置按商圈展示残差的坐标范围。
       xlab = "城市商圈", ylab = "模型残差") # 建立残差散布坐标轴。
  abline(h = 0, col = "#8B96A2", lty = 2) # 标出零残差参照线。
  points(jitter_x, residual, pch = 16, cex = 0.5, # 按商圈配色绘制全部残差。
         col = adjustcolor(palette[as.character(survey$city_district)], 0.55)) # 展示各组的全部残差。
  axis(1, at = 1:3, labels = district_levels) # 标记三个商圈。
  mtext("b  各商圈的残差散布", side = 3, line = 1.6, adj = 0, font = 2, cex = 1.02) # 添加标题。
  mtext("比较三组的垂直离散程度", side = 3, line = 0.25, adj = 0, cex = 0.84, col = "#607080") # 说明方差齐性读图方法。
  boxes[[2]] <- plot_bbox() # 记录残差散布面板边界。
  if (record_geometry) save_alignment(boxes, "Rplot_test5_diagnostics") # 保存诊断图的几何尺寸。
} # 结束诊断图函数。
for (stem in c("Rplot_test5", "Rplot_test5_diagnostics")) { # 分别导出结果图与诊断图。
  drawer <- if (stem == "Rplot_test5") draw_overview else draw_diagnostics # 选择对应绘图函数。
  cairo_pdf(file.path(output_dir, paste0(stem, ".pdf")), width = figure_width_mm / 25.4, # 设置可编辑PDF的保存路径与物理宽度。
            height = figure_height_mm / 25.4, family = font_family, pointsize = 10) # 建立可编辑矢量PDF。
  drawer(TRUE) # 绘图并测量最终物理尺寸。
  invisible(dev.off()) # 关闭PDF设备。
  png(file.path(output_dir, paste0(stem, ".png")), width = figure_width_mm, # 设置高清PNG的保存路径与物理尺寸。
      height = figure_height_mm, units = "mm", res = 500, type = "cairo", pointsize = 10) # 建立高清预览设备。
  drawer(FALSE) # 绘制与PDF相同的数据内容。
  invisible(dev.off()) # 关闭PNG设备。
} # 结束图像导出。
cat("\n结果已保存至：", normalizePath(output_dir), "\n") # 显示实际结果位置。
