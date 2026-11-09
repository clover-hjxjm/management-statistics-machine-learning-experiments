# 实验六绘图：只读取同一次分析的实际数据、系数和企业聚类协方差。
# 图形合同：170mm宽、中文无衬线字体、可编辑PDF、600dpi预览、全样本保留。
font_family <- if (.Platform$OS.type == "windows") "Microsoft YaHei" else "sans" # 设置中文绘图字体。
ink <- "#293746"; blue <- "#327D92"; amber <- "#BF8854"; muted <- "#6E7D8B" # 定义克制的配色。
width_mm <- 170; height_mm <- 94 # 确定最终物理尺寸。
crit <- qt(0.975, n_firms - 1) # 统一采用企业数量确定聚类近似临界值。
graphics_setup <- function() par(mfrow = c(1, 2), mar = c(4.2, 4.5, 3.4, 1.2), oma = c(0, 0, 0, 0), family = font_family, cex = 0.86, cex.axis = 0.9, col.axis = ink, col.lab = ink, fg = ink, bty = "l", las = 1, tcl = -0.2, mgp = c(2.65, 0.65, 0)) # 统一绘图区与文字间距。
geometry <- function() { # 获取当前绘图区真实物理尺寸。
  f <- par("fig"); p <- par("plt"); d <- dev.size("in") * 72 # 获取设备和面板参数。
  c((f[1] + p[1] * diff(f[1:2])) * d[1], (f[3] + p[3] * diff(f[3:4])) * d[2], (f[1] + p[2] * diff(f[1:2])) * d[1], (f[3] + p[4] * diff(f[3:4])) * d[2]) # 转换为物理点。
} # 结束测量函数。
save_geometry <- function(boxes, stem) { # 写出后台无关的面板对齐审查输入。
  items <- vapply(1:2, function(i) sprintf('{"id":"%s","bbox_pt":[%s],"panel_label_anchor_pt":[%.4f,%.4f],"grid_id":"main","row_start":0,"row_stop":1,"col_start":%d,"col_stop":%d}', letters[i], paste(sprintf("%.4f", boxes[[i]]), collapse = ","), boxes[[i]][1], boxes[[i]][4] + 17, i - 1, i), character(1)) # 记录两面板边界。
  manifest <- sprintf('{"schema_version":1,"figure":{"width_pt":%.4f,"height_pt":%.4f},"panels":[%s],"row_groups":[["a","b"]],"column_groups":[]}', dev.size("in")[1] * 72, dev.size("in")[2] * 72, paste(items, collapse = ",")) # 声明同一行的比较关系。
  writeLines(manifest, file.path(output_dir, paste0(stem, ".alignment-layout.json"))) # 保存最终设备测量记录。
} # 结束测量输出。
heading <- function(letter, title, subtitle) { # 统一面板标题的物理位置。
  mtext(paste(letter, title), side = 3, line = 1.6, adj = 0, cex = 1.02, font = 2) # 放置面板字母与标题。
  mtext(subtitle, side = 3, line = 0.25, adj = 0, cex = 0.8, col = muted) # 简要说明数据与区间口径。
} # 结束标题函数。
pad <- function(x, amount = 0.08) range(x) + c(-1, 1) * max(diff(range(x)), 0.1) * amount # 为标记和区间留出边距。
effect_grid <- function(model, V, x, soe = 0, size = 0, derivative = FALSE, reference = 3) { # 从模型的线性组合计算调整后曲线。
  b <- coef(model); L <- matrix(0, length(x), length(b), dimnames = list(NULL, names(b))) # 建立系数对比矩阵。
  L[, "dig_ratio"] <- if (derivative) 1 else x - reference # 设置一次项或一次项导数。
  L[, "dig2"] <- if (derivative) 2 * x else x^2 - reference^2 # 设置二次项或二次项导数。
  if ("dig_ratio:soe" %in% names(b)) L[, "dig_ratio:soe"] <- soe * (if (derivative) 1 else x - reference) # 添加所有制调节项。
  if ("dig_ratio:size_c" %in% names(b)) L[, "dig_ratio:size_c"] <- size * (if (derivative) 1 else x - reference) # 添加规模调节项。
  mean <- drop(L %*% b); se <- sqrt(pmax(0, rowSums((L %*% V) * L))) # 按企业聚类协方差传播误差。
  data.frame(x, mean, lower = mean - crit * se, upper = mean + crit * se) # 返回点估计和逐点95%区间。
} # 结束模型曲线计算。
draw_curve <- function(d, color, lty = 1) { # 使用透明区间与线型共同编码。
  polygon(c(d$x, rev(d$x)), c(d$lower, rev(d$upper)), border = NA, col = adjustcolor(color, 0.14)) # 绘制真实估计不确定性。
  lines(d$x, d$mean, col = color, lwd = 1.7, lty = lty) # 绘制估计均值曲线。
} # 结束曲线函数。
grid_x <- seq(min(dig_data$dig_ratio), max(dig_data$dig_ratio), length.out = 180) # 限于完整样本的观测支持范围。
curve_fe <- effect_grid(fe, Vfe, grid_x); marginal_fe <- effect_grid(fe, Vfe, grid_x, derivative = TRUE) # 计算主模型两种互补证据。
curve_source <- rbind(cbind(kind = "FE_relative_ROA", curve_fe), cbind(kind = "FE_marginal", marginal_fe)) # 整理图形源数据。
write.csv(curve_source, file.path(output_dir, "nonlinear_curve_source.csv"), row.names = FALSE) # 保存曲线与区间原值。

figure_sample <- function(record = FALSE) { # 描述投入变化与原始关系，不进行因果解释。
  graphics_setup(); boxes <- list() # 初始化两个相同网格面板。
  trend <- do.call(rbind, lapply(0:1, function(s) do.call(rbind, lapply(years, function(y) { # 逐所有制、逐年汇总全部企业。
    z <- dig_data$dig_ratio[dig_data$soe == s & dig_data$year == y]; e <- qt(0.975, length(z) - 1) * sd(z) / sqrt(length(z)) # 计算当年企业均值区间。
    data.frame(soe = s, year = y, mean = mean(z), lower = mean(z) - e, upper = mean(z) + e) # 保留均值与误差。
  })))) # 合并所有分组。
  plot(NA, xlim = pad(years, 0.03), ylim = pad(c(trend$lower, trend$upper), 0.18), xlab = "年份", ylab = "平均数字化投入强度（%）", xaxt = "n") # 建立年际背景坐标系。
  axis(1, at = c(2016, 2018, 2020, 2022)) # 控制年份刻度密度。
  for (s in 0:1) { # 绘制两类企业的全部年度均值。
    z <- trend[trend$soe == s, ]; color <- c(blue, amber)[s + 1] # 获取对应组与颜色。
    polygon(c(z$year, rev(z$year)), c(z$lower, rev(z$upper)), border = NA, col = adjustcolor(color, 0.13)) # 展示企业间均值95%区间。
    lines(z$year, z$mean, col = color, lwd = 1.6, lty = s + 1); points(z$year, z$mean, col = color, pch = c(16, 17)[s + 1], cex = 0.6) # 使用线型和点型区分组别。
  } # 结束年度曲线。
  legend("topleft", legend = c("非国企", "国企"), col = c(blue, amber), lty = 1:2, lwd = 1.5, bty = "n", cex = 0.8) # 解释两类企业。
  heading("a", "数字化投入的年际变化", "模拟企业；阴影为均值 95% 区间"); boxes[[1]] <- geometry() # 记录面板A。
  plot(dig_data$dig_ratio, dig_data$roa, pch = 16, cex = 0.28, col = adjustcolor(blue, 0.19), xlim = pad(grid_x), ylim = pad(dig_data$roa), xlab = "数字化投入强度（%）", ylab = "ROA（%）") # 显示完整2800个观测。
  heading("b", "投入与绩效的原始分布", "400 家企业 × 7 年；全部 2800 个观测"); boxes[[2]] <- geometry() # 注明原始关系与样本量。
  if (record) {save_geometry(boxes, "Rplot_test6_sample"); write.csv(trend, file.path(output_dir, "ownership_year_source.csv"), row.names = FALSE)} # 保存几何与源数据。
} # 结束样本背景图。
figure_effect <- function(record = FALSE) { # 展示控制企业与年份后的投入关系。
  graphics_setup(); boxes <- list() # 初始化两面板。
  plot(NA, xlim = pad(grid_x), ylim = pad(c(curve_fe$lower, curve_fe$upper)), xlab = "数字化投入强度（%）", ylab = "相对投入 3% 的 ROA 变化（百分点）") # 明确相对变化的参照。
  abline(h = 0, col = "#CAD2DA", lty = 2); draw_curve(curve_fe, blue) # 绘制模型条件对比。
  abline(v = peak[1], col = amber, lty = 3, lwd = 1) # 标出计算所得转折点。
  heading("a", "固定效应模型的非线性关系", sprintf("企业与年份效应；转折点 %.2f%%", peak[1])); boxes[[1]] <- geometry() # 给出图形判断依据。
  plot(NA, xlim = pad(grid_x), ylim = pad(c(marginal_fe$lower, marginal_fe$upper)), xlab = "数字化投入强度（%）", ylab = "边际关联（ROA 百分点 / 投入百分点）") # 建立导数坐标系。
  abline(h = 0, col = muted, lty = 2); draw_curve(marginal_fe, blue) # 展示导数何时由正变负。
  heading("b", "投入增量对应的边际关联", "阴影为企业聚类标准误的逐点 95% 区间"); boxes[[2]] <- geometry() # 标明不确定性口径。
  if (record) save_geometry(boxes, "Rplot_test6_effect") # 保存最终面板测量。
} # 结束非线性关系图。
figure_heterogeneity <- function(record = FALSE) { # 比较所有制和规模条件下的模型关联。
  graphics_setup(); boxes <- list() # 初始化两个面板。
  lo <- max(tapply(dig_data$dig_ratio, dig_data$soe, min)); hi <- min(tapply(dig_data$dig_ratio, dig_data$soe, max)) # 使用两类企业共有支持范围。
  xx <- seq(lo, hi, length.out = 180); sizes <- quantile(dig_data$size_c, c(0.25, 0.75)) # 确定共同网格和规模代表值。
  ownership <- lapply(0:1, function(s) effect_grid(hetero, Vhet, xx, soe = s, size = 0)) # 在相同规模下比较所有制。
  scale_curves <- lapply(sizes, function(z) effect_grid(hetero, Vhet, xx, soe = 0, size = z)) # 在相同所有制下比较规模。
  for (i in 1:2) { # 使用相同区间定义绘制两个问题。
    curves <- if (i == 1) ownership else scale_curves # 选择当前对比曲线。
    yy <- unlist(lapply(curves, function(z) c(z$lower, z$upper))) # 计算包含全部区间的轴范围。
    plot(NA, xlim = pad(xx), ylim = pad(yy), xlab = "数字化投入强度（%）", ylab = "相对投入 3% 的 ROA 变化（百分点）") # 保持纵轴含义一致。
    abline(h = 0, col = "#CAD2DA", lty = 2); draw_curve(curves[[1]], blue); draw_curve(curves[[2]], amber, 2) # 绘制两组点估计及区间。
    legend("bottomleft", legend = if (i == 1) c("非国企；规模对数 22", "国企；规模对数 22") else c("规模第 25 百分位", "规模第 75 百分位"), col = c(blue, amber), lty = 1:2, lwd = 1.5, bty = "n", cex = 0.75) # 说明固定的比较条件。
    heading(letters[i], if (i == 1) "所有制的调节关系" else "企业规模的调节关系", "统一交互模型；企业聚类逐点 95% 区间"); boxes[[i]] <- geometry() # 测量实际坐标。
  } # 结束异质性比较。
  if (record) {save_geometry(boxes, "Rplot_test6_heterogeneity"); write.csv(rbind(cbind(group = "nonSOE", ownership[[1]]), cbind(group = "SOE", ownership[[2]]), cbind(group = "size_Q25", scale_curves[[1]]), cbind(group = "size_Q75", scale_curves[[2]])), file.path(output_dir, "heterogeneity_curve_source.csv"), row.names = FALSE)} # 保存可追溯源数据。
} # 结束异质性图。
figure_diagnostics <- function(record = FALSE) { # 保留完整残差的辅助诊断。
  graphics_setup(); boxes <- list(); e <- as.numeric(resid(fe)); fit <- as.numeric(fitted(fe)) # 提取企业内残差与拟合值。
  plot(fit, e, pch = 16, cex = 0.27, col = adjustcolor(blue, 0.2), xlim = pad(fit), ylim = pad(e), xlab = "企业内拟合值", ylab = "模型残差") # 显示全部残差。
  abline(h = 0, lty = 2, col = muted); heading("a", "残差与组内拟合值", "全部 2800 条企业—年份观测"); boxes[[1]] <- geometry() # 添加参照并测量。
  q <- qqnorm(e, plot.it = FALSE) # 获取正态理论分位数。
  plot(q$x, q$y, pch = 16, cex = 0.3, col = adjustcolor(blue, 0.45), xlim = pad(q$x), ylim = pad(q$y), xlab = "标准正态理论分位数", ylab = "模型残差") # 保留两端完整标记。
  qqline(e, col = amber, lwd = 1.3); heading("b", "残差正态 Q-Q 图", "参考线由四分位数确定"); boxes[[2]] <- geometry() # 评估残差分布形状。
  if (record) save_geometry(boxes, "Rplot_test6_diagnostics") # 保存最终几何信息。
} # 结束诊断图。
drawers <- list(sample = figure_sample, effect = figure_effect, heterogeneity = figure_heterogeneity, diagnostics = figure_diagnostics) # 收集四种不同证据角色的图。
for (name in names(drawers)) { # 从同一函数导出矢量和位图。
  stem <- paste0("Rplot_test6_", name) # 固定可用于LaTeX的文件名。
  cairo_pdf(file.path(output_dir, paste0(stem, ".pdf")), width = width_mm / 25.4, height = height_mm / 25.4, family = font_family, pointsize = 10) # 创建可编辑PDF。
  drawers[[name]](TRUE); invisible(dev.off()) # 绘制并记录最终物理位置。
  png(file.path(output_dir, paste0(stem, ".png")), width = width_mm, height = height_mm, units = "mm", res = 600, type = "cairo", pointsize = 10) # 创建600dpi预览。
  drawers[[name]](FALSE); invisible(dev.off()) # 关闭位图设备并完成导出。
} # 结束全部图像导出。
