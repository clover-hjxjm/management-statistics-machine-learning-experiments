#电商 A/B 测试：置换检验、独立 Bootstrap 区间和 Welch t 检验

#参数设置
nA <- 1000L
nB <- 1000L

muA <- 5.2
muB <- 5.8

shape <- 4 #负二项分布形状参数

R <- 10000L #每种方法重抽样次数

alpha <- 0.05 #显著性水平

seed_data <- 20260916L #随机种子
seed_permutation <- 20260917L
seed_bootstrap <- 20260918L

stopifnot(nA >= 2, nB >= 2, nA == floor(nA), nB == floor(nB),
          R >= 1, R == floor(R), muA > 0, muB > 0, shape > 0,
          alpha > 0, alpha < 1)

#生成两组用户的点击数
set.seed(seed_data, kind = "Mersenne-Twister",
         normal.kind = "Inversion", sample.kind = "Rejection")

A <- rnbinom(nA, size = shape, mu = muA)
B <- rnbinom(nB, size = shape, mu = muB)
stopifnot(all(A >= 0 & A == floor(A)), all(B >= 0 & B == floor(B)))

#对生成的数据进行描述统计
meanA <- mean(A)
cat("A 组样本均值 =", meanA, "\n")
meanB <- mean(B)
cat("B 组样本均值 =", meanB, "\n")
sdA <- sd(A)
cat("A 组样本标准差 =", sdA, "\n")
sdB <- sd(B)
cat("B 组样本标准差 =", sdB, "\n")
observed <- meanB - meanA
cat("观察均值差 B-A =", observed, "次/用户\n")
relative_lift <- if (meanA > 0) observed / meanA else NA_real_
cat("相对提升 =", 100 * relative_lift, "%（A 均值为零时无定义）\n")

#两独立样本 t 检验
##H0：A、B 两组总体均值相等；H1：A、B 两组总体均值不相等
t_result <- t.test(B, A)
t_statistic <- unname(t_result$statistic)
p_t <- t_result$p.value
cat("两独立样本 t 统计量 =", t_statistic, "\n")
cat("两独立样本 t 检验 p 值 =", p_t, "\n")
reject_t <- p_t < alpha
cat("t 检验是否拒绝 H0 =", reject_t, "\n")
if (reject_t) {
  cat("t 检验结论：两组均值差异具有统计显著性。\n")
} else {
  cat("t 检验结论：两组均值差异不具有统计显著性。\n")
}

#置换 Bootstrap 检验
##混合无放回抽取
set.seed(seed_permutation)
pool <- c(A, B)
N <- length(pool)
permutation_diff <- numeric(R)
for (r in seq_len(R)) {
  order <- sample.int(N, size = N, replace = FALSE)
  simulated_A <- pool[order[seq_len(nA)]]
  simulated_B <- pool[order[seq.int(nA + 1L, N)]]
  permutation_diff[r] <- mean(simulated_B) - mean(simulated_A)
}
cat("已完成", R, "次无放回置换。\n")
##Bootstrap 检验计算
extreme_count <- sum(permutation_diff >= observed - 1e-12)
cat("右尾极端次数 K =", extreme_count, "\n")
p_raw <- extreme_count / R
cat("原始置换 p 值 K/R =", p_raw, "\n")
p_permutation <- (extreme_count + 1) / (R + 1)
cat("加一修正置换 p 值 =", p_permutation, "\n")
reject_permutation <- p_permutation < alpha
cat("置换检验是否拒绝 H0 =", reject_permutation, "\n")

#独立样本 Bootstrap
##独立样本 Bootstrap
set.seed(seed_bootstrap)
bootstrap_diff <- numeric(R)
for (r in seq_len(R)) {
  resampled_A <- A[sample.int(nA, size = nA, replace = TRUE)]
  resampled_B <- B[sample.int(nB, size = nB, replace = TRUE)]
  bootstrap_diff[r] <- mean(resampled_B) - mean(resampled_A)
}
cat("已完成", R, "次组内有放回 Bootstrap。\n")
##百分位置信区间和标准
ci_lower <- unname(quantile(bootstrap_diff, alpha / 2, type = 7))
cat("Bootstrap 双侧", 100 * (1-alpha), "% 区间下界 =", ci_lower, "\n")
ci_upper <- unname(quantile(bootstrap_diff, 1 - alpha / 2, type = 7))
cat("Bootstrap 双侧", 100 * (1-alpha), "% 区间上界 =", ci_upper, "\n")
one_sided_lower <- unname(quantile(bootstrap_diff, alpha, type = 7))
cat("Bootstrap 单侧", 100 * (1-alpha), "% 下界 =", one_sided_lower,
    "（上界为 +Inf）\n")
bootstrap_se <- sd(bootstrap_diff)
cat("Bootstrap 均值差标准误 =", bootstrap_se, "\n")

#可视化
local({
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(mfrow = c(1, 3), mar = c(4.3, 4.2, 3.6, 1.0),
      mgp = c(2.5, 0.8, 0), cex = 0.85, bg = "white")
  
  # 左图：统一整数分箱，并叠加 A、B 的概率直方图。
  click_breaks <- seq(-0.5, max(c(A, B)) + 0.5, by = 1)
  hist_A <- hist(A, breaks = click_breaks, plot = FALSE)
  hist_B <- hist(B, breaks = click_breaks, plot = FALSE)
  color_A <- adjustcolor("#2378AD", alpha.f = 0.55)
  color_B <- adjustcolor("#E18A3B", alpha.f = 0.55)
  plot(hist_A, freq = FALSE, col = color_A, border = "white",
       ylim = c(0, max(hist_A$density, hist_B$density) * 1.25),
       main = "User click distribution", xlab = "Clicks per user",
       ylab = "Proportion")
  plot(hist_B, freq = FALSE, col = color_B, border = "white", add = TRUE)
  legend("topright", legend = c("A: original", "B: new"),
         fill = c(color_A, color_B), bty = "n", cex = 0.85)
  
  # 中图：置换零分布，以零为参考，红线表示真实观察差值。
  hist(permutation_diff, breaks = 45, probability = TRUE,
       xlim = range(c(permutation_diff, observed, 0)),
       col = "#80B8D9", border = "white",
       main = sprintf("Permutation null\np = %.6f", p_permutation),
       xlab = "Mean difference (B - A)", ylab = "Density")
  abline(v = 0, col = "#555555", lty = 3, lwd = 1.5)
  abline(v = observed, col = "#D83B3B", lwd = 2)
  legend("topright", legend = c("Observed B - A", "Zero"),
         col = c("#D83B3B", "#555555"), lty = c(1, 3),
         lwd = c(2, 1.5), bty = "n", cex = 0.8)
  
  # 右图：组内 Bootstrap 估计分布，红虚线表示双侧置信区间端点。
  hist(bootstrap_diff, breaks = 45, probability = TRUE,
       xlim = range(c(bootstrap_diff, ci_lower, ci_upper, observed, 0)),
       col = "#80B8D9", border = "white",
       main = sprintf("Bootstrap %.0f%% CI\n[%.3f, %.3f]",
                      100 * (1-alpha), ci_lower, ci_upper),
       xlab = "Mean difference (B - A)", ylab = "Density")
  abline(v = 0, col = "#555555", lty = 3, lwd = 1.5)
  abline(v = observed, col = "#23844D", lwd = 2)
  abline(v = c(ci_lower, ci_upper), col = "#D83B3B", lty = 2, lwd = 2)
  legend("topright", legend = c("CI endpoints", "Observed", "Zero"),
         col = c("#D83B3B", "#23844D", "#555555"),
         lty = c(2, 1, 3), lwd = c(2, 2, 1.5), bty = "n", cex = 0.75)
})
