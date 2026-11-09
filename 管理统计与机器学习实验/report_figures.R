# 实验报告配图。实验四执行 test4.R 的终稿计算与图形代码。
# 实验五复用全部 240 条评分及 Tukey 区间，实验六复用完整模拟面板及模型区间。
# 每个文件只含一张图。Rscript report_figures.R [数据项目目录] [输出项目目录]
source_paths <- Filter(function(x) is.character(x) && length(x) == 1L,
                       lapply(sys.frames(), function(frame) frame$ofile))
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(source_paths)) tail(source_paths, 1)[[1]] else if (length(file_arg)) {
  sub("^--file=", "", file_arg[1])
} else "report_figures.R"
script_dir <- dirname(normalizePath(script_path, mustWork = TRUE))
args <- if (length(source_paths)) character() else commandArgs(trailingOnly = TRUE)
project <- if (length(args) >= 1L) args[1] else script_dir
destination <- if (length(args) >= 2L) args[2] else project
project <- normalizePath(project, mustWork = TRUE)
if (!dir.exists(destination)) dir.create(destination, recursive = TRUE)
destination <- normalizePath(destination, mustWork = TRUE)
stopifnot(capabilities("cairo"))

optional_library <- file.path(project, "test6_library")
if (dir.exists(optional_library)) .libPaths(c(optional_library, .libPaths()))
has_svg <- requireNamespace("svglite", quietly = TRUE)
blue <- "#0072B2"
orange <- "#D55E00"
teal <- "#009E73"
ink <- "#242A30"
muted <- "#7A858E"
group_colours <- c(A = blue, B = teal, C = orange)
font_family <- "Arial"
# PDF 采用兼容的 Helvetica 并保留文字对象，便于逐个刻度检查。
pdfFonts(Arial = pdfFonts("Helvetica")$Helvetica)

paper_style <- function() {
  par(family = font_family, las = 1, bty = "l", bg = "white",
      mar = c(3.45, 4.1, .8, .85), mgp = c(2.40, .63, 0), tcl = -.23,
      cex.axis = .91, cex.lab = 1, lwd = .7, fg = ink, col.axis = ink, col.lab = ink)
}

export_figure <- function(experiment, name, draw, width_mm = 83, height_mm = 63,
                          style = paper_style) {
  folder <- file.path(destination, "Latex_paper", paste0("figures_exp", experiment))
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  stem <- file.path(folder, name)
  width <- width_mm / 25.4
  height <- height_mm / 25.4
  grDevices::pdf(paste0(stem, ".pdf"), width = width, height = height,
                 family = font_family, pointsize = 8, bg = "white", useDingbats = FALSE)
  tryCatch({ style(); draw() }, finally = dev.off())
  grDevices::png(paste0(stem, ".png"), width = width, height = height, units = "in",
                 res = 600, type = "cairo", pointsize = 8, bg = "white")
  tryCatch({ style(); draw() }, finally = dev.off())
  if (has_svg) {
    svglite::svglite(paste0(stem, ".svg"), width = width, height = height,
                    pointsize = 8, bg = "white")
    tryCatch({ style(); draw() }, finally = dev.off())
  }
  message("Experiment ", experiment, " | ", name)
}

# 执行终稿语句，仅将 show_plot 改接为报告导出设备。
pricing <- new.env()
rendered_pricing <- character()
render_pricing <- function(name, draw) {
  style <- function() {
    pricing$plot_style()
    par(mar = c(3.55, 4.5, 2.25, .9), mgp = c(2.6, .62, 0),
        cex.axis = .92, cex.main = .93, lwd = .72)
  }
  width <- if (identical(name, "Fig07_CV_differences")) 136 else 110
  export_figure(4, name, draw, width_mm = width, height_mm = 70, style = style)
  rendered_pricing <<- c(rendered_pricing, name)
}
pricing_code <- parse(file.path(script_dir, "test4.R"), encoding = "UTF-8")
invisible(capture.output(for (statement in pricing_code) {
  if (is.call(statement) && identical(statement[[1L]], as.name("<-")) &&
      identical(statement[[2L]], quote(show_plot))) {
    pricing$show_plot <- render_pricing
  } else {
    eval(statement, envir = pricing)
  }
}))
stopifnot(length(rendered_pricing) == 9L, !anyDuplicated(rendered_pricing))
data_folder <- file.path(destination, "test4_results_final")
dir.create(data_folder, showWarnings = FALSE)
first_models <- do.call(rbind, lapply(c("H", "L"), function(g) {
  coefficients <- coef(pricing$first$fit[[paste0("model_", g)]])
  data.frame(group = g, term = names(coefficients), estimate = unname(coefficients))
}))
tables4 <- list(
  user_data = pricing$u,
  feature_summary = pricing$feature_summary,
  ab_summary = pricing$ab,
  z_test = pricing$z_result,
  order_revenue = pricing$revenue_result,
  first_split = pricing$first_split,
  first_fold_models = first_models,
  fold_predictions = pricing$ff,
  fold_variance = pricing$variance_table,
  reference_test = pricing$reference_result
)
stopifnot(all(vapply(tables4, is.data.frame, logical(1))))
for (name in names(tables4)) {
  write.csv(tables4[[name]], file.path(data_folder, paste0(name, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")
}

# 实验五 | 单因素方差分析的数据、组对差值及残差诊断。
read_result <- function(experiment, file) {
  read.csv(file.path(project, paste0("test", experiment, "_results"), file),
           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
}
district <- read_result(5, "price_acceptance_simulated.csv")
district$city_district <- factor(district$city_district, levels = c("A", "B", "C"))
stopifnot(nrow(district) == 240L, !anyNA(district),
          !anyDuplicated(district$user_id), all(table(district$city_district) == 80L))
anova_fit <- aov(price_acceptance ~ city_district, data = district)
residual <- residuals(anova_fit)
tukey <- read_result(5, "tukey.csv")
tukey_fit <- TukeyHSD(anova_fit)$city_district
stopifnot(identical(tukey$comparison, rownames(tukey_fit)),
          max(abs(as.matrix(tukey[c("difference", "lower", "upper")]) -
                    unname(tukey_fit[, 1:3]))) < 1e-9)
set.seed(20261108) # 仅用于绘图横向抖动，评分及残差保持原值。
district_x <- as.integer(district$city_district)
jitter_x <- district_x + runif(nrow(district), -.14, .14)
alpha_col <- function(colour, opacity) adjustcolor(colour, alpha.f = opacity)
dot_colours <- alpha_col(group_colours, .45)[district_x]

export_figure(5, "district_distribution", function() {
  boxplot(price_acceptance ~ city_district, data = district, outline = FALSE,
          ylim = range(pretty(district$price_acceptance)), boxwex = .49,
          xlab = "Business district", ylab = "Price acceptance score",
          col = alpha_col(group_colours, .10), border = ink, medlwd = 1.05,
          whisklty = 1, staplelty = 1, boxlwd = .75, whisklwd = .7, staplelwd = .7)
  points(jitter_x, district$price_acceptance, pch = 16, cex = .46, col = dot_colours)
  points(1:3, tapply(district$price_acceptance, district$city_district, mean),
         pch = 23, cex = .88, bg = "white", col = group_colours, lwd = 1)
})

export_figure(5, "district_tukey", function() {
  y <- rev(seq_len(nrow(tukey)))
  plot(tukey$difference, y, type = "n", yaxt = "n", ylim = c(.42, 3.58),
       xlim = range(pretty(c(tukey$lower, tukey$upper, 0))),
       xlab = "Mean difference (score points)", ylab = "")
  axis(2, at = y, labels = tukey$comparison)
  abline(v = 0, col = muted, lty = 2, lwd = .75)
  segments(tukey$lower, y, tukey$upper, y, col = ink, lwd = 1)
  segments(tukey$lower, y - .085, tukey$lower, y + .085, col = ink, lwd = .8)
  segments(tukey$upper, y - .085, tukey$upper, y + .085, col = ink, lwd = .8)
  points(tukey$difference, y, pch = 16, cex = .8, col = blue)
})

export_figure(5, "district_qq", function() {
  qqnorm(residual, main = "", xlab = "Theoretical normal quantile",
         ylab = "Residual (score points)", pch = 16, cex = .46,
         col = alpha_col(blue, .68))
  qqline(residual, col = muted, lwd = .9)
})

export_figure(5, "district_residuals", function() {
  plot(jitter_x, residual, type = "n", xaxt = "n", xlim = c(.55, 3.45),
       ylim = range(pretty(residual)), xlab = "Business district",
       ylab = "Residual (score points)")
  axis(1, at = 1:3, labels = levels(district$city_district))
  abline(h = 0, col = muted, lty = 2, lwd = .75)
  points(jitter_x, residual, pch = 16, cex = .46, col = dot_colours)
})

# 实验六 | 全部企业-年份观测、年度均值区间、固定效应模型逐点区间。
panel <- read_result(6, "digital_panel_simulated.csv")
trend <- read_result(6, "ownership_year_source.csv")
nonlinear <- read_result(6, "nonlinear_curve_source.csv")
heterogeneity <- read_result(6, "heterogeneity_curve_source.csv")
main_results <- read_result(6, "main_results.csv")
peak <- main_results$value[main_results$metric == "FE_turning_point"]
stopifnot(nrow(panel) == 2800L, length(unique(panel$firm_id)) == 400L,
          all(is.finite(panel$dig_ratio)), all(is.finite(panel$roa)),
          length(peak) == 1L, is.finite(peak))
for (tab in list(trend, nonlinear, heterogeneity)) {
  stopifnot(all(is.finite(tab$mean)), all(tab$lower <= tab$mean),
            all(tab$mean <= tab$upper))
}
interval_band <- function(x, low, high, colour) {
  stopifnot(all(diff(x) > 0))
  polygon(c(x, rev(x)), c(low, rev(high)), col = alpha_col(colour, .16), border = NA)
}

export_figure(6, "digital_ownership_trend", function() {
  plot(trend$year, trend$mean, type = "n", xaxt = "n",
       ylim = c(min(trend$lower) - .16, max(trend$upper) + .50),
       xlab = "Year", ylab = "Mean digital investment (%)")
  axis(1, at = seq(2016, 2022, 2))
  for (g in 0:1) {
    d <- trend[trend$soe == g, ]
    col <- c(blue, orange)[g + 1]
    interval_band(d$year, d$lower, d$upper, col)
    lines(d$year, d$mean, type = "o", lwd = 1.2, lty = g + 1,
          pch = c(16, 17)[g + 1], cex = .59, col = col)
  }
  legend("topleft", c("Non-SOE", "SOE"), col = c(blue, orange),
         lty = 1:2, pch = c(16, 17), lwd = 1.1, pt.cex = .63,
         bty = "n", cex = .9, inset = .02)
})

export_figure(6, "digital_roa_scatter", function() {
  plot(panel$dig_ratio, panel$roa, pch = 16, cex = .38,
       col = alpha_col(blue, .24), xlab = "Digital investment (%)", ylab = "ROA (%)")
})

fe_effect <- nonlinear[nonlinear$kind == "FE_relative_ROA", ]
fe_marginal <- nonlinear[nonlinear$kind != "FE_relative_ROA", ]
stopifnot(nrow(fe_effect) > 2L, nrow(fe_marginal) == nrow(fe_effect),
          length(unique(fe_marginal$kind)) == 1L)
plot_fe_curve <- function(d, ylabel) {
  plot(d$x, d$mean, type = "n", ylim = range(c(d$lower, d$upper, 0)),
       xlab = "Digital investment (%)", ylab = ylabel)
  interval_band(d$x, d$lower, d$upper, blue)
  abline(h = 0, col = muted, lty = 2, lwd = .7)
  abline(v = peak, col = orange, lty = 3, lwd = .8)
  lines(d$x, d$mean, col = blue, lwd = 1.35)
}
export_figure(6, "digital_fe_response", function() {
  plot_fe_curve(fe_effect, "Change in ROA (pp)")
})
export_figure(6, "digital_fe_marginal", function() {
  plot_fe_curve(fe_marginal, "Marginal association (pp per pp)")
})

plot_heterogeneity <- function(groups, labels) {
  d <- heterogeneity[heterogeneity$group %in% groups, ]
  stopifnot(all(groups %in% d$group))
  limits <- range(c(d$lower, d$upper, 0))
  plot(d$x, d$mean, type = "n", ylim = limits + c(0, .17 * diff(limits)),
       xlab = "Digital investment (%)", ylab = "Change in ROA (pp)")
  for (i in seq_along(groups)) {
    z <- d[d$group == groups[i], ]
    interval_band(z$x, z$lower, z$upper, c(blue, orange)[i])
  }
  abline(h = 0, col = muted, lty = 3, lwd = .7)
  for (i in seq_along(groups)) {
    z <- d[d$group == groups[i], ]
    lines(z$x, z$mean, col = c(blue, orange)[i], lwd = 1.25, lty = i)
  }
  legend("topleft", labels, col = c(blue, orange), lty = 1:2,
         lwd = 1.15, bty = "n", cex = .9, inset = .02)
}
export_figure(6, "digital_ownership_effect", function() {
  plot_heterogeneity(c("nonSOE", "SOE"), c("Non-SOE", "SOE"))
})
export_figure(6, "digital_size_effect", function() {
  plot_heterogeneity(c("size_Q25", "size_Q75"), c("Size: 25th percentile", "Size: 75th percentile"))
})
