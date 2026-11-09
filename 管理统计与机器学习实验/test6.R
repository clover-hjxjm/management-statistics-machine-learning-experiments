# 实验六：企业数字化投入与经营绩效；400家虚拟企业×2016—2022年。
# 全部观测为模拟数据，企业编号不对应真实证券代码。
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "Chinese (Simplified)_China.utf8")) # 设置中文编码。
source_files <- Filter(Negate(is.null), lapply(sys.frames(), function(e) e$ofile)) # 查找Source文件名。
file_arg <- grep("^--file=", commandArgs(), value = TRUE) # 查找命令行文件名。
script_path <- if (length(source_files)) tail(source_files, 1)[[1]] else if (length(file_arg)) sub("^--file=", "", file_arg[1]) else "test6.R" # 识别脚本位置。
script_dir <- dirname(normalizePath(script_path, mustWork = FALSE)) # 确定代码目录。
local_lib <- c(file.path(script_dir, "test6_library"), file.path(script_dir, "lib")) # 识别项目专用依赖。
.libPaths(c(local_lib[dir.exists(local_lib)], .libPaths())) # 加入本项目的R库。
required <- c("plm", "glmnet", "sandwich", "lmtest") # 列出必要扩展包。
stopifnot(all(vapply(required, requireNamespace, logical(1), quietly = TRUE))) # 确认依赖已安装。
output_dir <- file.path(script_dir, "test6_results") # 设置结果目录。
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE) # 创建结果目录。
alpha <- 0.05 # 预先确定显著性水平。

# -------------------- 1. 固定一次模拟规则 --------------------
RNGkind("Mersenne-Twister", "Inversion", "Rejection") # 固定随机数算法。
set.seed(20261102) # 固定唯一的数据生成种子。
n_firms <- 400L # 指定企业数量。
years <- 2016:2022 # 指定七个观察年份。
firms <- data.frame(firm_id = sprintf("F%03d", 1:n_firms), industry = rep(1:20, each = 20), soe = rbinom(n_firms, 1, 0.4), latent = rnorm(n_firms), listed_year = sample(1995:2015, n_firms, TRUE)) # 生成固定企业特征。
firms$tech_industry <- as.integer(firms$industry <= 6) # 标记模拟高技术行业。
firms$size_base <- rnorm(n_firms, 22, 0.9) + 0.5 * firms$soe # 生成初始资产对数。
panel <- firms[rep(1:n_firms, each = 7), ] # 每家企业重复七次。
panel$year <- rep(years, n_firms) # 将企业与年份配对。
panel$age <- panel$year - panel$listed_year # 计算上市年限。
N <- nrow(panel) # 获取企业—年份观测数。
policy <- matrix(rnorm(20 * 7), 20, 7) # 生成仅进入数字化投入的行业年度推动因素。
panel$policy <- policy[cbind(panel$industry, panel$year - 2015)] # 匹配行业年度推动量。
innovation <- as.vector(replicate(n_firms, as.numeric(arima.sim(list(ar = 0.3), n = 7)))) # 按企业依次生成七期相关扰动。
panel$size <- panel$size_base + 0.045 * (panel$year - 2016) + rnorm(N, 0, 0.22) # 生成总资产对数。
panel$size_c <- panel$size - 22 # 以固定值22中心化，便于解释交互项。
panel$lev <- 100 * plogis(-0.2 + 0.18 * panel$size_c + rnorm(N, 0, 0.5)) # 生成资产负债率百分数。
panel$growth <- 8 + 0.7 * (panel$year - 2016) + rnorm(N, 0, 12) # 生成营业收入增长率百分数。
panel$board_size <- sample(7:13, N, TRUE) # 生成董事会人数。
panel$indep_ratio <- runif(N, 33.3, 55) # 生成独立董事比例百分数。
panel$dig_ratio <- 12 * plogis(-0.5 + 0.14 * (panel$year - 2016) + 0.7 * panel$policy - 0.35 * panel$soe + 0.15 * panel$size_c + 0.35 * panel$latent + 0.3 * innovation + rnorm(N, 0, 0.35)) # 生成有界投入强度。
panel$dig2 <- panel$dig_ratio^2 # 同时保存平方项。
panel$roa <- 3 + (0.70 - 0.14 * panel$soe + 0.10 * panel$size_c) * panel$dig_ratio - 0.055 * panel$dig2 + 0.32 * panel$size_c - 0.035 * panel$lev + 0.02 * panel$growth + 0.05 * panel$board_size + panel$latent + 0.10 * (panel$year - 2016) + 0.6 * innovation + rnorm(N, 0, 1) # 生成含曲率、异质性与内生性的ROA。
stopifnot(N == 2800, !anyDuplicated(panel[c("firm_id", "year")]), all(table(panel$firm_id) == 7), all(is.finite(panel$roa))) # 检查平衡面板与有限值。
latent_names <- c("latent", "policy", "innovation", "size_base", "listed_year") # 指定不进入实证模型的生成信息。
dig_data <- panel[setdiff(names(panel), latent_names)] # 保留分析能够观察到的变量。
write.csv(dig_data, file.path(output_dir, "digital_panel_simulated.csv"), row.names = FALSE, fileEncoding = "UTF-8") # 保存全部原始模拟观测。
variables <- c("roa", "dig_ratio", "size", "lev", "growth", "age", "board_size", "indep_ratio") # 指定描述变量。
descriptive <- do.call(rbind, lapply(variables, function(v) data.frame(variable = v, n = N, mean = mean(dig_data[[v]]), sd = sd(dig_data[[v]]), min = min(dig_data[[v]]), max = max(dig_data[[v]])))) # 汇总均值、标准差和范围。

# -------------------- 2. OLS、非线性与正交多项式 --------------------
controls <- "size_c + lev + growth + age + board_size + indep_ratio + soe + tech_industry + factor(year)" # 设置混合回归控制项。
make_formula <- function(digital) as.formula(paste("roa ~", digital, "+", controls)) # 用相同控制变量构造不同阶数模型。
m1 <- lm(roa ~ dig_ratio, data = dig_data) # 拟合一元线性回归。
m2 <- lm(make_formula("dig_ratio"), data = dig_data) # 加入控制变量。
m3 <- lm(make_formula("dig_ratio + I(dig_ratio^2)"), data = dig_data) # 加入二次项。
m4 <- lm(make_formula("dig_ratio + I(dig_ratio^2) + I(dig_ratio^3) + I(dig_ratio^4)"), data = dig_data) # 拟合四次候选模型。
m5 <- lm(make_formula("poly(dig_ratio, 2)"), data = dig_data) # 使用正交二次基。
stopifnot(max(abs(fitted(m3) - fitted(m5))) < 1e-7) # 验证重参数化不改变拟合值。
cluster_lm <- function(model, data = dig_data) sandwich::vcovCL(model, cluster = data$firm_id, type = "HC1") # 按企业计算聚类协方差。
coef_table <- function(model, V, df = n_firms - 1L) { # 统一整理聚类推断结果。
  b <- coef(model); se <- sqrt(diag(V)); q <- qt(0.975, df) # 提取系数、标准误和临界值。
  data.frame(term = names(b), estimate = unname(b), se = unname(se), statistic = unname(b / se), p = unname(2 * pt(-abs(b / se), df)), lower = unname(b - q * se), upper = unname(b + q * se), row.names = NULL) # 生成系数表。
} # 结束系数整理函数。
ols_models <- list(M1 = m1, M2 = m2, M3 = m3, M4 = m4, M5 = m5) # 收集五个模型。
ols_coefficients <- do.call(rbind, lapply(names(ols_models), function(n) cbind(model = n, coef_table(ols_models[[n]], cluster_lm(ols_models[[n]]))))) # 合并系数结果。
fit_metrics <- do.call(rbind, lapply(names(ols_models), function(n) { # 计算拟合指标。
  m <- ols_models[[n]]; s <- summary(m) # 提取当前模型摘要。
  data.frame(model = n, R2 = s$r.squared, adjusted_R2 = s$adj.r.squared, AIC = AIC(m), BIC = BIC(m), RMSE_in_sample = sqrt(mean(resid(m)^2))) # 区分样本内误差。
})) # 结束模型汇总。
vif_vars <- c("dig_ratio", "size_c", "lev", "growth", "age", "board_size", "indep_ratio", "soe", "tech_industry") # 只对连续与二元实质变量计算VIF。
vif <- data.frame(term = vif_vars, VIF = vapply(vif_vars, function(v) 1 / (1 - summary(lm(reformulate(setdiff(vif_vars, v), v), dig_data))$r.squared), numeric(1))) # 用辅助回归计算VIF。
high_terms <- c("I(dig_ratio^3)", "I(dig_ratio^4)") # 指定共同检验的高阶项。
bh <- coef(m4)[high_terms]; Vh <- cluster_lm(m4)[high_terms, high_terms] # 提取高阶项与聚类协方差。
high_wald <- as.numeric(t(bh) %*% solve(Vh, bh)) / 2 # 计算两个高阶项的联合Wald F值。
high_p <- pf(high_wald, 2, n_firms - 1, lower.tail = FALSE) # 计算聚类近似p值。

# -------------------- 3. 企业分组交叉验证与LASSO --------------------
set.seed(20261103) # 固定预测评估的数据划分。
train_ids <- sample(firms$firm_id, 320) # 留出80家企业作为独立测试集。
is_train <- dig_data$firm_id %in% train_ids # 保证同一企业不跨训练与测试集。
train <- dig_data[is_train, ]; test <- dig_data[!is_train, ] # 获得2240与560条观测。
fold_firm <- setNames(sample(rep(1:5, length.out = 320)), train_ids) # 将训练企业分配到五个折。
foldid <- unname(fold_firm[train$firm_id]) # 将企业级折号匹配到所有年份。
cv_poly <- sapply(1:4, function(degree) { # 分别评估一次至四次多项式。
  loss <- sapply(1:5, function(fold) { # 只在训练企业内部交叉验证。
    model <- lm(make_formula(paste0("poly(dig_ratio,", degree, ")")), train[foldid != fold, ]) # 在当前训练折构造正交基。
    sum((train$roa[foldid == fold] - predict(model, train[foldid == fold, ]))^2) # 累积当前验证折误差。
  }) # 结束单个阶数的交叉验证。
  sqrt(sum(loss) / nrow(train)) # 返回企业分组CV的RMSE。
}) # 结束阶数比较。
chosen_degree <- which.min(cv_poly) # 仅依据训练集CV选阶数。
poly_best <- lm(make_formula(paste0("poly(dig_ratio,", chosen_degree, ")")), train) # 重拟合所选阶数。
lasso_formula <- make_formula("dig_ratio + I(dig_ratio^2) + dig_ratio:soe + dig_ratio:size_c") # 指定含非线性和调节项的候选矩阵。
X_all <- model.matrix(lasso_formula, dig_data)[, -1, drop = FALSE] # 只建立列编码，不使用测试集计算均值或方差。
lasso_cv <- glmnet::cv.glmnet(X_all[is_train, ], train$roa, alpha = 1, foldid = foldid, standardize = TRUE, type.measure = "mse") # 折内标准化并选择惩罚强度。
lasso_prediction <- as.numeric(predict(lasso_cv, X_all[!is_train, ], s = "lambda.1se")) # 采用预设的一标准误规则预测测试集。
ols_prediction <- predict(lm(make_formula("dig_ratio + I(dig_ratio^2)"), train), test) # 计算二次OLS测试预测。
prediction_scores <- function(y, pred) c(RMSE = sqrt(mean((y - pred)^2)), MAE = mean(abs(y - pred)), R2_test = 1 - sum((y - pred)^2) / sum((y - mean(y))^2)) # 统一计算测试指标。
prediction <- rbind(data.frame(model = "Quadratic_OLS", t(prediction_scores(test$roa, ols_prediction))), data.frame(model = "CV_selected_polynomial", t(prediction_scores(test$roa, predict(poly_best, test)))), data.frame(model = "LASSO_1se", t(prediction_scores(test$roa, lasso_prediction)))) # 评价同一独立测试集。
lasso_b <- as.matrix(coef(lasso_cv, s = "lambda.1se")) # 提取实际LASSO系数。
lasso_coefficients <- data.frame(term = rownames(lasso_b), estimate = lasso_b[, 1], selected = abs(lasso_b[, 1]) > 1e-10, row.names = NULL) # 如实记录被保留和压缩的项。

# -------------------- 4. 正态—逆伽马共轭贝叶斯回归 --------------------
Xb_raw <- model.matrix(m3) # 使用与二次混合OLS相同的设计矩阵。
centers <- colMeans(Xb_raw[, -1]); scales <- apply(Xb_raw[, -1], 2, sd) # 计算样本内解释模型的标准化参数。
Xb <- cbind(1, scale(Xb_raw[, -1], center = centers, scale = scales)) # 标准化解释变量，保留截距。
prior_precision <- diag(1 / 100, ncol(Xb)) # 设置beta给定sigma平方时的宽松零均值正态先验。
Vn <- solve(crossprod(Xb) + prior_precision) # 更新条件后验协方差因子。
bn <- drop(Vn %*% crossprod(Xb, dig_data$roa)) # 更新回归系数后验中心。
an <- 2 + N / 2 # 更新逆伽马形状参数。
dn <- 2 + 0.5 * (sum(dig_data$roa^2) - sum(bn * drop(solve(Vn, bn)))) # 更新逆伽马尺度参数。
bayes_se <- sqrt(dn / an * diag(Vn)) # 计算边际t分布的尺度。
bayes <- data.frame(term = colnames(Xb_raw)[-1], mean = bn[-1] / scales, lower = (bn[-1] - qt(0.975, 2 * an) * bayes_se[-1]) / scales, upper = (bn[-1] + qt(0.975, 2 * an) * bayes_se[-1]) / scales, row.names = NULL) # 转回原变量单位的可信区间。

# -------------------- 5. 混合、固定效应和随机效应模型 --------------------
pdata <- plm::pdata.frame(dig_data, index = c("firm_id", "year")) # 建立企业—年份索引。
panel_formula <- roa ~ dig_ratio + dig2 + size_c + lev + growth + board_size + indep_ratio + factor(year) # 统一面板模型的实质解释变量。
pool <- plm::plm(panel_formula, pdata, model = "pooling") # 拟合带年份虚拟变量的混合OLS。
fe <- plm::plm(panel_formula, pdata, model = "within", effect = "individual") # 吸收企业固定效应，保留年份效应。
re <- plm::plm(panel_formula, pdata, model = "random", effect = "individual") # 拟合随机企业截距模型。
selection <- list(F_FE_pool = plm::pFtest(fe, pool), BP_RE_pool = plm::plmtest(pool, effect = "individual", type = "bp"), Hausman_FE_RE = plm::phtest(fe, re)) # 按顺序执行三项模型选择检验。
selection_table <- do.call(rbind, lapply(names(selection), function(n) data.frame(test = n, statistic = unname(selection[[n]]$statistic), p = selection[[n]]$p.value))) # 保存原始检验统计量。
cluster_plm <- function(m) plm::vcovHC(m, method = "arellano", type = "HC1", cluster = "group") # 允许同一企业内异方差与序列相关。
cre_data <- dig_data # 为相关随机效应诊断准备企业均值扩展数据。
mean_vars <- c("dig_ratio", "dig2", "size_c", "lev", "growth", "board_size", "indep_ratio") # 选择所有时变解释变量。
for (v in mean_vars) cre_data[[paste0("mean_", v)]] <- ave(cre_data[[v]], cre_data$firm_id, FUN = mean) # 添加每家企业的时间均值。
cre_formula <- as.formula(paste("roa ~", paste(c(mean_vars, "factor(year)", paste0("mean_", mean_vars)), collapse = "+"))) # 同时保留原项、年份与企业均值项。
cre <- plm::plm(cre_formula, plm::pdata.frame(cre_data, index = c("firm_id", "year")), model = "random") # 拟合Mundlak均值扩展模型。
cre_b <- coef(cre)[paste0("mean_", mean_vars)]; cre_V <- cluster_plm(cre)[names(cre_b), names(cre_b)] # 提取企业均值项的聚类协方差。
cre_W <- drop(t(cre_b) %*% solve(cre_V, cre_b)) # 联合检验企业均值项是否全部为零。
selection_table <- rbind(selection_table, data.frame(test = "Mundlak_cluster", statistic = cre_W, p = pchisq(cre_W, length(mean_vars), lower.tail = FALSE))) # 补充随机效应正交限制的稳健诊断。
Vfe <- cluster_plm(fe) # 计算主模型企业聚类协方差。
fe_coefficients <- coef_table(fe, Vfe) # 输出主模型系数与聚类区间。
fe_cubic <- plm::plm(update(panel_formula, . ~ . + I(dig_ratio^3)), pdata, model = "within") # 检查二次面板规格对三次项的敏感性。
fe_cubic_coefficients <- coef_table(fe_cubic, cluster_plm(fe_cubic)) # 保存敏感性模型的聚类推断。
joint_terms <- c("dig_ratio", "dig2", "size_c", "lev", "growth", "board_size", "indep_ratio") # 指定整体实质解释变量。
joint_b <- coef(fe)[joint_terms] # 提取联合零假设对应系数。
joint_F <- drop(t(joint_b) %*% solve(Vfe[joint_terms, joint_terms], joint_b)) / length(joint_terms) # 计算企业聚类整体Wald F近似量。
serial_test <- plm::pbgtest(fe, order = 1) # 检查一阶残差相关。
hetero_test <- lmtest::bptest(as.numeric(resid(fe)) ~ as.numeric(fitted(fe))) # 将残差方差与拟合值作辅助诊断。
diagnostics <- data.frame(test = c("BG_serial_order1", "BP_residual_fitted_auxiliary"), statistic = c(unname(serial_test$statistic), unname(hetero_test$statistic)), p = c(serial_test$p.value, hetero_test$p.value)) # 保存诊断结果。
turning <- function(m, V, linear = "dig_ratio", square = "dig2") { # 用delta法估计曲线转折点的不确定性。
  b <- coef(m); peak <- -b[linear] / (2 * b[square]) # 计算二次函数顶点。
  g <- c(-1 / (2 * b[square]), b[linear] / (2 * b[square]^2)) # 求顶点对两个系数的梯度。
  se <- sqrt(drop(t(g) %*% V[c(linear, square), c(linear, square)] %*% g)) # 传播估计误差。
  c(estimate = unname(peak), se = se, lower = peak - qt(0.975, 399) * se, upper = peak + qt(0.975, 399) * se) # 返回近似95%区间。
} # 结束转折点函数。
peak <- turning(fe, Vfe) # 计算主模型转折点。

# -------------------- 6. 企业固定效应工具变量回归 --------------------
cell <- interaction(dig_data$industry, dig_data$year, drop = TRUE) # 建立行业—年份分组。
cell_sum <- ave(dig_data$dig_ratio, cell, FUN = sum); cell_n <- ave(dig_data$dig_ratio, cell, FUN = length) # 汇总每个行业年度的投入。
dig_data$peer_dig <- (cell_sum - dig_data$dig_ratio) / (cell_n - 1) # 排除自身，构造其他企业的平均投入。
dig_data$peer_dig2 <- dig_data$peer_dig^2 # 为第二个内生解释项构造第二个工具。
pdata_iv <- plm::pdata.frame(dig_data, index = c("firm_id", "year")) # 更新面板索引。
iv_formula <- roa ~ dig_ratio + dig2 + size_c + lev + growth + board_size + indep_ratio + factor(year) | peer_dig + peer_dig2 + size_c + lev + growth + board_size + indep_ratio + factor(year) # 同时将投入及平方项作为内生项。
iv <- plm::plm(iv_formula, pdata_iv, model = "within", effect = "individual") # 企业内变换后进行2SLS。
Viv <- cluster_plm(iv); iv_coefficients <- coef_table(iv, Viv) # 使用企业聚类标准误。
first_stages <- lapply(c("dig_ratio", "dig2"), function(v) { # 分别检查两个内生项的工具相关性。
  f <- as.formula(paste(v, "~ peer_dig + peer_dig2 + size_c + lev + growth + board_size + indep_ratio + factor(year)")) # 指定第一阶段。
  m <- plm::plm(f, pdata_iv, model = "within"); b <- coef(m)[c("peer_dig", "peer_dig2")] # 估计第一阶段。
  V <- cluster_plm(m)[names(b), names(b)]; W <- drop(t(b) %*% solve(V, b)) / 2 # 计算排除工具的聚类联合F近似值。
  data.frame(endogenous = v, excluded_instrument_F = W, p = pf(W, 2, 399, lower.tail = FALSE)) # 记录相关性证据。
}) # 结束第一阶段相关性检查。
first_stage <- do.call(rbind, first_stages) # 合并两个内生变量结果。

# -------------------- 7. 所有制与规模异质性 --------------------
hetero_formula <- update(panel_formula, . ~ . + dig_ratio:soe + dig_ratio:size_c) # 增加所有制与规模调节项。
hetero <- plm::plm(hetero_formula, pdata, model = "within", effect = "individual") # 统一样本内估计差异。
Vhet <- cluster_plm(hetero); heterogeneity <- coef_table(hetero, Vhet) # 给出差异项的直接显著性检验。

# -------------------- 8. 保存完整输出 --------------------
tables <- list(descriptive = descriptive, ols_coefficients = ols_coefficients, fit_metrics = fit_metrics, vif = vif, polynomial_cv = data.frame(degree = 1:4, RMSE_groupCV = cv_poly), prediction_test = prediction, lasso_coefficients = lasso_coefficients, bayesian_coefficients = bayes, model_selection = selection_table, fe_coefficients = fe_coefficients, fe_cubic_coefficients = fe_cubic_coefficients, diagnostics = diagnostics, iv_coefficients = iv_coefficients, first_stage = first_stage, heterogeneity = heterogeneity) # 将每项分析分别命名。
for (n in names(tables)) write.csv(tables[[n]], file.path(output_dir, paste0(n, ".csv")), row.names = FALSE, fileEncoding = "UTF-8") # 保存CSV结果。
main <- data.frame(metric = c("firms", "years", "observations", "SOE_firms", "nonSOE_firms", "high_order_Wald_F", "high_order_p", "selected_degree_CV", "lambda_1se", "FE_within_R2", "FE_turning_point", "FE_turning_lower", "FE_turning_upper", "dig_min", "dig_max", "max_VIF", "FE_joint_F", "FE_joint_p"), value = c(n_firms, 7, N, sum(firms$soe), sum(1 - firms$soe), high_wald, high_p, chosen_degree, lasso_cv$lambda.1se, summary(fe)$r.squared[1], peak[1], peak[3], peak[4], min(dig_data$dig_ratio), max(dig_data$dig_ratio), max(vif$VIF), joint_F, pf(joint_F, length(joint_terms), 399, lower.tail = FALSE))) # 汇总报告所需关键指标。
write.csv(main, file.path(output_dir, "main_results.csv"), row.names = FALSE) # 保存核心结果。
capture.output({print(main, row.names = FALSE); print(selection_table); print(fe_coefficients); print(first_stage); print(heterogeneity); print(prediction)}, file = file.path(output_dir, "results.txt")) # 保存可读结果。
writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt")) # 记录实际软件与依赖版本。
saveRDS(list(data = dig_data, models = list(fe = fe, iv = iv, hetero = hetero), covariance = list(fe = Vfe, iv = Viv, hetero = Vhet), peak = peak), file.path(output_dir, "analysis_objects.rds")) # 保留绘图与复核所需模型。
source(file.path(script_dir, "test6_figures.R"), encoding = "UTF-8") # 生成与这些实际模型一致的图像。
cat("分析完成：", output_dir, "\n") # 显示保存位置。
