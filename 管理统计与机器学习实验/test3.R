# 贝叶斯方法在管理预测中的应用：月度销售额预测
# 使用基础 R 实现四条 Gibbs 采样链，不依赖 brms、Stan 等扩展包。

# -------------------- 1. 参数与模拟数据 --------------------
set.seed(20261003L)
n <- 100L
beta_true <- c(intercept = 10, advertising = 2, holiday = 15)
sigma_true <- 4

# advertising 表示广告投入，单位为万元；holiday 为节假日指示变量。
advertising <- runif(n, min = 0, max = 10)
holiday <- rbinom(n, size = 1, prob = 0.30)
mu <- beta_true[1] + beta_true[2] * advertising +
  beta_true[3] * holiday
sales <- rnorm(n, mean = mu, sd = sigma_true)

data <- data.frame(
  sales = sales,
  advertising = advertising,
  holiday = holiday
)
stopifnot(nrow(data) == n, !anyNA(data),
          all(data$advertising >= 0 & data$advertising <= 10),
          all(data$holiday %in% c(0, 1)))

cat("\nData summary\n")
print(data.frame(
  sample_size = n,
  mean_sales = round(mean(data$sales), 4),
  mean_advertising = round(mean(data$advertising), 4),
  holiday_months = sum(data$holiday)
), row.names = FALSE)

# -------------------- 2. 贝叶斯线性回归模型 --------------------
# sales_i ~ Normal(x_i' beta, sigma^2)
# beta | sigma^2 ~ Normal(m0, sigma^2 V0)
# sigma^2 ~ Inverse-Gamma(a0, b0)
X <- model.matrix(~ advertising + holiday, data = data)
y <- data$sales
p <- ncol(X)

m0 <- rep(0, p)
V0 <- diag(c(100, 100, 100))
V0_inv <- solve(V0)
a0 <- 2
b0 <- 8

Vn <- solve(crossprod(X) + V0_inv)
mn <- as.vector(Vn %*% (crossprod(X, y) + V0_inv %*% m0))
chol_Vn <- chol(Vn)

# -------------------- 3. Gibbs 采样 --------------------
n_chains <- 4L
n_iter <- 6000L
burn_in <- 1000L
n_keep <- n_iter - burn_in
chain_seeds <- 20261004L + seq_len(n_chains) - 1L

run_chain <- function(seed, beta_start, sigma2_start) {
  set.seed(seed)
  beta <- beta_start
  sigma2 <- sigma2_start
  draws <- matrix(NA_real_, nrow = n_keep, ncol = p + 1L)
  colnames(draws) <- c(colnames(X), "sigma")

  for (iter in seq_len(n_iter)) {
    beta <- mn + sqrt(sigma2) *
      as.vector(t(chol_Vn) %*% rnorm(p))

    residual <- y - as.vector(X %*% beta)
    prior_difference <- beta - m0
    shape_post <- a0 + (n + p) / 2
    rate_post <- b0 + 0.5 *
      (sum(residual^2) +
         as.numeric(t(prior_difference) %*% V0_inv %*%
                      prior_difference))
    sigma2 <- 1 / rgamma(1, shape = shape_post, rate = rate_post)

    if (iter > burn_in) {
      draws[iter - burn_in, ] <- c(beta, sqrt(sigma2))
    }
  }
  draws
}

chains <- vector("list", n_chains)
for (chain_id in seq_len(n_chains)) {
  chains[[chain_id]] <- run_chain(
    seed = chain_seeds[chain_id],
    beta_start = c(0, 0, 0),
    sigma2_start = 25
  )
}

# Gelman-Rubin R-hat，接近 1 表示四条链具有较好的一致性。
calculate_rhat <- function(chain_list, parameter_index) {
  chain_matrix <- sapply(chain_list, function(z) z[, parameter_index])
  n_draw <- nrow(chain_matrix)
  between <- n_draw * var(colMeans(chain_matrix))
  within <- mean(apply(chain_matrix, 2, var))
  variance_estimate <- ((n_draw - 1) / n_draw) * within +
    between / n_draw
  sqrt(variance_estimate / within)
}

rhat <- sapply(seq_len(p + 1L), function(j) calculate_rhat(chains, j))
names(rhat) <- colnames(chains[[1]])
posterior_draws <- do.call(rbind, chains)

# -------------------- 4. 后验估计 --------------------
posterior_summary <- data.frame(
  parameter = colnames(posterior_draws),
  posterior_mean = apply(posterior_draws, 2, mean),
  lower_95 = apply(posterior_draws, 2, quantile, probs = 0.025),
  upper_95 = apply(posterior_draws, 2, quantile, probs = 0.975),
  Rhat = rhat,
  row.names = NULL
)
posterior_summary[, 2:5] <- round(posterior_summary[, 2:5], 4)

cat("\nPosterior parameter summary\n")
print(posterior_summary, row.names = FALSE)

# -------------------- 5. 后验预测 --------------------
# 预测广告投入为 5 万元时，非节假日和节假日的月度销售额。
new_data <- data.frame(
  advertising = c(5, 5),
  holiday = c(0, 1)
)
X_new <- model.matrix(~ advertising + holiday, data = new_data)

set.seed(20261008L)
prediction_draws <- matrix(NA_real_, nrow = nrow(posterior_draws),
                           ncol = nrow(new_data))
for (i in seq_len(nrow(new_data))) {
  predictive_mean <- as.vector(posterior_draws[, seq_len(p)] %*% X_new[i, ])
  prediction_draws[, i] <- rnorm(
    nrow(posterior_draws),
    mean = predictive_mean,
    sd = posterior_draws[, "sigma"]
  )
}

prediction_summary <- data.frame(
  scenario = c("Non-holiday", "Holiday"),
  advertising = new_data$advertising,
  posterior_predictive_mean = colMeans(prediction_draws),
  lower_95 = apply(prediction_draws, 2, quantile, probs = 0.025),
  upper_95 = apply(prediction_draws, 2, quantile, probs = 0.975)
)
prediction_summary[, 3:5] <- round(prediction_summary[, 3:5], 4)

cat("\nPosterior predictive summary\n")
print(prediction_summary, row.names = FALSE)

# -------------------- 6. 诊断图与后验分布图 --------------------
output_dir <- Sys.getenv("EXPERIMENT3_OUTPUT_DIR",
                         unset = file.path(getwd(), "test3_results"))
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
plot_file <- file.path(output_dir, "Rplot_test3.jpeg")

jpeg(plot_file, width = 2400, height = 1700, quality = 95, res = 220)
old_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), mar = c(4.0, 4.2, 3.3, 1.0),
    mgp = c(2.4, 0.8, 0), cex = 0.85)

chain_colors <- c("#2378AD", "#E18A3B", "#23844D", "#8E5AA9")
parameter_titles <- c("Intercept trace", "Advertising trace",
                      "Holiday trace", "Residual SD trace")
for (j in seq_len(p + 1L)) {
  plot(chains[[1]][, j], type = "l", col = chain_colors[1],
       xlab = "Post-burn-in iteration", ylab = colnames(chains[[1]])[j],
       main = parameter_titles[j])
  for (chain_id in 2:n_chains) {
    lines(chains[[chain_id]][, j], col = chain_colors[chain_id])
  }
  legend("topright", legend = paste("Chain", 1:4),
         col = chain_colors, lty = 1, bty = "n", cex = 0.72)
}
par(old_par)
dev.off()

cat("\nDiagnostic plot saved to:\n", plot_file, "\n")
