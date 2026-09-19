#电商用户满意度参数估计报告
#读取数据文件地址
data_dir <- "D:/Users/hanjiaxuan/Desktop/课程内容/管理统计与机器学习/作业二_参数估计"
data_file <- file.path(data_dir, "ecommerce_survey.csv")

#读取文件数据
data <- read.csv(data_file, stringsAsFactors = FALSE)
stopifnot(nrow(data) == 200, !anyNA(data),
          is.numeric(data$satisfaction_score),
          all(data$satisfaction_score >= 1 & data$satisfaction_score <= 10))
n <- nrow(data)

#数据描述
descriptive_result <- data.frame(
  有效样本量 = n,
  样本标准差 = round(sd(data$satisfaction_score), 4)
)
cat("\n数据基本情况\n")
print(descriptive_result, row.names = FALSE)


#均值的点估计及 95% 置信区间
t_result <- t.test(data$satisfaction_score, conf.level = 0.95)
mean_result <- data.frame(
  平均满意度 = round(mean(data$satisfaction_score), 4),
  置信区间下限 = round(t_result$conf.int[1], 4),
  置信区间上限 = round(t_result$conf.int[2], 4)
)
cat("\n平均满意度估计结果\n")
print(mean_result, row.names = FALSE)
cat("\nt.test 原始输出\n")
print(t_result)

#计算高满意度比例及 95% 置信区间
data$high <- as.integer(data$satisfaction_score > 8)
high_count <- sum(data$high)
p_result <- prop.test(high_count, n, conf.level = 0.95, correct = TRUE)
proportion_result <- data.frame(
  高满意度人数 = high_count,
  总人数 = n,
  高满意度比例 = paste0(round(100 * high_count / n, 2), "%"),
  置信区间下限 = paste0(round(100 * p_result$conf.int[1], 2), "%"),
  置信区间上限 = paste0(round(100 * p_result$conf.int[2], 2), "%")
)
cat("\n高满意度比例估计结果\n")
print(proportion_result, row.names = FALSE)
cat("比例区间采用连续性校正。\n")
cat("\nprop.test 原始输出\n")
print(p_result)


