#实验四
options(digits = 8, scipen = 99)
RNGkind('Mersenne-Twister', 'Inversion', 'Rejection')
H <- '#0072B2'; L <- '#D55E00'; ink <- '#252A34'
grey <- '#87939F'; pale <- '#E8EEF3'
plot_style <- function() par(family = 'Arial', las = 1, bty = 'l',
  mar = c(4.6, 4.8, 3.8, 1.2), mgp = c(2.8, .8, 0), tcl = -.22,
  cex.axis = 1, cex.lab = 1, cex.main = 1.1, col.axis = ink, col.lab = ink, fg = ink)

show_plot <- function(name, draw) {
 
  if (!interactive() && dev.cur() == 1L) return(invisible(NULL))
  plot_style()
  draw()
  cat('已显示图片：', name, '\n')
}

# 一、数据生成
cat('\n【一、数据生成】\n')

# 1.1 用户特征。
set.seed(20261103)
N <- 20000L
user_id <- sprintf('U%05d', seq_len(N))
income <- rlnorm(N, meanlog = log(10000), sdlog = .45)
health_need <- rbinom(N, size = 1, prob = .35)
repeat_customer <- rbinom(N, size = 1, prob = .30)

# 1.2 随机报价
group <- sample(rep(c('H', 'L'), each = N/2))
price <- ifelse(group == 'H', 380, 298)
users <- data.frame(user_id, income, health_need, repeat_customer, group, price)
print(users)
# 1.3 支付身份
z_generator <- (log(users$income)-log(10000))/.45
weight <- exp(.45*z_generator + .70*users$health_need + .40*users$repeat_customer +
              .15*users$health_need*(users$group == 'H'))
users$paid <- 0L
paid_count <- c(H = 250L, L = 280L)
for (g in c('H', 'L')) {
  ids <- which(users$group == g)
  selected <- sample(ids, size = paid_count[g], replace = FALSE, prob = weight[ids])
  users$paid[selected] <- 1L
}
users$revenue <- users$price*users$paid  # 订单金额
users$exposure <- 1L
users$payment_weight <- weight        # 抽样权重
u <- users                            
print(u)
# 1.4 训练标签
u$income_index <- pnorm((log(u$income)-log(10000))/.45)
u$feature_base <- with(u, .20*income_index+.35*health_need+.20*repeat_customer)
u$score <- 10*(u$feature_base+.25*u$paid)
stopifnot(nrow(u) == 20000L, !anyDuplicated(u$user_id), !anyNA(u),
  all(u$income > 0), all(u$score >= 0 & u$score <= 10),
  all(u$revenue == u$price*u$paid), all(table(u$group) == 10000L))

# 查看数据
print(head(u[, c('user_id','group','income','health_need','repeat_customer','paid','score')]))
feature_summary <- do.call(rbind, lapply(c('H','L'), function(g) {
  v <- u[u$group == g, ]
  data.frame(group = g, n = nrow(v), mean_income = mean(v$income),
    health_rate = mean(v$health_need), repeat_rate = mean(v$repeat_customer),
    mean_score = mean(v$score), sd_score = sd(v$score))
}))


# 图1
income_breaks <- pretty(range(log10(u$income)), n = 40)
income_H <- hist(log10(u$income[u$group == 'H']), breaks = income_breaks, plot = FALSE)
income_L <- hist(log10(u$income[u$group == 'L']), breaks = income_breaks, plot = FALSE)
show_plot('Fig01_income', function() {
  plot(income_H$mids, income_H$counts, type = 'n',
    xlim = range(income_breaks), ylim = c(0,max(income_H$counts,income_L$counts)*1.25),
    xlab = 'log10 income (CNY / month)', ylab = 'Users (count)', main = 'Generated monthly income')
  xx <- rep(income_breaks,each = 2)[-c(1,2*length(income_breaks))]
  lines(xx,rep(income_H$counts,each = 2),col = H,lwd = 1.6)
  lines(xx,rep(income_L$counts,each = 2),col = L,lwd = 1.6,lty = 2)
  legend('topright', c('H','L'), col = c(H,L), lty = c(1,2), lwd = 1.6, bty = 'n')
})

# 图2
feature_rates <- rbind(feature_summary$health_rate,feature_summary$repeat_rate)*100
show_plot('Fig02_feature_balance', function() {
  mids <- barplot(t(feature_rates), beside = TRUE, col = c(H,L), border = NA,
    names.arg = c('Health need','Repeat customer'), ylim = c(0,50),
    ylab = 'Generated users (%)', main = 'Feature balance after random assignment')
  legend('topright', c('H','L'), fill = c(H,L), border = NA, bty = 'n', horiz = TRUE)
})

# 图3
payment <- table(factor(u$group,levels = c('H','L')),
                 factor(u$paid,levels = c(0,1)))
colnames(payment) <- c('Unpaid','Paid')
show_plot('Fig03_payment_composition', function() {
  pos <- barplot(t(payment[2:1,]),horiz = TRUE,col = c(pale,H),border = grey,
    names.arg = c('L','H'),xlim = c(0,12500),ylim = c(0,3.1),xlab = 'Users (count)',
    main = 'Payment composition')
  rect(payment['L','Unpaid'],pos[1]-.5,10000,pos[1]+.5,col = L,border = grey)
  text(10150,pos,sprintf('%d paid',payment[2:1,'Paid']),adj = 0)
  legend('top',c('Unpaid','Paid'),fill = c(pale,H),border = grey,bty = 'n',horiz = TRUE)
})

# 图4
score_breaks <- seq(0,10,.25)
score_H <- hist(u$score[u$group == 'H'], breaks = score_breaks, plot = FALSE)
score_L <- hist(u$score[u$group == 'L'], breaks = score_breaks, plot = FALSE)
score_hist <- data.frame(left = head(score_breaks,-1), right = tail(score_breaks,-1),
  H_count = score_H$counts, L_count = score_L$counts,
  H_density = score_H$density, L_density = score_L$density)
show_plot('Fig04_composite_score', function() {
  plot(score_H$mids, score_H$density, type = 'n',
    xlim = c(0,10), ylim = c(0,max(score_H$density,score_L$density)*1.3),
    xlab = 'Composite score (0-10 points)', ylab = 'Density (per score point)',
    main = 'Training label: composite score')
  xx <- rep(score_breaks,each = 2)[-c(1,2*length(score_breaks))]
  lines(xx,rep(score_H$density,each = 2),col = H,lwd = 1.6)
  lines(xx,rep(score_L$density,each = 2),col = L,lwd = 1.6,lty = 2)
  legend('topright', c('H','L'), col = c(H,L), lty = c(1,2), lwd = 1.6, bty = 'n')
})

# A/B实验
cat('\n【二、A/B实验】\n')
# 
ab <- do.call(rbind,lapply(c('H','L'),function(g) {
  v <- u[u$group == g, ]
  data.frame(group = g, price = v$price[1], exposed = nrow(v), paid = sum(v$paid),
    unpaid = sum(1-v$paid), rate = mean(v$paid), revenue = sum(v$revenue), rpu = mean(v$revenue))
}))

# 2.2 两独立样本比例Z检验
x <- setNames(ab$paid,ab$group)
n <- setNames(ab$exposed,ab$group)
alpha <- .05
p_hat <- x/n
pooled <- sum(x)/sum(n)
SE0 <- sqrt(pooled*(1-pooled)*sum(1/n))
observed_difference <- unname(p_hat['H']-p_hat['L'])
null_difference <- 0                    # 分子中“减0”的来源。
Z <- (observed_difference-null_difference)/SE0
p_Z <- 2*pnorm(-abs(Z))                 # 双侧尾面积。
z_result <- data.frame(pooled, SE0, observed_difference, null_difference,
  Z, two_sided_p = p_Z, reject_at_005 = p_Z < alpha)


# 2.3 实际订单营收
revenue_H <- ab$revenue[1]; revenue_L <- ab$revenue[2]
rpu_H <- ab$rpu[1]; rpu_L <- ab$rpu[2]
revenue_result <- data.frame(revenue_H,revenue_L,difference = revenue_H-revenue_L,
  lift_percent = 100*(revenue_H-revenue_L)/revenue_L,
  rpu_H,rpu_L,rpu_difference = rpu_H-rpu_L)


# 图5
zz <- seq(-4.5,4.5,length.out = 3001)
pp <- zz*SE0*100
normal_reference <- data.frame(Z = zz,density = dnorm(zz),difference_pp = pp,
  difference_density = dnorm(zz)/(SE0*100))
show_plot('Fig05_rate_difference_reference', function() {
  plot(pp,dnorm(zz)/(SE0*100),type = 'l',lwd = 1.6,col = ink,
    ylim = c(0,2.8),xlab = 'H - L (percentage points)',ylab = 'Density (per percentage point)',
    main = 'Reference distribution under equal payment rates')
  segments(observed_difference*100,0,observed_difference*100,
    dnorm(Z)/(SE0*100),col = H,lty = 2,lwd = 1.3)
  text(min(pp),2.55,sprintf('SE = %.4f percentage points',SE0*100),adj = 0)
})

# 图6
show_plot('Fig06_Z_test', function() {
  yy <- dnorm(zz)
  plot(zz,yy,type = 'n',ylim = c(0,.62),xlab = 'Z',ylab = 'Density',
    main = 'Two-sided Z reference test')
  for (side in c(-1,1)) {
    index <- if(side < 0) zz <= -abs(Z) else zz >= abs(Z)
    polygon(c(zz[index],rev(zz[index])),c(yy[index],rep(0,sum(index))),
      col = '#A5CBDD',border = NA)
    index <- if(side < 0) zz <= -qnorm(.975) else zz >= qnorm(.975)
    polygon(c(zz[index],rev(zz[index])),c(yy[index],rep(0,sum(index))),col = H,border = NA)
  }
  lines(zz,yy,lwd = 1.5,col = ink)
  segments(Z,0,Z,dnorm(Z),col = L,lwd = 1.4)
  text(-4.3,.56,sprintf('Z = %.4f; two-sided p = %.4f',Z,p_Z),adj = 0)
  legend('topright',c('P-value tails','5% rejection region'),
    fill = c('#A5CBDD',H),border = NA,bty = 'n',cex = .85,inset = c(0,.10))
})

# 5x2折分析
cat('\n【三、5x2折分析】\n')

# 3.1 五轮整体随机平分
set.seed(20261104)
fold_rows <- predictions <- memberships <- coefficients <- list()
first <- NULL
for (i in 1:5) {
  S1 <- sample.int(N,N/2)
  S2 <- setdiff(seq_len(N),S1)
  for (j in 1:2) {
    train_id <- if(j == 1) S1 else S2
    test_id <- if(j == 1) S2 else S1
    stopifnot(!length(intersect(train_id,test_id)))
    train <- u[seq_len(N) %in% train_id, ]
    test <- u[seq_len(N) %in% test_id, ]
    train_H <- train[train$group == 'H', ]
    train_L <- train[train$group == 'L', ]

  
    model_H <- lm(score ~ income_index+health_need+repeat_customer,data = train_H)
    model_L <- lm(score ~ income_index+health_need+repeat_customer,data = train_L)

   
    raw_H <- predict(model_H,newdata = test)
    raw_L <- predict(model_L,newdata = test)
    sH <- pmin(10,pmax(0,raw_H))
    sL <- pmin(10,pmax(0,raw_L))
    aH <- 380*sH/100
    aL <- 298*sL/100
    k <- (i-1L)*2L+j
    fold_rows[[k]] <- data.frame(repeat_id = i,fold = j,
      train_H = nrow(train_H),train_L = nrow(train_L),
      test_H = sum(test$group == 'H'),test_L = sum(test$group == 'L'),
      predicted_score_H = mean(sH),predicted_score_L = mean(sL),
      predicted_H = mean(aH),predicted_L = mean(aL),difference = mean(aH-aL))
    predictions[[k]] <- data.frame(repeat_id = i,fold = j,user_id = test$user_id,
      group = test$group,score = test$score,predicted_score_H = sH,predicted_score_L = sL,
      converted_H = aH,converted_L = aL)
    memberships[[k]] <- data.frame(repeat_id = i,test_fold = j,user_id = test$user_id)
    for (g in c('H','L')) {
      cc <- coef(if(g == 'H') model_H else model_L)
      coefficients[[length(coefficients)+1L]] <- data.frame(repeat_id = i,fold = j,
        group = g,term = names(cc),estimate = unname(cc))
    }
    if(i == 1 && j == 1) {
      first <- list(train = train,test = test,
        fit = list(model_H = model_H,model_L = model_L,sH = sH,sL = sL,aH = aH,aL = aL),
        row = fold_rows[[k]])
    }
  }
}
ff <- do.call(rbind,fold_rows)
membership <- do.call(rbind,memberships)
pred <- do.call(rbind,predictions)
first_split <- data.frame(set = c('Train S1','Test S2','All users'),
  H = c(first$row$train_H,first$row$test_H,10000),
  L = c(first$row$train_L,first$row$test_L,10000),total = c(10000,10000,20000))
cat('\n第一轮人数\n'); print(first_split)
cat('\n第一折模型与预测\n'); print(first$row)
print(coef(first$fit$model_H)); print(coef(first$fit$model_L))

# 十折汇总
fold_table <- do.call(rbind,lapply(1:5,function(i) {
  v <- ff[ff$repeat_id == i, ]
  data.frame(repeat_id = i,H1 = v$predicted_H[1],L1 = v$predicted_L[1],d1 = v$difference[1],
    H2 = v$predicted_H[2],L2 = v$predicted_L[2],d2 = v$difference[2])
}))
cat('\n五轮两折（PDF第15页）\n'); print(round(fold_table,6))
d <- matrix(ff$difference,ncol = 2,byrow = TRUE)
repeat_means <- rowMeans(d)
cat('十折平均差：',mean(d),'\n')

#轮内方差
s2 <- rowSums(sweep(d,1,repeat_means,'-')^2)
stopifnot(max(abs(s2-(d[,1]-d[,2])^2/2)) < 1e-12)
D <- sqrt(mean(s2))
variance_table <- data.frame(repeat_id = 1:5,d1 = d[,1],d2 = d[,2],
  mean = repeat_means,variance_term = s2)
cat('\n轮内方差与D\n'); print(variance_table); print(c(sum_s2 = sum(s2),D = D))

# t/p统计量计算
t_case <- (repeat_means[1]-0)/D
p_ref <- pt(t_case,df = 5,lower.tail = FALSE)
reference_result <- data.frame(mean_all_folds = mean(d),first_repeat_mean = repeat_means[1],
  sum_s2 = sum(s2),D = D,case_t = t_case,df = 5,case_reference_p = p_ref,
  case_rule_rejects = p_ref < alpha,calibrated_real_revenue_test = FALSE)
cat('\t与p\n'); print(reference_result)
print(reference_result)
# 图7
show_plot('Fig07_CV_differences', function() {
  plot(1:5,d[,1],type = 'n',ylim = c(0,max(d)*1.4),xaxt = 'n',
    xlab = 'Repeat',ylab = 'Converted H - L (uncalibrated CNY / user)',
    main = 'Repeat mean and complete fold range')
  axis(1,at = 1:5)
  segments(1:5,d[,1],1:5,d[,2],col = grey,lwd = 1.3)
  points(1:5,d[,1],pch = 16,col = H,cex = 1)
  points(1:5,d[,2],pch = 17,col = L,cex = 1)
  segments((1:5)-.06,repeat_means,(1:5)+.06,repeat_means,col = ink,lwd = 2)
  legend('top',c('Fold 1','Fold 2','Repeat mean'),col = c(H,L,ink),
    pch = c(16,17,NA),lty = c(NA,NA,1),lwd = c(1,1,2),bty = 'n',horiz = TRUE)
})

# 图8
tt <- seq(-4,4,length.out = 2001)
show_plot('Fig08_t5_central_reference', function() {
  yy <- dt(tt,5)
  plot(tt,yy,type = 'n',ylim = c(0,.62),xlab = 't reference (df = 5)',ylab = 'Density',
    main = 'Central t5 reference window')
  index <- tt >= qt(.95,5)
  polygon(c(tt[index],rev(tt[index])),c(yy[index],rep(0,sum(index))),col = H,border = NA)
  lines(tt,yy,col = ink,lwd = 1.6)
  segments(qt(.95,5),0,qt(.95,5),.2,col = L,lty = 2)
  text(-3.9,.56,sprintf('Observed t = %.2f (outside this window)',t_case),adj = 0)
  legend('topright','5% right-tail region',fill = H,border = NA,bty = 'n',inset = c(0,.1))
})

# 图9
tail_t <- sort(unique(c(seq(0,80,length.out = 2401),t_case)))
tail_p <- pt(tail_t,5,lower.tail = FALSE)
stopifnot(all(tail_p > 0))
tail_reference <- data.frame(t = tail_t,right_tail = tail_p)
show_plot('Fig09_t5_right_tail', function() {
  plot(tail_t,tail_p,type = 'l',log = 'y',col = ink,lwd = 1.6,yaxt = 'n',
    ylim = c(1e-10,1),xlim = c(0,82),xlab = 't reference (df = 5)',
    ylab = 'Right-tail probability (log scale)',main = 'Observed t and its reference right-tail area')
  axis(2,at = 10^seq(-10,0,2),labels = paste0('1e',seq(-10,0,2)),cex.axis = .9)
  points(t_case,p_ref,pch = 16,col = H,cex = 1.2)
  text(4,.35,sprintf('t = %.4f; reference p = %.3g',t_case,p_ref),adj = 0)
})
