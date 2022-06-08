#--------数据处理--------------------------
library(readxl)
library(tidyr)
library(tidyverse)
library(stargazer)
data <- read_excel("D:/data2.xls")
View(data)

data <- as_tibble(data)
data$最新股票名称_Lstknm <- as.factor(data$最新股票名称_Lstknm)
data <- data %>%
  group_by(最新股票名称_Lstknm) %>%
  spread(最新股票名称_Lstknm, '日收益标准差_20日移动平均_Dstd20')
head(data)

library(VIM)
aggr(data)
data <- data[,c(-15)]
data$荣泰健康 <- ifelse(is.na(data$荣泰健康) == TRUE, 0, data$荣泰健康)
data$三花智控 <- ifelse(is.na(data$三花智控) == TRUE, 0, data$三花智控)
data$格力电器 <- ifelse(is.na(data$格力电器) == TRUE, 0, data$格力电器)
data$美的集团 <- ifelse(is.na(data$美的集团) == TRUE, 0, data$美的集团)
data$TCL科技 <- ifelse(is.na(data$TCL科技) == TRUE, 0, data$TCL科技)
data$奥佳华 <- ifelse(is.na(data$奥佳华) == TRUE, 0, data$奥佳华)
data$飞科电器 <- ifelse(is.na(data$飞科电器) == TRUE, 0, data$飞科电器)
data$苏泊尔 <- ifelse(is.na(data$苏泊尔) == TRUE, 0, data$苏泊尔)
data$海尔智家 <- ifelse(is.na(data$海尔智家) == TRUE, 0, data$海尔智家)
data$奥佳华 <- ifelse(is.na(data$奥佳华) == TRUE, 0, data$奥佳华)
data$海信视像 <- ifelse(is.na(data$海信视像) == TRUE, 0, data$海信视像)
data$华帝股份 <- ifelse(is.na(data$华帝股份) == TRUE, 0, data$华帝股份)
data$九阳股份 <- ifelse(is.na(data$九阳股份) == TRUE, 0, data$九阳股份)
data$兆驰股份 <- ifelse(is.na(data$兆驰股份) == TRUE, 0, data$兆驰股份)
data$万和电气 <- ifelse(is.na(data$万和电气) == TRUE, 0, data$万和电气)
data$日出东方 <- ifelse(is.na(data$日出东方) == TRUE, 0, data$日出东方)
aggr(data)
data <- as.data.frame(data)
stargazer(data, title = "Table 1. descriptive statistic",type ="html",
          out='D:/descriptive statistic.txt')

#--------------------模型检验------------------------------
#install.packages("pampe")
#install.packages("leaps")
library(pampe)
library(leaps)
??pample

treated <- "华帝股份"
time.pretr <- 1:607
time.tr <- 608:677
possible.ctrls <- c('奥佳华','TCL科技','飞科电器','格力电器','海尔智家','海信家电',
                    '莱克电气','老板电器','美的集团','海信视像','九阳股份','四川九洲',
                    '苏泊尔','新宝股份','三花智控','浙江美大','荣泰健康','日出东方','兆驰股份')

pol.integ1 <- pampe(time.pretr = time.pretr, time.tr = time.tr, treated = treated,
                    controls = possible.ctrls, data = data,select = "AIC")
pol.integ2 <- pampe(time.pretr = time.pretr, time.tr = time.tr, treated = treated,
                    controls = possible.ctrls, data = data,select = "BIC")
summary(pol.integ1)
summary(pol.integ2)
#stargazer(pol.integ2, title = "Results", align = TRUE)
plot(pol.integ)
plot(pol.integ2)

#  A plot of the actual Hong Kong together with the predicted path
matplot(c(time.pretr, time.tr), pol.integ2$counterfactual, type = "l", xlab = "",
         ylab = "华帝股份的平均波动率", ylim = c(0, 0.05), col = 1, lwd = 2, xaxt = "n")

axis(1, at = c(time.pretr, time.tr)[c(seq(2, length(c(time.pretr, time.tr)),
                by = 2))], labels = c(rownames(data$日期_Date)[c(time.pretr, time.tr)
                [c(seq(2, length(c(time.pretr, time.tr)), by = 2))]]), las = 3)
title(xlab = "Time", mgp = c(3.6, 0.5, 0))
legend("bottomright", c("Actual", "predicted"),
       col = 1, lty = c(1, 2), lwd = 1)
abline(v = time.pretr[length(time.pretr)], lty = 3, lwd = 2,col = 2)

# A plot of the estimated treatment effect
tr.effect <- pol.integ2$counterfactual[, 1] - pol.integ2$counterfactual[, 2]
plot(c(time.pretr, time.tr), tr.effect, type = "l", ylab = "华帝股份的平均波动率",
     xlab = "", col = 1, lwd = 2, xaxt = "n", ylim = c(-0.05, 0.05))
axis(1, at = c(time.pretr, time.tr)[c(seq(2, length(c(time.pretr, time.tr)),
              by = 2))], labels = c(rownames(data)[c(time.pretr, time.tr)
              [c(seq(2, length(c(time.pretr, time.tr)), by = 2))]]), las = 3)
title(xlab = "Time", mgp = c(3.6, 0.5, 0))
legend("topleft", "Treatment Effect", col = 1, lty = 1, lwd = 2)
abline(v = time.pretr[length(time.pretr)], lty = 3, lwd = 2)
abline(h = 0, lty = 3, lwd = 2)

# 安慰剂检验
pol.integ.placebos <- pampe(time.pretr = time.pretr, time.tr = time.tr,
                            treated = treated, controls = possible.ctrls,
                            data = data, placebos = "Both", select = "BIC")
mspe <- pol.integ.placebos$placebo.ctrl$mspe
linewidth <- matrix(2, 1, ncol(mspe) - 1)
linewidth <- append(linewidth, 5, after = 0)
matplot(c(time.pretr, time.tr), pol.integ.placebos$placebo.ctrl$tr.effect,
        type = "l", xlab = "", ylab = "华帝股份的平均波动率",
        col = c("red", matrix(1, 1, ncol(mspe) - 1)),
        lty = c(1, matrix(2, 1, ncol(mspe) - 1)), lwd = linewidth,
        ylim = c(-0.05, 0.05), xaxt = "n")

axis(1, at = c(time.pretr, time.tr)[c(seq(2, length(c(time.pretr, time.tr)),
                by = 2))], labels = c(rownames(data)[c(time.pretr, time.tr)
                [c(seq(2, length(c(time.pretr, time.tr)), by = 2))]]), las = 3)
title(xlab = "Time", mgp = c(3.6, 0.5, 0))
legend("bottomleft", c("Treated", "Controls"), col = c("red", 1),
      lty = c(1, 2), lwd = c(5, 2))
abline(h = 0, lty = 3, lwd = 2)
abline(v = time.pretr[length(time.pretr)], lty = 3, lwd = 2)

#--------------------------------------------------------------------------------------
treated <- "华帝股份"
time.pretr <- 1:156
time.tr <- 157:677
possible.ctrls <- c('奥佳华','TCL科技','飞科电器','格力电器','海尔智家','海信家电',
                    '莱克电气','老板电器','美的集团','海信视像','九阳股份','四川九洲',
                    '苏泊尔','新宝股份','三花智控','浙江美大','荣泰健康','日出东方','兆驰股份')
pol.integ.placebos <- pampe(time.pretr = time.pretr, time.tr = time.tr, treated = treated,
                    controls = possible.ctrls, data = data,select = "BIC",placebos = "controls")
summary(pol.integ.placebos)
#stargazer(pol.integ2, title = "Results", align = TRUE)
plot(pol.integ.placebos)

#-----------------------------------------------------------------------------------------
placebo.in.time1 <- pol.integ.placebos$placebo.time$tr.effect[, 2] +
  data[c(time.pretr, time.tr), 1]
matplot(c(time.pretr, time.tr), cbind(data[c(time.pretr, time.tr), 1],
          pol.integ.placebos$counterfactual, placebo.in.time1), type = "l",
       ylab = "GDP growth", xlab = "", ylim = c(0, 0.05), col = 1,
       lwd = 2, xaxt = "n")
axis(1, at = c(time.pretr, time.tr)[c(seq(2, length(c(time.pretr, time.tr)),
                by = 2))], labels = c(rownames(data)[c(time.pretr, time.tr)
               [c(seq(2, length(c(time.pretr, time.tr)), by = 2))]]), las = 3)

title(xlab = "Time", mgp = c(3.6, 0.5, 0))
legend("bottomleft", c("actual", "predicted", paste("placebo",
       colnames(pol.integ.placebos$placebo.time$tr.effect)[2], sep = " ")),
       col = 1, lty = c(1, 2, 3), lwd = 2)
abline(v = time.pretr[length(time.pretr)], lty = 2, lwd = 3)
abline(v = which(colnames(pol.integ.placebos$placebo.time$tr.effect)[2]
                   == rownames(data)), lty = 3, lwd = 3)


       
















