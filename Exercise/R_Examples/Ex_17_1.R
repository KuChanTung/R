# Example 17-1: Cause-and-Effect Chart and Pareto Chart

library(qcc)

cause.and.effect(cause=list(
   What=c("看韓劇", "打電玩", "發呆", "唱卡拉OK"),
   Where=c("家裡", "網咖", "卡拉OK店"),
   How=c("心情不好", "電玩太迷人", "老師不帥", "不喜歡目前科系"),
   When=c("白天", "晚上", "深夜")),
   effect="成績不好")

defect = c(80, 67, 51, 20, 33)
names(defect) = c("員工訓練不足", "產品瑕疵", "客服溝通不良","媒體批評", "其他")

devAskNewPage(ask = TRUE)

pareto.chart(defect, ylab = "客戶抱怨次數", xlab = "Error causes", las=1)

library(qAnalyst)
pr = c("員工訓練不足", "產品瑕疵", "客服溝通不良", "媒體批評","其他")
prx = rep(pr,times=c(80,67,51,20,23))

devAskNewPage(ask = TRUE)

paretoChart(prx)
