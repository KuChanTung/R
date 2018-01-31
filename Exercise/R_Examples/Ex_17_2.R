# Example 17-2: Xbar,R,S charts

# 請修改資料檔路徑
radius = read.table("d:/r/radius.txt",header=T)
radius2 = read.table("d:/r/radius2.txt",header=T)



library(qcc)
qcc.options(bg.margin="white")
qcc.options(bg.figure="white")
q.xbar = qcc(data=radius,type="xbar")

devAskNewPage(ask = TRUE)
q.R = qcc(data=radius,type="R")

devAskNewPage(ask = TRUE)
q.S = qcc(data=radius,type="S")

q.xbar
q.xbar$center
q.xbar$limits
summary(q.xbar)

q.R$center
q.R$limits
q.S$center
q.S$limits

process.capability(q.xbar,spec.limits=c(0.4,3.0))

library(qAnalyst)
radius2.Xbar=spc(x=radius2$radius,sg=radius2$sample, type="xbar", name="Radius")

devAskNewPage(ask = TRUE)
plot(radius2.Xbar)

devAskNewPage(ask = TRUE)
radius.R =spc(x=radius2$radius,sg=radius2$sample, type="r", name="Radius")
plot(radius.R)

devAskNewPage(ask = TRUE)
radius.S =spc(x=radius2$radius,sg=radius2$sample, type="s", name="Radius")
plot(radius.S)

