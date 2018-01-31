# Example 17-6

# 請修改資料檔路徑
radius2 = read.table("c:/r/radius2.txt",header=T)
radius2

q.xbarone = qcc(data=radius2$radius,type="xbar.one", add.stats=FALSE )

q.xbarone$center
q.xbarone$limits

library(qAnalyst)
radius.i=spc(x=radius2$radius,sg=2,type="i", name="Radius", testType=1,k=1,p=1,nSigma=3)

plot(radius.i)

devAskNewPage(ask = TRUE)

radius.mr=spc(x=radius2$radius,sg=2,type="mr" ,name="Radius", testType=1,k=1,p=1,nSigma=3)
plot(radius.mr)

