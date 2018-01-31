# Example 16-4: Two-way Design

# 請修改資料檔路徑
M = as.matrix(read.table("c:/r/twoway-2.txt"))

n = 12*6
Y = numeric(n)
FactorA = character(n)
FactorB = character(n)
k = 1
for (i in 1:12)
{
  i2 = ceiling(i/2)				# B 因子每兩列同屬一個分類
  for (j in 1:6)
  {
     Y[k] = M[i,j]
     FactorA[k] = switch(j,"A1","A2","A3","A4","A5","A6")
     FactorB[k] = switch(i2,"B1","B2","B3","B4","B5","B6")
     k = k+1
  }
}
data = data.frame(Y, FactorA, FactorB)
data

tapply(data$Y,list(data$FactorA),mean)
tapply(data$Y,list(data$FactorB),mean)

tapply(data$Y,list(data$FactorA,data$FactorB),mean)

devAskNewPage(ask = TRUE)

plot(data$FactorA,data$Y,xlab="殺蟲劑")

devAskNewPage(ask = TRUE)

plot(data$FactorB,data$Y,xlab="作物種類")


attach(data)
devAskNewPage(ask = TRUE)
interaction.plot(FactorB, FactorA,Y,col=1:6)
devAskNewPage(ask = TRUE)
interaction.plot(FactorA,FactorB,Y,col=1:6)

twoway.model = lm(Y ~ FactorA*FactorB, data=data)
anova(twoway.model)

aov.model = aov(Y ~ FactorA*FactorB,data=data)
aov.model

summary(aov.model)		   # output與 anova(twoway.model) 相同

devAskNewPage(ask = TRUE)
plot(twoway.model)

