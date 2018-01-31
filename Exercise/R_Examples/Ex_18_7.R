# Example 18-7 : Dowjonse stock rate

# 請修改資料檔路徑
dowjonse = read.table("c:/r/dowjonse.txt", header = T)
head(dowjonse)

Z = dowjonse$stock
Z.ts = ts(dowjonse$stock)
rate.ts = Z.ts/lag(Z.ts,-1) - 1
length(rate.ts)

library(TSA)			# 使用 TSA 套件的 zlag 函數
Z2 = zlag(Z,1)
rate = Z/zlog(Z,1) - 1
length(rate)

head(rate)
rate = rate[-1]			# 刪除第一筆 NA 紀錄
length(rate)
head(rate)

library(TSA)
kurtosis(rate)

library(FinTS)
ArchTest(rate)

ts.plot(rate)

devAskNewPage(ask = TRUE)
acf(rate)

devAskNewPage(ask = TRUE)
pacf(rate)

eacf(rate)	# TSA 套件

devAskNewPage(ask = TRUE)
plot(zlag(rate,1),rate,xlab="rate(t-1)",ylab="rate(t)",type="o")


summary(garch(rate,c(1,1),trace=F))

library(fGarch)
garchFit(~ garch(1,1),data=rate,trace=F)

garchFit(~arma(1,0)+garch(1,1),data=rate,trace=F)

for (i in 0:4)
{
   k = 764 + i
   p = predict(garchFit(~ arma(1,0)+garch(1,1),
		data=rate[1:k], trace=F),n.ahead = 1 )
   print(p)
}

