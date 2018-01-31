# Example 18-1 : TRTC data

# 請修改資料檔路徑
trtc = read.table("c:/R/trtc.dat",header=TRUE)

attach(trtc)
Z.ts = ts(trtc$Z,start=c(2003,363),frequency=365)
Z.ts

ts.plot(Z)

devAskNewPage(ask = TRUE)

plot(Z.ts)

weekday = D1 + 2*D2 + 3*D3 + 4*D4 + 5*D5 + 6*D6
weekday

devAskNewPage(ask = TRUE)
monthplot(Z.ts,phase=weekday)

devAskNewPage(ask = TRUE)
acf(Z,lag.max=35)

devAskNewPage(ask = TRUE)
pacf(Z,lag.max=35)

Z2 = diff(Z,7,1)
devAskNewPage(ask = TRUE)
acf(Z2,36)
devAskNewPage(ask = TRUE)
pacf(Z2,36)
devAskNewPage(ask = TRUE)
ts.plot(Z2)

adf.test(Z2)
pp.test(Z2)

library(TSA)
eacf(Z2,15,15)	

for (p in c(0,1)) {
	for (q in c(0,1)) {
	 for (P in c(0,1)) {
	  for (Q in c(0,1)) {
		result = arima(trtc$Z, order = c(p, 1, q), 
		seasonal = list(order = c(P, 1, Q), period = 7))
		aic = result$aic

		cat("p = ",p," q = ",q," P = ",P," Q = ",
		Q," AIC = ", aic," \n")
}}}}


Z.result = arima0(trtc$Z, order = c(1, 0, 1),
           seasonal = list(order = c(0, 1, 1), period = 7))
Z.result

library(forecast)
auto.arima(trtc$Z)

Z.result = arima0(Z, order = c(1, 0, 0),
           seasonal = list(order = c(0, 1, 1), period = 7))
Z.result

names(Z.result)
(  BIC = -2*( Z.result$loglik ) + 3*log(length(Z2))  )

Z.result$loglik

ts2 = arima(number.ts, order = c(0, 1, 4), 
        transform.pars = FALSE,fixed=c(0,0,0,NA,NA), 
        seasonal = list(order = c(0, 1, 1), period = 12))
ts2

BIC = -2*ts2$loglik + 3*log(length(mydata$number)
BIC

residuals =Z$residuals

devAskNewPage(ask = TRUE)
oldpar = par()
par(mfrow=c(2,2))

ts.plot(residuals)
abline(h = 0)

qqnorm(residuals)
qqline(residuals)

acf(residuals)
pacf(residuals)

par(oldpar)

adf.test(residuals)

pp.test(residuals)
kpss.test(residuals)
shapiro.test(residuals)

library(nortest)
cvm.test(residuals)

sf.test(residuals)

Box.test(residuals,type="Box-Pierce")	
Box.test(residuals,type="Ljung-Box")
eacf(residuals)

devAskNewPage(ask = TRUE)
tsdiag(Z.result)

length(residuals)

length(Z)

Z3 = Z[8:399]
Z.fit = Z3 + residuals

devAskNewPage(ask = TRUE)
plot(300:399,Z[300:399],type="l")
lines(300:399,Z.fit[293:392],lty=3)

Z.pred10 = predict(Z.result,n.ahead=10)
Z.pred10
Z.real_10 = c(1038642,1050005,1040893,1046401,1070204,
         948585,642983,558409,295688,684943)
         
minz = min(Z.real_10,Z.pred10$pred)
maxz = max(Z.real_10,Z.pred10$pred)

devAskNewPage(ask = TRUE)
plot(400:409,Z.real_10,ylim=c(minz,maxz),type="l")
lines(400:409,Z.pred10$pred,lty=2)

z.error10 = Z.real_10 - Z.pred10$pred
MAPE = mean(abs(z.error10)/Z.real_10)
MAPE

MAPE = mean(abs(z.error10[1:6])/Z.real_10[1:6])
MAPE
         

