# Example 18-2

# 請修改資料檔路徑
trtc = read.table("c:/R/trtc.dat",header=TRUE)

trtc385 = trtc[1:385,]

trtc385.lm = lm(Z~D1+D2+D3+D4+D5+D6,data=trtc385)

lm.residuals = trtc385.lm$residuals

acf(lm.residuals)

devAskNewPage(ask = TRUE)

pacf(lm.residuals)

attach(trtc385)
X = cbind(D1,D2,D3,D4,D5,D6) 
tsreg.result = arima(Z, order = c(1, 0, 0), xreg=X )
tsreg.result

trtc14 = trtc[386:399,]
trtc14

with(trtc14, { X.14 = cbind(D1,D2,D3,D4,D5,D6) } )
reg.pred14 = predict(tsreg.result,n.ahead=14,newxreg=X.14)
reg.pred14

devAskNewPage(ask = TRUE)
plot(386:399,trtc14$Z,type="l")			# 實際旅運值為實線
lines(386:399,reg.pred14$pred,lty=2)	

( Z.error14 = trtc14$Z - reg.pred14$pred )
( MAPE = mean(abs(Z.error14)/trtc14$Z) )

