# Example 18-4: LA Ozone data

# 請修改資料檔路徑
LA = read.table("c:/r/ozone.txt",header=T)

LA

library(timeDate)
LA.time = timeSequence(from = "1955-01-01",
        by = "month",format="%Y-%m-%d",
        length.out=length(LA$ozone))

LA.time

LA.time = paste(substr(LA.time,1,4), substr(LA.time,6,7),sep="")
LA.time

LA$X1 = ( LA.time >= 196001 )*1
LA$X1       # 也可使用 LA$X1 = (LA$index >= 61)*1

LA.year = as.integer(substr(LA.time,1,4))
LA.month = as.integer(substr(LA.time,5,6))

LA$X2 = 1*( LA.year >= 1966 & LA.month %in% 6:10 )

LA$X3 = 1*((LA.year == 1966 & LA.month %in% 11:12) |(LA.year > 1966 & LA.month %in% c(1:5,11,12)))

LA2 = LA[1:216, ]

LA.result11 = arima0(LA2$ozone, order = c(0, 0, 1),
      seasonal = list(order = c(0, 1, 1), period = 12),
      xreg=LA2$X1)

LA.result11

LA.result12 = arima(LA2$ozone, order = c(0, 0, 1),
      seasonal = list(order = c(0, 1, 1), period = 12),
      xreg=LA2$X1)
LA.result12

X2new = diffinv(LA$X2,12,1)
length(X2new)
X2new = X2new[-c(1:12)]
X2new

X3new = diffinv(LA$X3,12,1)
X3new = X3new[-c(1:12)]		# 去除最前面多餘的 12 個元素
X3new

Xmatrix = cbind(X1 = LA$X1,X2 = X2new,X3 = X3new)
Xmatrix2 = Xmatrix[1:216, ]		# 取出最前面 216 列
Xmatrix2

LA.result2 = arima0(LA2$ozone, order = c(0, 0, 1),
     seasonal = list(order = c(0, 1, 1), period = 12),
     xreg=Xmatrix2 )
LA.result2

LA.result22 = arima(LA2$ozone, order = c(0, 0, 1),
		seasonal = list(order = c(0, 1, 1), period = 12),
		xreg=Xmatrix2)
LA.result22

Xmatrix12 = Xmatrix[217:228,]	# 取出最後 12 列的 X1,X2,X3
predict(LA.result22,n.ahead=12,newxreg=Xmatrix12 )









