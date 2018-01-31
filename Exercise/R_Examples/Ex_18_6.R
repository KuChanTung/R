# Example 18-6: LA Ozone data with Outliers

# 請修改資料檔路徑
LA = read.table("c:/r/ozone.txt",header=T)

head(LA)

LA2 = LA[1:216, ]

AO23 = ( LA2$index == 23)*1
AO35 = ( LA2$index == 35)*1
AO45 = ( LA2$index == 45)*1
Xmatrix4 = cbind(Xmatrix2,AO23,AO35,AO45)

LA.result5 = arima(LA2$ozone, order = c(0, 0, 1),
    seasonal = list(order = c(0, 1, 1), period = 12),  
    xreg=Xmatrix4 )

AO21 = ( LA2$index == 21)*1
TC39 = ( LA2$index == 39)*1
TC43 = ( LA2$index == 43)*1

X2new = diffinv(LA$X2,12,1)
X2new = X2new[-c(1:12)]		# 去除最前面多餘的 12 個元素
X3new = diffinv(LA$X3,12,1)
X3new = X3new[-c(1:12)]		# 去除最前面多餘的 12 個元素

Xmatrix = cbind(X1 = LA$X1, X2 = X2new, X3 = X3new)
Xmatrix2 = Xmatrix[1:216, ]		# 取出最前面 216 列
XmatrixAO = cbind(Xmatrix2, AO21)	# 包含 X1,X2,X3,AO21

library(TSA)					# 使用 TSA 套件的 arima 函數 
Outliers.result1 = arima(LA2$ozone, order = c(0, 0, 1),
     seasonal = list(order=c(0,1,1),period = 12),
     xreg=XmatrixAO
     xtransf = cbind(TC39,TC43)
     transfer = list(c(1,0),c(1,0))
 )

Outliers.result1


# library(TSA)
Outliers.result2 = arima(LA2$ozone, order = c(0, 0, 1),
       seasonal = list(order = c(0, 1, 1), period = 12),
       xreg=Xmatrix2,			# Xmatrix2 含 X1,X2,X3
       xtransf = cbind(AO21,TC39,TC43), 
       transfer = list(c(0,0),c(1,0),c(1,0))
 )

Outliers.result2

TC39new = filter(TC39,c(0.7),method="recursive")
TC43new = filter(TC43,c(0.7),method="recursive")

Xmatrix3 = cbind(Xmatrix2,AO21,TC39=TC39new,TC43=TC43new)

Outliers.result3 = arima(LA2$ozone, order = c(0, 0, 1),
      seasonal = list(order=c(0,1,1),period = 12),
      xreg=Xmatrix3  )
 
Outliers.result3

