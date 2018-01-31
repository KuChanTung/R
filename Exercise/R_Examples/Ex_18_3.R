# Example 18-3: Gas and CO2 data

# 請修改兩個檔案的路徑
gas = read.table("c:/r/gas.txt",header=T)
source("c:/r/predictTSA.R")


gasRate = gas$gasRate[1:286]
CO2 = gas$CO2[1:286]

library(TSA)
gasRate3 = zlag(gasRate,3)

model = arima(CO2,c(2,0,0),,include.mean=T, 
        xtransf=cbind(gasRate3), transfer=list(c(1,2)))
        
model

confint(model)

gasRate.model = arima(gasRate,c(3,0,0))
gasRate.pred = predict(gasRate.model, n.ahead=10)

gasRate.pred2 = c(gasRate,as.vector(gasRate.pred$pred))
newGasRate = zlag(gasRate.pred2, 3)

CO2.predict = predictTSA(model, n.ahead=10,
     newxtransf=cbind(newGasRate),transfer=list(c(1,2))) 

CO2.predict$pred

cbind(R.pred=as.vector(CO2.predict$pred),SAS.pred)

MAPE.R = sum(abs((gas$CO2[287:296] - CO2.predict$pred)/gas$CO2[287:296]))/10
MAPE.R

MAPE.SAS = sum(abs((gas$CO2[287:296] - SAS.pred)/gas$CO2[287:296]))/10
MAPE.SAS






