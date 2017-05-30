#######################################################
##巨量資料分析 期末報告 題目：預測2017NBA年度總冠軍 ###
#######################################################

#隨機森林
library(randomForest)
#載入NBA資料集，資料期間1979年~2017年，總計1,044筆
NBA <- read.csv("NBA19802017.csv",header=T,sep = ",")
#挑選出需要欄位--選擇要投入的屬性
#NBA.Record <-subset(NBA, select = c(Age,Wins,Losses,MOV,SOS,SRS,ORTG,DRTG,Pace,FTR,PAR,TS,OFFRFG,OFFTOV,OFFORB,OFFFTFGA,DFFEFG,DFFTOV,DFFDRB,DFFFTFGA,Stats,Final))
NBA.Record <-subset(NBA, select = c(Age,Wins,Losses,MOV,SOS,SRS,ORTG,DRTG,Pace,FTR,PAR,TS,OFFRFG,OFFTOV,OFFORB,OFFFTFGA,DFFEFG,DFFTOV,DFFDRB,DFFFTFGA,Final))
#指定1979至2016年的資料為訓練資料
NBA.rf.train <- NBA.Record[1:1014,]
#指定2017年的資料為測試資料
NBA.rf.test <- NBA.Record[1015:1044,]
#設定隨機種子
set.seed(100)

#使用訓練樣本進行建模與評估辨識正確率
NBA.rf <-randomForest(Final~.,data=NBA.rf.train,importance=TRUE,proximity=TRUE,do.trace = 100)
plot(NBA.rf)
legend('topright', colnames(NBA.rf$err.rate), col=1:3, fill=1:3)
print(NBA.rf)

#判斷變數重要性並畫圖
varImpPlot(NBA.rf)
round(importance(NBA.rf),2)

#擷取混淆矩陣取得更精確平均辨識率
names(NBA.rf)
sum(diag(NBA.rf$confusion)/sum(NBA.rf$confusion))

#找出最佳的mtry參數
n=ncol(NBA.Record)-1#扣掉要預測的屬性
model_err_rate=1
for(i in 1:n)
{
  result=randomForest(Final~.,data=NBA.Record,mtry=i,importance=TRUE,ntree=500,do.trace = 100)
  model_err_rate[i]=mean(result$err.rate)
  cat("Model",i,":",model_err_rate[i],"\n")
}

#找出錯誤率最小模型
mtry_value=which.min(model_err_rate)
cat("mtry set:",mtry_value)

#重新設定mtry參數，並使用訓練樣本重新建模與評估辨識正確率
NBA.rf.Opt <-randomForest(Final~.,data=NBA.rf.train,mtry=mtry_value,importance=TRUE,proximity=TRUE,ntree=500,do.trace = 100)
print(NBA.rf.Opt)
#畫圖判別ntree參數設定
plot(NBA.rf.Opt)
#重新檢視重要變數並畫圖
varImpPlot(NBA.rf.Opt)
round(importance(NBA.rf.Opt),2)
#畫出群集分析圖
MDSplot(NBA.rf.Opt,NBA$Final,palette=rep(1,5), pch=as.numeric(NBA$Final))

#預測2017年度總冠軍
NBA.LC.prediction <- predict(NBA.rf.Opt, NBA.rf.test)
solution <- data.frame(OrininalPrediction = NBA.rf.test$Final, RF.Prediction = NBA.LC.prediction)
write.csv(solution, file = 'NBA2017LC.csv', row.names = F)
