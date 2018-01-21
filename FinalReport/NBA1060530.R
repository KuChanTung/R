#######################################################
##巨量資料分析 期末報告 題目：預測2017NBA年度總冠軍 ###
#######################################################
library(rpart)
library(party)
library(rattle)
#載入NBA資料集，資料期間1979年~2017年，總計1,044筆
NBA <- read.csv("NBA19802017.csv",header=T,sep = ",")
#顯示資料集
View(NBA)

#挑選出需要欄位--選擇要投入的屬性
#NBA.CART <-subset(NBA, select = c(TeamNme,Age,Wins,Losses,MOV,SOS,SRS,ORTG,DRTG,Pace,FTR,PAR,TS,OFFRFG,OFFTOV,OFFORB,OFFFTFGA,DFFEFG,DFFTOV,DFFDRB,DFFFTFGA,Final))
NBA.CART <-subset(NBA, select = c(Age,Wins,Losses,MOV,SOS,SRS,ORTG,DRTG,Pace,FTR,PAR,TS,OFFRFG,OFFTOV,OFFORB,OFFFTFGA,DFFEFG,DFFTOV,DFFDRB,DFFFTFGA,Final))

#隨機取其中10%為測試資料
np <-ceiling(0.1*nrow(NBA.CART))
#顯示測試資料
#View(np)
test.index<-sample(1:nrow(NBA.CART),np)
#測試樣本
NBA.test <-NBA.CART[test.index,]
#訓練樣本
NBA.train <-NBA.CART[-test.index,]
#-----------------------------------------------------------------

#CART決策樹
#訓練樣本投入分類
NBA.tree <- rpart(Final~.,data=NBA.train)
NBA.tree
#畫出分類圖
summary(NBA.tree)
fancyRpartPlot(NBA.tree)
#plot(NBA.tree);text(NBA.tree,all=T)

#訓練樣本的混淆矩陣與辨識正確率
Stats.train <-NBA$Final [-test.index]
train.pred <-factor(predict(NBA.tree,NBA.train,type = 'class'),level=levels(Stats.train))
table.train <-table(Stats.train,train.pred)
table.train
correct.train <-sum(diag(table.train)/sum(table.train))
correct.train

#測試樣本的混淆矩陣與辨識正確率
Stats.test <-NBA$Final[test.index]
test.pred <-factor(predict(NBA.tree,NBA.test,type = 'class'),levels = levels(Stats.test))
table.test <-table(Stats.test,test.pred)
table.test
correct.test <-sum(diag(table.test)/sum(table.test))
correct.test

#----------------------------------------------------------------

#條件推論樹
ct <- ctree(Final ~ ., data = NBA)
plot(ct, main = "Conditional Inference Tree")
#正確率97.041%
ct.pre <- table(NBA$Final, predict(ct))
ct.table <- sum(diag(ct.pre)/sum(ct.pre))
ct.table
#------------------------------------------------------------------

#隨機森林
library(randomForest)

#挑選出需要欄位--選擇要投入的屬性
#NBA.Record <-subset(NBA, select = c(Age,Wins,Losses,MOV,SOS,SRS,ORTG,DRTG,Pace,FTR,PAR,TS,OFFRFG,OFFTOV,OFFORB,OFFFTFGA,DFFEFG,DFFTOV,DFFDRB,DFFFTFGA,Stats,Final))
NBA.Record <-subset(NBA, select = c(Age,Wins,Losses,MOV,SOS,SRS,ORTG,DRTG,Pace,FTR,PAR,TS,OFFRFG,OFFTOV,OFFORB,OFFFTFGA,DFFEFG,DFFTOV,DFFDRB,DFFFTFGA,Final))
#提取重要變數
NBA.Record <-subset(NBA, select = c(Wins,MOV,Final))


#指定1979至2016年的資料為訓練資料
NBA.rf.train <- NBA.Record[1:1014,]
#指定2017年的資料為測試資料
NBA.rf.test <- NBA.Record[1015:1044,]
#設定隨機種子
set.seed(100)

#使用訓練樣本進行建模與評估辨識正確率
NBA.rf <-randomForest(Final~.,data=NBA.rf.train,importance=TRUE,proximity=TRUE,do.trace = 100)
NBA.rf <-randomForest(Final~.,data=NBA.rf.train,importance=TRUE,proximity=TRUE,do.trace = 100,ntree=1000)

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
legend('topright', colnames(NBA.rf$err.rate), col=1:3, fill=1:3)
print(NBA.rf.Opt)
#重新檢視重要變數並畫圖
varImpPlot(NBA.rf.Opt)
round(importance(NBA.rf.Opt),2)
#畫出群集分析圖
MDSplot(NBA.rf.Opt,NBA$Final,palette=rep(1,5), pch=as.numeric(NBA$Final))

#預測2017年度總冠軍
#NBA.LC.prediction <- predict(NBA.rf.Opt, NBA.rf.test)
solution <- data.frame(OrininalPrediction = NBA.rf.test$Final, RF.Prediction = NBA.LC.prediction)
solution <- data.frame(RF.Prediction = NBA.LC.prediction)

write.csv(solution, file = 'NBA2017LC.csv', row.names = F)
