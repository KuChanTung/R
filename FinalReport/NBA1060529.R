#####################################
##巨量資料分析 期末報告           ###
#####################################
library(rpart)
library(party)
library(rattle)
#載入NBA資料集，資料期間1979年~2016年，總計1,014筆
NBA <- read.csv("NBA19792016NEW.csv",header=T,sep = ",")
#顯示資料集
View(NBA)
#隨機取其中10%為測試資料
np <-ceiling(0.1*nrow(NBA))
#顯示測試資料
View(np)
test.index<-sample(1:nrow(NBA),np)
#測試樣本
NBA.test <-NBA[test.index,]
#訓練樣本
NBA.train <-NBA[-test.index,]

#-----------------------------------------------------------------

#CART決策樹
#訓練樣本投入分類
NBA.tree <- rpart(Final~.,data=NBA.train)
NBA.tree
#畫出分類圖
summary(NBA.tree)
fancyRpartPlot(NBA.tree)
#plot(NBA.tree);text(NBA.tree,all=T)

#訓練樣本的混淆矩陣與辨識正確率97.478%
Stats.train <-NBA$Final [-test.index]
train.pred <-factor(predict(NBA.tree,NBA.train,type = 'class'),level=levels(Stats.train))
table.train <-table(Stats.train,train.pred)
table.train
correct.train <-sum(diag(table.train)/sum(table.train))
correct.train

#測試樣本的混淆矩陣與辨識正確率97.058%
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
set.seed(70)

#使用訓練樣本進行建模與評估辨識正確率95.94%
NBA.rf <-randomForest(Final~.,data=NBA.train,importance=TRUE,proximity=TRUE,do.trace = 100)
plot(NBA.rf)
print(NBA.rf)

#判斷變數重要性並畫圖
varImpPlot(NBA.rf)
round(importance(NBA.rf),2)
#擷取混淆矩陣取得更精確平均辨識率95.85%
names(NBA.rf)
sum(diag(NBA.rf$confusion)/sum(NBA.rf$confusion))

#測試樣本混淆矩陣與平均預測正確率98.039%
rf.pred=predict(NBA.rf,newdata = NBA.test)
rf.pred
table.test <-table(Stats=Stats.test,Predicated=rf.pred)
table.test
sum(diag(table.test)/sum(table.test))

#找出最佳mtry參數
n=ncol(NBA)-1 #取24-1=23個屬性，即扣掉要預測的狀態
model_err_rate=1
for(i in 1:n)
{
  result=randomForest(Final~.,data=NBA,mtry=i,importance=TRUE,ntree=500,do.trace = 100)
  model_err_rate[i]=mean(result$err.rate)
  cat("Model",i,":",model_err_rate[i],"\n")
}

#找出錯誤率最小模型
mtry_value=which.min(model_err_rate)
cat("mtry set:",mtry_value)

#重新設定mtry參數，並使用訓練樣本重新建模與評估辨識正確率96.27%
NBA.rf.Opt <-randomForest(Final~.,data=NBA.train,mtry=mtry_value,importance=TRUE,proximity=TRUE,ntree=500,do.trace = 100)
print(NBA.rf.Opt)
#畫圖判別ntree參數設定
plot(NBA.rf.Opt)
#重新檢視重要變數並畫圖
varImpPlot(NBA.rf.Opt)
round(importance(NBA.rf.Opt),2)
#畫出群集分析圖
MDSplot(NBA.rf.Opt,NBA$Final,palette=rep(1,5), pch=as.numeric(NBA$Final))

#利用隨機森林分群
set.seed(100)
#剔除欄位23，並進行分類
NBA.urf=randomForest(NBA[,-24])
NBA.urf
#畫出分類群集分析圖
MDSplot(NBA.urf,NBA$Final,palette = rep(1,3),pch = as.numeric(NBA$Final))


#
GSW<- read.csv("GSW.csv",header=T,sep = ",")
attach(GSW)
predict (NBA.rf.Opt,newdata = data.frame(Age=28.2),type="response")
