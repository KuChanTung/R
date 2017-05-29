#####################################
##巨量資料分析 期末報告           ###
#####################################
library(rpart)
#載入NBA資料集，資料期間1979年~2016年，總計1,014筆
NBA <- read.csv("NBA19792016.csv",header=T,sep = ",")
View(NBA)
dim(NBA)
#CART決策樹
#隨機取其中10%為測試資料
np <-ceiling(0.1*nrow(NBA))
np
test.index<-sample(1:nrow(NBA),np)
#測試樣本
NBA.test <-NBA[test.index,]
#訓練樣本
NBA.train <-NBA[-test.index,]
#訓練樣本投入分類
NBA.tree <- rpart(Stats~.,data=NBA.train)
NBA.tree
#畫出分類圖
summary(NBA.tree)
plot(NBA.tree);text(NBA.tree,all=T)
#訓練樣本的混淆矩陣與辨識正確率90.46%
Stats.train <-NBA$Stats[-test.index]
train.pred <-factor(predict(NBA.tree,NBA.train,type = 'class'),level=levels(Stats.train))
table.train <-table(Stats.train,train.pred)
table.train
correct.train <-sum(diag(table.train)/sum(table.train))
correct.train
#測試樣本的混淆矩陣與辨識正確率88.235%
Stats.test <-NBA$Stats[test.index]
test.pred <-factor(predict(NBA.tree,NBA.test,type = 'class'),levels = levels(Stats.test))
table.test <-table(Stats.test,test.pred)
table.test
correct.test <-sum(diag(table.test)/sum(table.test))
correct.test


#隨機森林
library(randomForest)
set.seed(70)
#使用訓練樣本進行建模與評估辨識正確率88.6%
NBA.rf <-randomForest(Stats~.,data=NBA.train,importance=TRUE,proximity=TRUE)
print(NBA.rf)
#判斷變數重要性
round(importance(NBA.rf),2)
#擷取混淆矩陣取得更精確平均辨識率88.492%
names(NBA.rf)
sum(diag(NBA.rf$confusion)/sum(NBA.rf$confusion))
#測試樣本混淆矩陣與平均預測正確率91.176%
rf.pred=predict(NBA.rf,newdata = NBA.test)
rf.pred
table.test <-table(Stats=Stats.test,Predicated=rf.pred)
table.test
sum(diag(table.test)/sum(table.test))

#利用隨機森林分群
set.seed(17)
NBA.urf=randomForest(NBA[,-23])
NBA.urf
MDSplot(NBA.urf,NBA$Stats,palette = rep(1,3),pch = as.numeric(NBA$Stats))
