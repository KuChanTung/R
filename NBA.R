#####################################
##決策樹、隨機森林專題報告        ###
#####################################
library(rpart)
#載入鳶尾花資料集，總計150筆
#data("iris")
NBA <- read.csv("All8083.csv",header=T,sep = ",")

NBA
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
plot(NBA.tree);text(NBA.tree)
#訓練樣本的混淆矩陣與辨識正確率97%
Stats.train <-NBA$Stats[-test.index]
train.pred <-factor(predict(NBA.tree,NBA.train,type = 'class'),level=levels(Stats.train))
table.train <-table(Stats.train,train.pred)
table.train
correct.train <-sum(diag(table.train)/sum(table.train))
correct.train
#測試樣本的混淆矩陣與辨識正確率80%
Stats.test <-NBA$Stats[test.index]
test.pred <-factor(predict(NBA.tree,NBA.test,type = 'class'),levels = levels(Stats.test))
table.test <-table(Stats.test,test.pred)
table.test
correct.test <-sum(diag(table.test)/sum(table.test))
correct.test

#CHAID決策樹
library(CHAID)
#依據花萼長度寬度分割資料集各為10等分
SepL=cut(iris$Sepal.Length,breaks = 10)
SepW=cut(iris$Sepal.Width,breaks = 10)
PetL=cut(iris$Petal.Length,breaks = 10)
PetW=cut(iris$Petal.Width,breaks = 10)
SepL=ordered(SepL)
SepW=ordered(SepW)
PetL=ordered(PetL)
PetW=ordered(PetW)
iris2=data.frame(SepL,SepW,PetL,PetW,Species=iris$Species)
iris2
#建構CHAID決策樹模型
iris.chaid=chaid(Species~.,data=iris2)
#畫出CHAID決策樹狀圖
plot(iris.chaid)
print(iris.chaid)
#計算混淆矩陣與預測正確率96%
pred <-predict(iris.chaid,newdata = iris2)
pred
table.chaid=table(Species=iris2$Species,prediction=pred)
table.chaid
sum(diag(table.chaid)/sum(table.chaid))

#隨機森林
library(randomForest)
set.seed(71)
#使用訓練樣本進行建模與評估辨識正確率97.04%
NBA.rf <-randomForest(Stats~.,data=NBA.train,importance=TRUE,proximity=TRUE)
print(NBA.rf)
#判斷變數重要性
round(importance(NBA.rf),2)
#擷取混淆矩陣取得更精確平均辨識率95.46%
names(NBA.rf)
sum(diag(NBA.rf$confusion)/sum(NBA.rf$confusion))
#測試樣本混淆矩陣與平均預測正確率86%
rf.pred=predict(NBA.rf,newdata = NBA.test)
rf.pred
table.test <-table(Stats=Stats.test,Predicated=rf.pred)
table.test
sum(diag(table.test)/sum(table.test))

#利用隨機森林分群
set.seed(17)
NBA.urf=randomForest(NBA[,-10])
NBA.urf
MDSplot(NBA.urf,NBA$Stats,palette = rep(1,3),pch = as.numeric(NBA$Stats))
