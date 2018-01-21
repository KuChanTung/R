# R version 3.2.3
# 隨機森林
# Data set:Student Performance Data Set
# Data set URL:https://archive.ics.uci.edu/ml/datasets/Student+Performance
# IDE:RStudio
#==================
#install.packages("randomForest")
library(randomForest)
#setwd("C:/Users/Jialin/Desktop/BI") #把工作目錄設定在要讀取檔案所存放的目錄之下
stud_math=read.csv("student-mat.csv",header=T,sep = ";") #讀取檔案
summary(stud_math)

#第33個屬性G3(也就是最終成績)，把此分數分成A,B,C,D,F五個等級
tmp=0 
for(i in 1:395){
  if(stud_math[i,33]>15)tmp[i]="A"
  else if(stud_math[i,33]>13)tmp[i]="B"
  else if(stud_math[i,33]>11)tmp[i]="C"
  else if(stud_math[i,33]>9)tmp[i]="D"
  else tmp[i]="F"
}
stud_math[,33]=factor(tmp) #把分好等級之結果存進stud_math
summary(stud_math$G3) #印出G3屬性中各等級的資料筆數

num_train_data=ceiling(0.9*nrow(stud_math)) #取90%為訓練資料
num_train_data #印出訓練資料之筆數

stud_math.train=sample(1:nrow(stud_math),num_train_data) #隨機選取
print(stud_math.train) #印出選到第幾筆為訓練資料

stud_math.rf=randomForest(G3~.,data=stud_math,importance=TRUE,proximity=TRUE,ntree=500,subset=stud_math.train, na.action = na.fail)
#第一個參數(G3~.):表示除了G3屬性之外，其他屬性皆為模型之引數(因為我們要預測G3呀~)
#第二個參數(data=stud_math):表示模型中含有變數的一組資料
#第三個參數(importance=TRUE):是否計算每個模型中各屬性之重要值，資料型態為布林
#第四個參數(proximity=TRUE):是否計算模型的鄰近矩陣，此參數搭配函數MDSplot()使用，資料型態為布林
#第五個參數(ntree=500):表示森林中的樹木數量
#第六個參數(subset=stud_math.train):表示選出的訓練集資料為第幾筆(此參數的資料格式為向量)
#第七個參數(na.action = na.fail):表示遺漏值之處理，na.fail表示不能出現遺漏值

print(stud_math.rf)


n=ncol(stud_math)-1 #取33-1=32個屬性，即扣掉要預測的G3
model_err_rate=1
for(i in 1:n)
{
  result=randomForest(G3~.,data=stud_math,mtry=i,importance=TRUE,ntree=500)
  model_err_rate[i]=mean(result$err.rate)
  cat("第",i,"個模型:",model_err_rate[i],"\n")
}
mtry_value=which.min(model_err_rate)
cat("mtry參數設定為:",mtry_value)

stud_math.rf=randomForest(G3~.,data=stud_math,mtry=mtry_value,importance=TRUE,proximity=TRUE,ntree=500)

print(stud_math.rf)
importance(stud_math.rf)

MDSplot(stud_math.rf,stud_math$G3,palette=rep(1,5), pch=as.numeric(stud_math$G3))
round(importance(stud_math.rf),2)