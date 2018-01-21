
#讀取檔案NBA2016-17球季全聯盟30隊各項攻防均數
NBA1617Analysis <- read.csv("NBA1617TeamPerGameStats.csv",header=T,sep = ",")
attach(NBA1617Analysis)
#籃球是以單位時間內得分多寡為勝負的比賽，先利用球隊場均得分進行敘述統計演練
NBATEAMPTS <- NBA1617Analysis$PTS
#設定顯示2X2圖形框
par(mfrow=c(2,2))
#顯示各隊均分圖形
hist(NBATEAMPTS)
dotchart(NBATEAMPTS)
boxplot(NBATEAMPTS)
qqnorm(NBATEAMPTS)

#畫出均得分點狀圖
par(mfrow=c(1,1))
library(epicalc)
dotplot(NBATEAMPTS,pch = 16,axes=F)

#敘述統計
#依據聯盟分組計算分組得分
xtabs(PTS~Area,data =NBA1617Analysis)
NBA1617Analysis.xtabs<-xtabs(PTS~Conference+Area,data=NBA1617Analysis)
#橫列邊際總和
margin.table(NBA1617Analysis.xtabs,margin = 1)
#直行邊際總和
margin.table(NBA1617Analysis.xtabs,margin = 2)
#橫列總和
rowSums(xtabs(PTS~Conference+Area,data=NBA1617Analysis))
#各分組依照分區的平均分數
colMeans(xtabs(PTS~Conference+Area,data=NBA1617Analysis))
#直行總和
colSums(xtabs(PTS~Conference+Area,data=NBA1617Analysis))
#計算各分區各組隊伍得分平均數
aggregate(NBATEAMPTS,by=list(NBA1617Analysis$Conference,NBA1617Analysis$Area),FUN=mean)
#計算分區各組得分比例數據
round(prop.table(xtabs(PTS~Conference+Area,data=NBA1617Analysis)),2)
#各橫列的邊際比例
round(prop.table(xtabs(PTS~Conference+Area,data=NBA1617Analysis),margin = 1),2)
#各直行的邊際比例
round(prop.table(xtabs(PTS~Conference+Area,data=NBA1617Analysis),margin =2),2)
#寫一個function找出全聯盟均分統計數據
#x=NBATEAMPTS
my.desc=function(x)
{
  n=length(x)
  x.desc <- c(n,summary(x),var(x),sum(x),sqrt(var(x)),IQR(x),mad(x))
  names(x.desc) <- c("樣本數","最小值","Q1","中位數","平均數","Q3","最大值","變異數","總和","標準差","IQR","MAD")
  return(x.desc)
}
my.desc(NBATEAMPTS)
#截尾平均數(90%)
mean(NBATEAMPTS,trim = 0.1)
#截尾平均數(80%)
mean(NBATEAMPTS,trim = 0.2)
#計算眾數
t=table(NBATEAMPTS)
t[which(table(NBATEAMPTS)==max(table(NBATEAMPTS)))]
#計算偏態係數與峰態係數
library(TSA)
skewness(NBATEAMPTS)
kurtosis(NBATEAMPTS)
#畫出枝葉圖
stem(NBATEAMPTS)


#常態性檢定 P值0.2283 > 0.05 
shapiro.test(NBATEAMPTS)
#判定機率分配---->logistic
library(qAnalyst)
rapidFitFun(NBATEAMPTS)
#查看東區球隊的場均得分屬於何種機率分配--->logistic
rapidFitFun(NBATEAMPTS[NBA1617Analysis$Conference=="Eastern"])
#查看西區球隊的場均得分屬於何種機率分配--->logistic
rapidFitFun(NBATEAMPTS[NBA1617Analysis$Conference=="Western"])

#Logistic線性回歸演練 
#晉級Promotion與場均分PTS
modellog<- glm(formula = Promotion~NBATEAMPTS,data = NBA1617Analysis,family = binomial(link = "logit"))
summary(modellog)
#以場均得分最低預測在聯盟例賽獲勝機率為0.2049738
predict.glm(modellog,type="response",newdata = data.frame(NBATEAMPTS=97.9))
#以場均得分最高預測在聯盟例賽獲勝機率為0.9028666
predict.glm(modellog,type="response",newdata = data.frame(NBATEAMPTS=115.9))
#場均得分多的0.1992 可以增加1.220426勝率
exp(0.1992)

#設定分區及分組為類別變數
areaf <- factor(Area)
Conferencef <- factor(Conference)
#驗證分區及分組的變數設定正確
is.factor(areaf)
is.factor(Conferencef)
#設定以西區太平洋組（3）為準，針對分區與分組進行Logistic迴歸分析
contrasts(areaf) <- contr.treatment(6,base = 3)
model2 <- glm(formula = Promotion~areaf+Conferencef,data=NBA1617Analysis,family = binomial(link = "logit"))
summary(model2)
#分組球隊在不同分區晉級可能性預測
classtab <- table(Area,Promotion)
prop.table(classtab,1)