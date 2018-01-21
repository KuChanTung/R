
#讀取檔案NBA2016-17球季全聯盟30隊各項攻擊均數
NBA1617Analysis <- read.csv("NBA1617TeamPerGameStatsAll.csv",header=T,sep = ",")
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
#找出全聯盟均分統計數據
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

#Logistic線性回歸
#晉級Promotion與場均分PTS
#聯盟東區球隊
Eastern <- read.csv("EasternTeamPerGameStats.csv",header=T,sep = ",")
attach(Eastern)
ETEAMPTS <- Eastern$PTS
Emodellog<- glm(formula = Promotion~ETEAMPTS,data = Eastern,family = binomial(link = "logit"))
summary(Emodellog)
#以場均得分最低預測在聯盟例賽獲勝機率為0.1430553
predict.glm(Emodellog,type="response",newdata = data.frame(ETEAMPTS=101.1))
#以場均得分最高預測在聯盟例賽獲勝機率為0.9673395
predict.glm(Emodellog,type="response",newdata = data.frame(ETEAMPTS=110.3))
Emodellog$coefficients
#場均得分多的0.5628832 可以增加1.755727勝率
exp(0.5628832)
#設定分區及分組為類別變數
Eareaf <- factor(Eastern$Area)
EPromotionf <- factor(Eastern$Promotion)
#驗證分區及分組的變數設定正確
is.factor(Eareaf)
is.factor(EPromotionf)
#進行Logistic迴歸分析
Emodel2 <- glm(formula = Eastern$Promotion~Eareaf+EPromotionf,data=Eastern,family = binomial(link = "logit"))
summary(Emodel2)
epiDisplay::logistic.display(Emodel2)
#分組球隊在分區晉級可能性預測
classtab <- table(Eastern$Area,Eastern$Promotion)
prop.table(classtab,1)


#聯盟西區球隊
Western <- read.csv("WesternTeamPerGameStats.csv",header=T,sep = ",")
attach(Western)
WTEAMPTS <- Western$PTS
Wmodellog<- glm(formula = Promotion~WTEAMPTS,data = Western,family = binomial(link = "logit"))
summary(Wmodellog)
#以場均得分最低預測在聯盟例賽獲勝機率為0.301573
predict.glm(Wmodellog,type="response",newdata = data.frame(WTEAMPTS=97.9))
#以場均得分最高預測在聯盟例賽獲勝機率為0.7797157
predict.glm(Wmodellog,type="response",newdata = data.frame(WTEAMPTS=115.9))
Wmodellog$coefficients
#場均得分多的0.1168794 可以增加1.123984勝率
exp(0.1168794)
#設定分區及分組為類別變數
Wareaf <- factor(Western$Area)
WPromotionf <- factor(Western$Promotion)
#驗證分區及分組的變數設定正確
is.factor(Wareaf)
is.factor(WPromotionf)
#進行Logistic迴歸分析
Wmodel2 <- glm(formula = Western$Promotion~Wareaf+WPromotionf,data=Western,family = binomial(link = "logit"))
summary(model2)
epiDisplay::logistic.display(Wmodel2)
#分組球隊在分區晉級可能性預測
classtab <- table(Western$Area,Western$Promotion)
prop.table(classtab,1)

