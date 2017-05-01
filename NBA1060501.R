
#讀取檔案NBA2016-17球季全聯盟30隊各項攻防均數
NBA1617Analysis = read.csv("NBA1617TeamPerGameStats.csv",header=T)
attach(NBA1617Analysis)

#設定顯示2X2圖形框
par(mfrow=c(2,2))
#顯示各隊均分圖形
hist(NBA1617Analysis$PTS)
dotchart(NBA1617Analysis$PTS)
boxplot(NBA1617Analysis$PTS)
qqnorm(NBA1617Analysis$PTS)

#畫出均得分點狀圖
par(mfrow=c(1,1))
library(epicalc)
dotplot(NBA1617Analysis$PTS,pch = 16,axes=F)

#敘述統計
#依據聯盟分組計算每組得分
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
aggregate(NBA1617Analysis$PTS,by=list(NBA1617Analysis$Conference,NBA1617Analysis$Area),FUN=mean)
#計算分區各組得分比例數據
round(prop.table(xtabs(PTS~Conference+Area,data=NBA1617Analysis)),2)
#各橫列的邊際比例
round(prop.table(xtabs(PTS~Conference+Area,data=NBA1617Analysis),margin = 1),2)
#各直行的邊際比例
round(prop.table(xtabs(PTS~Conference+Area,data=NBA1617Analysis),margin =2),2)
#寫一個function找出全聯盟均分統計數據
x=NBA1617Analysis$PTS
my.desc=function(x)
{
  n=length(x)
  x.desc <- c(n,summary(x),var(x),sum(x),sqrt(var(x)),IQR(x),mad(x))
  names(x.desc) <- c("樣本數","最小值","Q1","中位數","平均數","Q3","最大值","變異數","總和","標準差","IQR","MAD")
  return(x.desc)
}
my.desc(x)
#截尾平均數(90%)
mean(x,trim = 0.1)
#截尾平均數(80%)
mean(x,trim = 0.2)
#計算眾數
t=table(x)
t[which(table(x)==max(table(x)))]
#計算偏態係數與峰態係數
library(TSA)
skewness(x)
kurtosis(x)
#畫出枝葉圖
stem(x)


#常態性檢定 
shapiro.test(NBA1617Analysis$PTS)
#判定機率分配---->logistic
library(qAnalyst)
rapidFitFun(NBA1617Analysis$PTS)
#查看東區球隊的場均得分屬於何種機率分配--->logistic
rapidFitFun(NBA1617Analysis$PTS[NBA1617Analysis$Conference=="Eastern"])
#查看西區球隊的場均得分屬於何種機率分配--->logistic
rapidFitFun(NBA1617Analysis$PTS[NBA1617Analysis$Conference=="Western"])