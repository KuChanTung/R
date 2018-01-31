# Example 13-4

# 請修改資料檔路徑
brain = read.csv("c:/r/brain.csv",header=T)
brain

x = brain$VIQ  
fivenum(x)
summary(x)

my.desc = function(x)
{
    n = length(x)
    x.desc = c(n,summary(x),var(x),sum(x),sqrt(var(x)),
           IQR(x),mad(x))

    names(x.desc) = c("樣本數","最小值","Q1","中位數","平均數",
        "Q3","最大值", "變異數","總和","標準差","IQR", "MAD")
    return(x.desc)
}

my.desc(x) 
mean(x,trim=0.1)					# 90% trimmed mean
mean(x,trim=0.2)					# 80% trimmed mean

(  t = table(x)  )
t[which(table(x) == max(table(x)))]

Gender = factor(c("女","男")[brain$Gender + 1])  # 轉碼成 男,女
Gender

plot(Gender,brain$VIQ)

library(TSA)
skewness(x)
kurtosis(x)

devAskNewPage(ask = TRUE)

library(epicalc)
dotplot(brain$VIQ,pch=16,axes=F)
 
stem(x) 

devAskNewPage(ask = TRUE)

boxplot(x,horizontal=T) 

devAskNewPage(ask = TRUE)

hist(x) 

devAskNewPage(ask = TRUE)

qqnorm(x) ; qqline(x)


shapiro.test(brain$VIQ)

library(qAnalyst)
rapidFitFun(brain$VIQ)
rapidFitFun(brain$VIQ[brain$Gender==1])

