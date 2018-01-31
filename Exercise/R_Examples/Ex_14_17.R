# Example 14-17: Chi-Square Goodness of Fit Test for iris data and Normal

data(iris)
SepL = iris$Sepal.Length				# 簡化變數名稱
mean(SepL) ; sd(SepL)
length(SepL)

normal = rnorm(mean(SepL), sd(SepL))
normal.h = hist(normal,breaks=10,plot=F)
normal.h

SepL.h = hist(SepL,breaks=normal.h$breaks,plot=F)
SepL.h

SepL.count = SepL.h$counts

normal.prob = normal.h$counts/150
normal.prob

chisq.test(SepL.count, p = normal.prob)

