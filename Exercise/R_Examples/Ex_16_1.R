# Example 16-1: One-way ANOVA

Y1 = c(575,542,530,539,570)
Y2 = c(565,593,590,579,610)
Y3 = c(600,651,610,637,629)
Y4 = c(725,700,715,685,710)
Y = c(Y1,Y2,Y3,Y4) 
Y

A = as.factor(rep(c("160w","180w","200w","220w"),each=5))
A

lm(Y~A)
summary(lm(Y~A))

anova(lm( Y~A ))

aov(Y~A)
summary(aov(Y~A))

lm.result = lm(Y~A)
coef(lm.result)
aov.result = aov(Y~A)

model.tables(aov.result,type="effects)
model.tables(aov.result,type="means")

tapply(Y,list(A),mean)
tapply(Y,list(A),sd)

model.matrix(Y~A)

oldpar = par()
par(mfrow=c(2,2))

residuals = lm.result$res
Y.fit = lm.result$fit
plot(residuals)
abline(h=0)
plot(Y.fit,residuals)
abline(h=0)
qqnorm(residuals)
qqline(residuals)

par(oldpar)

shapiro.test(residuals)

library(lmtest)
dwtest(Y~A)

library(car)
durbin.watson(lm.result,method="normal")

bartlett.test(residuals, A)

        Bartlett test of homogeneity of variances

data:  residuals and A 
Bartlett's K-squared = 0.4335, df = 3, p-value = 0.9332

ncv.test(lm.result)   # ©Î ncv.test(lm(Y~A))

outlier.test(lm.result)     # ©Î outlier.test(lm(Y~A))

oneway.test( Y ~ A, var.equal=FALSE )
kruskal.test(Y~A)

TukeyHSD(aov.model)

devAskNewPage(ask = TRUE)

plot(TukeyHSD(aov.model))	

library(multcomp)
glht(aov.model, linfct = mcp(A = "Tukey"))

glht(aov.model, linfct = mcp(A = "Dunnett"))

tuk = glht(aov.model, linfct = mcp(A = "Tukey"))
tukcld = cld(tuk)

devAskNewPage(ask = TRUE)

plot(tuk)	

library(multcompView)
mydata = data.frame(Y,A)
multcompBoxplot(Y~A,data=mydata)

library(laercio)
aov.result = aov(Y ~ A)
LDuncan(aov.result, "A")
LTukey(aov.result, "A")

library(asbio)
LSD.test(Y,A)
BonferroniCI(Y,A)
TukeyCI(Y,A)
ScheffeCI(Y,A)
Pairw.test(Y,A,method="Scheffe")



