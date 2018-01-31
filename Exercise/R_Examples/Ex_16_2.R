# Example 16-2: Randomized Block Design

mydata = expand.grid(Block=paste("B",1:6,sep=""),
                       Extrusion=c(8500,8700,8900,9100))
mydata
Y1 = c(90.3,89.2,98.2,93.9,87.4,97.9)
Y2 = c(92.5,89.5,90.6,94.7,87.0,95.8)
Y3 = c(85.5,89.5,85.6,87.4,78.9,90.7)
Y4 = c(82.5,89.5,85.6,87.4,78.9,90.7)
Y = c(Y1,Y2,Y3,Y4)

mydata$Extrusion = as.factor(mydata$Extrusion)
mydata$Y = c(Y1,Y2,Y3,Y4)
mydata

tapply(mydata$Y,list(mydata$Extrusion),mean)		# 平均數
tapply(mydata$Y,list(mydata$Extrusion),sd)		# 標準差
tapply(mydata$Y,list(mydata$Block),mean)
tapply(mydata$Y,list(mydata$Blocks),sd)

devAskNewPage(ask = TRUE)

plot(mydata$Extrusion,mydata$Y,xlab="Extrusion")

devAskNewPage(ask = TRUE)

plot(mydata$Blocks,mydata$Y,xlab="Blocks")

attach(mydata)

devAskNewPage(ask = TRUE)

interaction.plot(Blocks, Extrusion,Y,col=1:4)

devAskNewPage(ask = TRUE)

interaction.plot(Extrusion,Blocks,Y,col=1:6)

lm.result = lm( Y ~ Extrusion + Blocks, data=mydata)
summary(lm.result)

anova(lm.result)

aov(Y~Extrusion + Blocks, data=mydata)

devAskNewPage(ask = TRUE)

plot(Y.pred,residuals)
abline(h=0)

devAskNewPage(ask = TRUE)

plot(as.vector(mydata$Extrusion),
            residuals,xlab="Extrusion")
abline(h=0)
B = rep(1:6,times=4)
B
plot( B, residuals,xlab="Block")
abline(h=0)

devAskNewPage(ask = TRUE)

qqnorm(residuals)
qqline(residuals)

aov.result = aov(Y ~ Extrusion + Blocks, data=mydata)
TukeyHSD(aov.result)

library(asbio)
LSD.test(mydata$Y,mydata$Extrusion)

LSD.test(mydata$Y,mydata$Blocks)



