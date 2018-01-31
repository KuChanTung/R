# Example 16-3: Latin Square

library(crossdes)
MOLS(5,1)
m = MOLS(5,1)[,,1]
rownames(m) = paste("a",1:5,sep="")
colnames(m) = paste("b",1:5,sep="")
m

Treatment = toupper(letters[as.vector(m)])
Treatment

mydata= expand.grid(Block2 = c("b1","b2","b3","b4","b5"),
                           Block1 = c("a1","a2","a3","a4","a5"))
mydata

mydata$Treatment = as.factor(Treatment)
mydata

devAskNewPage(ask = TRUE)

oldpar = par()
par(mfrow=c(2,2))
plot(mydata$Treatment,mydata$Y,xlab="Treatment")
plot(mydata$Block1,mydata$Y,xlab="Block1")
plot(mydata$Block2,mydata$Y,xlab="Block2")
par(oldpar)

with(mydata,{
  devAskNewPage(ask = TRUE)
  interaction.plot(Treatment,Block1,Y,legend=F,
                           main="Block1 vs. Treatment")
                           
  devAskNewPage(ask = TRUE)                           
  interaction.plot(Treatment,Block2,Y,legend=F,
                           main="Block2 vs. Treatment")
                           
  devAskNewPage(ask = TRUE)                           
  interaction.plot(Block2,Block1,Y,legend=F,
                           main="Block1 vs. Block2")
 })

lm.result = lm(Y ~ Block1 + Block2 + Treatment,data=mydata)
lm.result

anova(lm.result)

aov.result=aov(Y~Block1+Block2+Treatment,data=mydata)
aov.result



