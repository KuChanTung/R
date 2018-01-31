# Example 16-5: 2^(k-p) Design

data(speedo,package="faraway")
speedo
speedo2 = with(speedo, {data.frame(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,y)} )
speedo2

speedo.lm = lm(y ~. ,data=speedo2)
summary(speedo.lm)

model.matrix(speedo.lm)

coeff = speedo.lm$coef[-1]
coeff

model.tables(speedo.lm,type="means")

(  speedo.effects = effects(speedo.lm)  )

speedo.effects = speedo.effects[-1]			# ¥h°£ºI¶Z¶µ
effects2 = sort(abs(speedo.effects))
s0 = 1.5*median(effects2)
PSE = 1.5* median(effects2[effects2 < 2.5*s0])
ME = 2.14*PSE
SME = 4.163*PSE
ME
SME
effects2[effects2 > ME]
effects2[effects2 > SME]

effects.sort = sort(speedo.effects)

devAskNewPage(ask = TRUE)

plot(qnorm(1:15/16),effects.sort,type="n",main="Normal")
text(qnorm(1:15/16),effects.sort,
                   sub("-","",names(effects.sort)))
                   
devAskNewPage(ask = TRUE)
                   
plot(qnorm(16:30/31),ylim=c(0,SME*1.2),effects2,
                   type="n",main="Half-Normal")
text(qnorm(16:30/31),effects2,sub("-","",names(effects2)))
abline(h=ME,lty=2)						
abline(h=SME,lty=3)
text(0.5,ME*1.1,"ME")
text(0.5,SME*1.1,"SME")

