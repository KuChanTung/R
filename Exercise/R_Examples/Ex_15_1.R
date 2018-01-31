# Example 15-1 : Simple Regression Model

# 請修改資料檔路徑
dogs = read.table("c:/r/dogs.txt",header=T)
dogs

devAskNewPage(ask = TRUE)

plot(dogs$computerR,dogs$adopted)

x = dogs$computerR
Y = dogs$adoptedR
beta1 = sum((x-mean(x))*(Y-mean(Y)))/sum((x-mean(x))^2)
beta1

beta0 = mean(Y)-beta1*mean(x)
beta0

x = dogs$computerR ; Y = dogs$adoptedR
( X = cbind(intercept = 1,computerR = x ) )

betaVector = solve(t(X) %*% X) %*% t(X) %*% Y
betaVector

slm.model = lm(adoptedR ~ computerR,data=dogs, x=TRUE)
summary(slm.model)

( SST = sum((dogs$adoptedR - mean(dogs$adoptedR))^2)  )
( SSR = 0.1963*SST )		# R2 = 0.1963
( SSE = SST - SSR )

( SSE = deviance(slm.model) )
( SSR = SST - SSE )

qt(0.975,21)		# 95% 信賴區間 t 分配查表值

# beta0 的95% 信賴區間：
c(-2.8601 - 2.079614*9.4447, -2.8601 + 2.079614*9.4447)

# beta1 的 95% 信賴區間：
c(0.2576 -2.079614*0.1137,0.2576 +2.079614*0.1137)

anova(slm.model)

names(slm.model)

slm.model$residuals			# 或用 residuals(slm.model)
slm.model$fitted.values		# 或用 predict(slm.model)

slm.model$coef
coef(slm.model)[2]

deviance(slm.model)

slm.model$x

devAskNewPage(ask = TRUE)

plot(slm.model)
resid = slm.model$residuals


shapiro.test(resid)

library(car)
ncv.test(slm.model)

durbin.watson(slm.model)



