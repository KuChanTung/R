# Example 15-5: Ordinal Logistic Regression

# 叫э polr.compare.R 隔畖
source("c:/r/polr_compare.R")		# 更 polr.compres ㄧ计

data(Arthritis,package="vcd")
head(Arthritis)

library(MASS)
result0.polr = polr(Improved ~ Treatment + Sex + Age,
   data=Arthritis, Hess=T, method="logistic")

stepAIC(result0.polr)


result.polr = polr(Improved ~ Treatment + Sex + Age,
              data=Arthritis, Hess=T, method="logistic")
              
result.polr
summary(result.polr)

names(result.polr)

result.polr$zeta

result.polr$coef	

model.matrix(result.polr)

library(epicalc)
ordinal.or.display(result.polr)

Ypred = predict(result.polr,newdata=Arthritis)
Ypred
tab = table(Real = Arthritis$Improved,Prediction = Ypred ) 
tab
cat("タ絋箇代ゑㄒ = ",100*sum(diag(tab))/sum(tab)," % \n")

# source("c:/r/polr_compare.R")		# 更 polr.compres ㄧ计
x1 = list(Treatment="Placebo",Sex="Male",Age=35)
x2 = list(Treatment="Treated",Sex="Female",Age=48)

polr.compare(X1=x1, Y1="None",X2=x2,Y2="Some",
                    data=Arthritis,model=result.polr)

