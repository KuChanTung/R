########################################################################
#練習ISLR                            106.10.10                        ＃
########################################################################
x=seq(-pi ,pi ,length =50)
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels = 45,add = T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels = 15)
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta = 30)
persp(x,y,fa,theta = 30,phi = 20)
persp(x,y,fa,theta = 30,phi = 70)
persp(x,y,fa,theta = 30,phi=20)
#Matrix
A <- matrix(1:16,4,4)
A[2,3]
A[c(1,3) ,c(2,4) ]
A[1:3 ,2:4]
A[1:2 ,]
A[ ,1:2]
#Load Data
library("ISLR")
data("Auto")
dim(Auto)
Auto=na.omit(Auto)
dim(Auto)
names(Auto)
plot(cylinders , mpg)
plot(Auto$cylinders , Auto$mpg)
attach(Auto)
plot(cylinders , mpg)
cylinders =as.factor (cylinders )
plot(cylinders,mpg)
plot(cylinders , mpg , col ="red ")
plot(cylinders , mpg , col ="red", varwidth =T)
plot(cylinders , mpg , col ="red", varwidth =T,horizontal =T)
plot(cylinders , mpg , col ="red", varwidth =T, xlab=" cylinders ",ylab ="MPG ")

hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks = 15)

pairs(Auto)
pairs(~mpg + displacement + horsepower + weight +acceleration , Auto)

plot(horsepower,mpg)
identify(horsepower,mpg,name)

summary(Auto)
