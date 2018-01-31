# Example 15-4: Polytomous Logistic Example

iris
p = 0.9
inddex = sample(2, nrow(iris), replace = TRUE, 
                 prob=c(p,1-p))
iris.train = iris[index == 1,]
iris.test = iris[index == 2,]

library(nnet)
train.result = multinom(Species ~ Sepal.Length+ Sepal.Width+ Petal.Length + 
                Petal.Width, data = iris.train, Hess = TRUE )

train.result

library(epicalc)
mlogit.display(result1)

s1 = summary(train.result)
coef(s1)
exp(coef(s1))
coef(s1)/s1$standard.errors
p.values = pnorm(abs(coef(s1)/s1$standard.errors), lower.tail=FALSE)*2 
p.values

Y.pred = predict(train.result,iris.train[,-5] )
(t = table(iris.train$Species,Y.pred))
cat("正確預測比例：",100*sum(diag(t))/nrow(iris.train), " % \n")


Y.pred = predict(train.result,iris.test[,-5])
(t = table(iris.test$Species,Y.pred))

cat('正確預測比例：",100*sum(diag(t))/nrow(iris.test)," % \n")





