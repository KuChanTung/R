# Example 19-2

np = ceiling(0.1*nrow(iris))
np

test.index = sample(1:nrow(iris),np)
iris.test = iris[test.index,]		# 訓練樣本
iris.train = iris[-test.index,]	# 測試樣本
iris.tree = rpart(Species ~  Sepal.Length + Sepal.Width +
 			Petal.Length + Petal.Width, data=iris.train )
 			
# 也可寫成 iris.tree = rpart(Species ~ . , data=iris.train)
iris.tree

summary(iris.tree)
plot(iris.tree) ; text(iris.tree)

species.train = iris$Species[-test.index]
train.pred=factor(predict(iris.tree, iris.train,
	type='class'), levels=levels(species.train))

table.train =table(species.train,train.pred)
table.train
correct.train=sum(diag(table.train))/sum(table.train)

species.test = iris$Species[test.index]
test.pred=factor(predict(iris.tree, iris.test,
		type='class'), levels=levels(species.test)) 
table.test  =table(species.test,test.pred)
table.test
correct.test=sum(diag(table.test))/sum(table.test)

