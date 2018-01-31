# Example 19-4 : RandomForest for iris data

library(randomForest)
set.seed(71)
iris.rf <- randomForest(Species ~ . , data=iris.train , 
			importance=TRUE, proximity=TRUE)
print(iris.rf)

round(importance(iris.rf), 2)

names(iris.rf)
(  table.rf = iris.rf$confusion  )

sum(diag(table.rf))/sum(table.rf)

rf.pred = predict(iris.rf, newdata=iris.test)
rf.pred

table.test=table(Species=species.test, Predicted = rf.pred)
table.test
sum(diag(table.test))/sum(table.test)	

set.seed(17)
iris.urf <- randomForest(iris[, -5])
iris.urf

MDSplot(iris.urf, iris$Species, palette=rep(1, 3), 
			pch=as.numeric(iris$Species))
			
			




