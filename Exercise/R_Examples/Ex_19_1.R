# Example 19-1

# 請修改資料檔路徑
babies = read.csv("c:/r/babies.csv", header=T)
head(babies, 3)

babies = na.exclude(babies)			# 去除遺失值
np = ceiling(0.1*nrow(babies))
np		

test.index = sample(1:nrow(babies),np)
babies.test = babies[test.index,]		# 測試樣本
babies.train = babies[-test.index,]	

library(tree)
babies.tree = tree(bwt ~  gestation + parity + age + height
				+ weight + smoke, data=babies.train )

# 或寫成 babies.tree = tree( bwt ~ . , data=babies.train)

plot(babies.tree);  text(babies.tree)

bwt.train = babies$bwt[-test.index]
train.pred = predict(babies.tree, newdata = babies.train,type='vector')
train.MAPE = mean(abs(bwt.train - train.pred)/bwt.train)*100
train.MAPE

test.pred=predict(babies.tree, newdata = babies.test, type='vector')
bwt.test = babies$bwt[test.index]
test.MAPE = mean(abs(bwt.test - test.pred)/bwt.test)*100
test.MAPE

library(rpart)
babies.tree2 = rpart(bwt ~  gestation + parity + age + 						height + weight + smoke, data=mydata)

# 或寫成 babies.tree2=rpart( bwt ~ . , data=babies.train)

devAskNewPage(ask = TRUE)
plot(babies.tree2);  text(babies.tree2)	

