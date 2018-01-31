# Example 15-3: Logistic Regression

# 請修改資料檔路徑
babies = read.csv("c:/r/babies.csv",header=T)

# 請修改 logistic_compare.R 檔案路徑
source("c:/r/logistic_compare.R")

babies = na.exclude(babies)
quantile(babies$bwt,0.25)

bwt2 = ( babies$bwt < 108.75 ) * 1
babies$bwt2 = as.factor( bwt2)		# 將分類變數轉為 factor
p = 0.9
index = sample(2, nrow(babies), replace = TRUE, prob=c(p,1-p))
babies.train = babies[index == 1,]
babies.test = babies[index == 2,]
nrow(babies.train)						# 訓練樣本有 1035筆

train.result = glm(bwt2 ~ gestation + parity + age+ 
  height+ weight + smoke, data=babies.train, 
  family = binomial(link = logit))

summary(train.result)
print(train.result)

exp(train.result$coef)
confint(train.result)

exp(confint(train.result))

library(epicalc)
logistic.display(train.result)

pred = predict(train.result, newdata=babies.train,
				type = "response")
pred = round(pred)
tab = table(Y = babies.train$bwt2,Ypred = pred)
catnames = levels(babies.train$bwt2)
rownames(tab) = catnames
colnames(tab) = catnames
tab
cat('正確分類比例 = ',100*sum(diag(tab))/length(pred),' % \n')

pred = predict(train.result, newdata=babies.test,
                  type = "response")
pred = round(pred)
tab = table(Y = babies.test$bwt2,Ypred = pred) 
catnames = levels(babies.test$bwt2)
rownames(tab) = catnames

colnames(tab) = catnames
tab
cat('正確分類比例 = ',100*sum(diag(tab))/length(pred),' % \n')

# source("c:/r/logistic_compare.R")

x1 = list(gestation=281,parity="1",age=30,height=63,
			weight=112,smoke="0")
x2 = list(gestation=279,parity="0",age=25,height=67,
			weight=100,smoke="1")
			
logistic.compare(X1=x1,X2=x2,Y1="1",Y2="0", 
               data=babies,model=train.result)

