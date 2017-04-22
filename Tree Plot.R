#install.packages("tree")
#install.packages("rpart")
#install.packages("CHAID")
#install.packages("randomForest")

HouseAnalysis = read.csv("house_data_UPDATE1.csv",header=T)
head(HouseAnalysis,3)
HouseAnalysis = na.exclude(HouseAnalysis)
#隨機抽出10%觀察值當作保留的測試樣本，剩下做為Tree模型所用的訓練樣本
np = ceiling(0.1*nrow(HouseAnalysis))
np#54個測試樣本
test.index = sample(1:nrow(HouseAnalysis),np)
#測試樣本
HouseAnalysis.test=HouseAnalysis[test.index,]
#訓練樣本
HouseAnalysis.train=HouseAnalysis[-test.index]
install.packages("tree")
library(tree);library(CHAID)
#建立決策樹的模型
HouseAnalysis.tree = tree(Total_Price~. , data=HouseAnalysis.train)
plot(HouseAnalysis.tree);text(HouseAnalysis.tree)
plot(HouseAnalysis$Finish_Build,HouseAnalysis$Total_Price)
slm.model = lm()

library("randomForest")
iris.tf = randomForest(Species~.,data = iris)
iris.tree = tree(Species~.,data=iris)
iris.tf 
plot(iris.tf)
plot(iris.tree);text(iris.tree)
plot(HouseAnalysis.tree); text(HouseAnalysis.tree)

#-------------------------------------------------------------------#
library(rpart)
plot(HouseAnalysis$Total_Price,HouseAnalysis$Car_Type);text(HouseAnalysis)

result = lm(Total_Price~Finish_Build,data=HouseAnalysis)
plot(result)

