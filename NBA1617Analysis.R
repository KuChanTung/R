#install.packages("tree")
#install.packages("rpart")
#install.packages("CHAID")
#install.packages("randomForest")

NBA1617Analysis = read.csv("NBA1617TeamPerGameStats.csv",header=T)
#head(USAHouseAnalysis,3)
#USAHouseAnalysis = na.exclude(USAHouseAnalysis)
#隨機抽出10%觀察值當作保留的測試樣本，剩下做為Tree模型所用的訓練樣本
np = ceiling(0.1*nrow(NBA1617Analysis))
np#54個測試樣本
test.index = sample(1:nrow(NBA1617Analysis),np)
#測試樣本
NBA1617Analysis.test=NBA1617Analysis[test.index,]
#訓練樣本
NBA1617Analysis.train=NBA1617Analysis[-test.index]
#install.packages("tree")
library(tree);library(CHAID)
#建立決策樹的模型
NBA1617Analysis.tree = tree(Team~. , data=NBA1617Analysis.train)
plot(NBA1617Analysis.tree);text(NBA1617Analysis.tree)
plot(NBA1617Analysis$Team,NBA1617Analysis$PTS)
#slm.model = lm()

library("randomForest")
iris.tf = randomForest(Species~.,data = iris)
iris.tree = tree(Species~.,data=iris)
iris.tf 
plot(iris.tf)
plot(iris.tree);text(iris.tree)
plot(NBA1617Analysis.tree); text(NBA1617Analysis.tree)

#-------------------------------------------------------------------#
library(rpart)
plot(NBA1617Analysis$X3P,NBA1617Analysis$X2P);text(NBA1617Analysis)
result = lm(Team~PTS,data=NBA1617Analysis)
plot(result)





# Principal component analysis on "USArrests"
data(USAHouseAnalysis)
X = scale(as.matrix(USAHouseAnalysis)) # "Center" (standardize) the data
cov_X= cov(X) # Covariance matrix
# Eigen decomposition of the covariance matrix
eigen_judge= eigen(cov_X)
eigen_judge$vectors[,1] # 1st PC loadings (weights)
# 1st PC score for each observation
as.matrix(X) %*% matrix(eigen_judge$vectors[,1], ncol= 1)
# sum of squared loadings is 1
sum(eigen_arrest$vectors[,1] ^ 2)
