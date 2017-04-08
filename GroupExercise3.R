 #******* Group information *******

#Group :

#Please rename this file to Group_.R
#Please write your code in each block.

#******* end of Group information *******


#******* 1. Download data, do some data management tasks and separate it into two datasets.*******
library(data.table)
titanic <- fread("train.csv", verbose = T, data.table = F, na.strings = "")
str(titanic)
titanic$Age <- ifelse(is.na(titanic$Age), median(titanic$Age, na.rm = T), titanic$Age)
summary(titanic)

titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- ifelse(is.na(titanic$Embarked), "S", titanic$Embarked)
titanic$Embarked <- as.factor(titanic$Embarked)

i <- sample(1:nrow(titanic), nrow(titanic) * 0.7)
train <- titanic[i, ]
test <- titanic[-i, ]

#********** end of 1. **********


#********** 2. fit a classification tree and a random forest on the training data **********
set.seed(1234)

library(rpart)
titanic_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                      data = train, control = rpart.control(cp = 0, xval = 10))
plotcp(titanic_tree)
printcp(titanic_tree)

library(partykit)
plot(as.party(titanic_tree))

library(randomForest)
train_rf <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                         data = train, ntree = 500)


train_rf_err = reshape2::melt(train_rf$err.rate)
colnames(train_rf_err) = c("n_of_trees", "err.typ","err.rate")
qplot(n_of_trees, err.rate, color=err.typ,data = train_rf_err, geom="line") + ylim(0.05, 0.3)
train_rf



#********** end of 2. **********


#********** 3.  Please plot the ROC curve of the two models and
#**********    try to compare your models with test set accuracy and AUCs  **********
library(pROC)
# ROC and AUCs for testing data
tree_pred_prob = predict(titanic_tree, newdata = test, type = "prob")[, "1"];
titanic_tree_roc = roc(test[, "Survived"], tree_pred_prob)
titanic_tree_roc


train_rf_pred_prob = predict(train_rf, newdata = test, type = "prob")[, "1"];
train_rf_roc = roc(test[, "Survived"], train_rf_pred_prob)
train_rf_roc


#********** end of 3. **********



#********** 4. do tree pruning on your classification tree and compare it with previous models.
train_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                    data = train, control = rpart.control(xval = 10))
plotcp(train_tree)
printcp(train_tree)


plot(as.party(train_tree))

train_tree_pred_prob = predict(train_tree, newdata = test, type = "prob")[, "1"];
train_tree_roc = roc(test[, "Survived"], train_tree_pred_prob)
train_tree_roc

plot.roc(titanic_tree_roc, col = "blue")
plot.roc(train_rf_roc, add = T, col="red")
plot.roc(train_tree_roc, add = T, col="yellow")
#********** end of 4. **********
