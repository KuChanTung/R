library(caret)
#-------------------------------------------------------
# (1) caret + xgboost: xgbTree, auto-tuning
#-------------------------------------------------------
# create training sample and test sample
index = createDataPartition(iris$Species, p = 0.9, list = FALSE)
iris.Train = iris[index, ]
iris.Test = iris[-index, ]

# set caret tuning configuration: 5-Fold CV, 3 repetitions
ctrl = trainControl(method = "repeatedcv", number=5, repeats = 3)	

# number of Classes in Species
m = nlevels(iris$Species) 

xgFit1 = train(Species ~ ., data = iris.Train,trControl = ctrl, method = "xgbTree", num_class = m )

xgFit1 
plot(xgFit1)

# Training sample
Ypred1 = predict(xgFit1,iris.Train)
confusionMatrix(iris.Train$Species,Ypred1)

# Test sample
Ypred2 = predict(xgFit1,iris.Test)
confusionMatrix(iris.Test$Species,Ypred2)

#-------------------------------------------------------
# (2) caret + xgboost: xgbTree, specify parameters
#-------------------------------------------------------

# Specify ranges of parameters 
trGrid = expand.grid(nrounds = c(50,100),  max_depth = 10, eta = 0.12,  
                     gamma = 0,               #default=0
                     colsample_bytree = 1,    #default=1
                     min_child_weight = 1,    #default=1
                     subsample = c(0.5,0.75)
)

xgFit2 = train(Species ~ .,  data = iris.Train,
               trControl = trainControl,  method = "xgbTree",
               tuneGrid = trGrid,  
               num_class = 3
)

xgFit2 
plot(xgFit2)

# Training sample
Ypred1 = predict(xgFit2,iris.Train)
confusionMatrix(iris.Train$Species,Ypred1)

# Test sample
Ypred2 = predict(xgFit2,iris.Test)
confusionMatrix(iris.Test$Species,Ypred2)

#-------------------------------------------------------
# (3) xgbLinear: Numerical Prediction
#-------------------------------------------------------
xgFit3 = train(Sepal.Length ~ ., data = iris.Train,
               trControl = trainControl,
               method = "xgbLinear" )

xgFit3 
plot(xgFit3)

MAPE = function(Y, Ypred) mean(abs((Y - Ypred)/Y))

# Training sample
Ypred1 = predict(xgFit3,iris.Train)
MAPE(iris.Train$Sepal.Length,Ypred1)

# Test sample
Ypred2 = predict(xgFit3,iris.Test)
MAPE(iris.Test$Sepal.Length,Ypred2)