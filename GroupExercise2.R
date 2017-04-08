#******* Group information *******

#Group : 

#Please rename this file to Group_.R
#Please write your code in each block.

#******* end of Group information *******

library(data.table)

#******* 1. Download data and do some data management tasks *******
titanic = na.omit(read.csv(file = "train.csv"))
View(titanic)

#********** end of 1. **********


#********** 2. fit logit models and report 10-fold MR **********
k_fold_CV_logit = function(f, d, k, cutoff){
  numOfRec = nrow(d) # number of observations
  reponse_var = all.vars(f)[1] # name of the reponse variable
  # k indices used to split data into k parts
  sample_idx_k = rep(sample(1:k),round(numOfRec / k) + 1)[1:numOfRec]
  # k models for k subsets of data
  k_fits = Map( function(x) glm(f, d[sample_idx_k != x, ], family = "binomial"), 1:k)
  # Predicted & actual classes for each hold-out subset 
  predActualClass = Map(function(x){
    predictedProb = predict(k_fits[[x]], d[sample_idx_k == x,], type = "response")
    predictedClass = ifelse(predictedProb > cutoff, 1, 0)
    return(data.frame("predictedClass" =  predictedClass, "actualClass" = d[sample_idx_k == x, reponse_var] ) )
  }, 1:k)
  # A data frame with all predicted & actual classes
  output_DF = Reduce(function(x, y) rbind(x, y), predActualClass)
  output_DF$predictedClass = factor(output_DF$predictedClass, levels=c(0,1),labels = c("No", "Yes"))
  return( table(output_DF$predictedClass, output_DF$actualClass))
}

# Accuracy of prediction for models with different feature set
Map(function(x) sum(diag(k_fold_CV_logit(x, titanic, 10, 0.45))) / nrow(titanic),
    list(Survived ~ Sex + Pclass + Parch:SibSp + Age + Embarked))

#********** end of 2. **********


#********** 3.  Remember Jack and Rose in the movie Titanic?  **********
#********** What are their probabilities of surviving the event? **********

glm_survived = glm(Survived ~ Sex + Pclass + Parch:SibSp + Age + Embarked, data = titanic, family = "binomial")
summary(glm_survived)
# Jack's survived rate.
predict(glm_survived, data.frame(Sex = "male", Pclass = 3, Age = 20, Parch = 0, SibSp = 0, Embarked = "Q"), type = "response")
# Rose's survived rate.
predict(glm_survived, data.frame(Sex = "female", Pclass = 1, Age = 17, Parch = 1, SibSp = 0, Embarked = "Q"), type = "response")

#********** end of 3. **********



#********** 4. report result of logit model using epiDisplay
install.packages("epiDisplay")
library(epiDisplay)
epiDisplay::logistic.display(glm_survived)

#********** end of 4. **********
