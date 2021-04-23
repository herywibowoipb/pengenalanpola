#install.packages("plyr") 
#install.packages("caret") # for sampling and confusion matrix, for CV  
#install.packages("randomForest") # for using classification algo - randomForest 
#install.packages("VIM")

library(plyr)
library(caret) 
library(e1071) 
library(randomForest) 
library(ggplot2)
library(VIM) # for missing values
library(corrplot)

#import dataset
dataset = read.csv(file.choose(), header=T, sep=",")
#cek struktur dataset
str(dataset)
#cek missing value
aggr(dataset, numbers = TRUE, prop = c(TRUE, FALSE))

#rename atribut PAY_0 menjadi PAY_1 dan atribut default.payment.next.month menjadi DEFAULT
colnames(dataset)[colnames(dataset) == "PAY_0"] = "PAY_1"
colnames(dataset)[colnames((dataset)) == "default.payment.next.month"] = "DEFAULT"

#convert variabel ke factors
dataset$DEFAULT = as.factor(dataset$DEFAULT)

#cek nilai distribusi atribut DEFAULT
table(dataset$DEFAULT)

#MODELING
#Taking significant columns
remove_feature = c(1, 26, 27, 29, 30)
our.modified.data = dataset[, -remove_feature]

#Structure of our final data set
str(our.modified.data)

# Sampling the data into test set and training set
index = createDataPartition(y = our.modified.data$DEFAULT, times = 1, 
                            p = 0.8, list = F)
trd = our.modified.data[index, ]
tsd = our.modified.data[-index, ]

#Checking the distribution of our DEFAULT variable in our data sets
# our data set
prop.table(table(our.modified.data$DEFAULT))

# our training set
prop.table(table(trd$DEFAULT))

# our testing set
prop.table(table(tsd$DEFAULT))

#Fitting a Random Forest Classifier to our training set
classifier.rf = randomForest(formula = DEFAULT ~., 
                             data = trd, ntree = 40)

summary(classifier.rf)

#Predicting results for testing data
our.predict.rf = predict(classifier.rf, newdata = tsd, type = "class")

#Evaluating model
#Confusion Matrix
confusionMatrix(our.predict.rf, tsd$DEFAULT)

#Evaluating Model using K-fold Cross Validation
# Applying k-fold cross validation - Random Forest
fold = createFolds(y = trd$DEFAULT, k = 10)

cv_rf = lapply(fold, function(x){
  trd_fold = trd[-x, ]
  tsd_fold = trd[x, ]
  
  classifier.rf_f = randomForest(formula = DEFAULT ~., 
                                 data = trd_fold, ntree = 40)
  our.predict.rf_f = predict(classifier.rf_f, newdata = tsd_fold, type = "class")
  
  rf_cm_f = confusionMatrix(our.predict.rf_f, tsd_fold$DEFAULT)
  accuracy = rf_cm_f$overall[1]
  
  return(accuracy)
})

#Accuracy
mean(as.numeric(cv_rf))

