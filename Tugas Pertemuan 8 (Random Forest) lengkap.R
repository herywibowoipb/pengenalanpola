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

#cek korelasi antar atribut
dataset$DEFAULT = as.numeric(dataset$DEFAULT)
r = cor(dataset[-c(3, 4, 5, 26, 27)])
corrplot(r, method = "circle")

#convert variabel ke factors
dataset$DEFAULT = as.factor(dataset$DEFAULT)
dataset$SEX = as.factor(dataset$SEX)

#cek nilai distribusi atribut DEFAULT
table(dataset$DEFAULT)

#peresentase atribut DEFAULT terhadap atribut lainnya
calDefaultPerc = function(x, colInd, data.level){
  default_count = c()
  default_perc = c()
  
  for (i in 1:length(data.level)) {
    print("start of for...")
    total_count = count(x[x[colInd] == data.level[i], ], vars = "DEFAULT")
    count_def = total_count[total_count$DEFAULT == 1, 2]
    total = total_count[total_count$DEFAULT == 1, 2] + total_count[total_count$DEFAULT == 0, 2]
    print(i)
    perc = round(count_def/total * 100, digits = 2)
    
    default_count = c(default_count, count_def)
    default_perc = c(default_perc, perc)
    print("end of for...")
  }
  print("outer for....")
  #data.level
  length(default_count)
  length(default_perc)
  
  paste("data.level: ", length(data.level))
  paste("default_count: ", length(default_count))
  paste("default_perc: ", length(default_perc))
  
  default_perc_df = data.frame(level = data.level, default.count = default_count,
                               percentage = default_perc)
  print("data frame created....")
  return(default_perc_df)
}

# Adding one feature marital status
addMaritalStatus = function(x, colInd, newColInd){
  for (i in 1: nrow(x)) {
    if(x[i, colInd] == 1){
      x[i, newColInd] = "Married"
    }
    else if (x[i, colInd] == 2) {
      x[i, newColInd] = "Not Married"
    }
    else{
      x[i, newColInd] = "Others"
    }
    
  }
  return(x)
}

# calculate diff of each row of total BILL_AMT and total PAY_AMT
calDiffBillAndPayPer = function(x, bill_amt_ind, pay_amt_ind){
  TOTAL_PAYABLE_PERC = c()
  
  for (i in 1:nrow(x)) {
    sum_bill_amt = sum(x[i, bill_amt_ind])
    sum_pay_amt = sum(x[i, pay_amt_ind])
    
    payable_amt =  sum_bill_amt - sum_pay_amt
    if(sum_bill_amt != 0){
      payable_perc = round(payable_amt / sum_bill_amt * 100, digits = 2)
    }
    else{
      payable_perc = 0.00
    }
    
    TOTAL_PAYABLE_PERC = c(TOTAL_PAYABLE_PERC, payable_perc)
  }
  return(TOTAL_PAYABLE_PERC)
}

# function to find the range of REMAINING_AMT_PER_RANGE
getRemPerRange = function(x, colInd){
  all_per_range = c()
  
  for (i in 1:nrow(x)) {
    per_range = -1
    
    if(x[i, colInd] >=0 & x[i, colInd] <= 10)
      per_range = 0
    else if(x[i, colInd] >10 & x[i, colInd] <= 20)
      per_range = 1
    else if(x[i, colInd] >20 & x[i, colInd] <= 30)
      per_range = 2
    else if(x[i, colInd] >30 & x[i, colInd] <= 40)
      per_range = 3
    else if(x[i, colInd] >40 & x[i, colInd] <= 50)
      per_range = 4
    else if(x[i, colInd] >50 & x[i, colInd] <= 60)
      per_range = 5
    else if(x[i, colInd] >60 & x[i, colInd] <= 70)
      per_range = 6
    else if(x[i, colInd] >70 & x[i, colInd] <= 80)
      per_range = 7
    else if(x[i, colInd] >80 & x[i, colInd] <= 90)
      per_range = 8
    else if(x[i, colInd] >90 & x[i, colInd] <= 100)
      per_range = 9
    else
      per_range = -1
    
    all_per_range = c(all_per_range, per_range)
  }
  
  return(all_per_range)
}

# Function to find the Limit Balance Range
getLimitBalRange = function(x, colInd){
  limit_bal_seg = c()
  
  for (i in 1:nrow(x)) {
    bal_seg = 0
    if (10000 >= x[i, colInd] & x[i, colInd] < 100000) 
      bal_seg = 0
    else if (100000 >= x[i, colInd] & x[i, colInd] < 200000) 
      bal_seg = 1
    else if (200000 >= x[i, colInd] & x[i, colInd] < 300000) 
      bal_seg = 2
    else if (300000 >= x[i, colInd] & x[i, colInd] < 400000) 
      bal_seg = 3
    else if (400000 >= x[i, colInd] & x[i, colInd] < 500000) 
      bal_seg = 4
    else if (500000 >= x[i, colInd] & x[i, colInd] < 600000) 
      bal_seg = 5
    else if (600000 >= x[i, colInd] & x[i, colInd] < 700000) 
      bal_seg = 6
    else if (700000 >= x[i, colInd] & x[i, colInd] < 800000) 
      bal_seg = 7
    else if (800000 >= x[i, colInd] & x[i, colInd] < 900000) 
      bal_seg = 8
    else
      bal_seg = 9
    
    limit_bal_seg = c(limit_bal_seg, bal_seg)
  }
  
  return(limit_bal_seg)
}

#feature gender
dataset$GENDER = ifelse(dataset$SEX == 1, "Male", "Female")

# Bar Graph for gender
ggplot(data = dataset, mapping = aes(x = GENDER, fill = DEFAULT)) +
  geom_bar() +
  ggtitle("Gender") +
  stat_count(aes(label = ..count..), geom = "label")

#merging nilai 0,4,5,6 menjadi 4(Lainnya) pada atribut EDUCATION
dataset$EDUCATION = ifelse(dataset$EDUCATION == 0 |dataset$EDUCATION == 5 | dataset$EDUCATION == 6,
                            4, dataset$EDUCATION)

#covert ataribut EDUCATION menajadi kategorikal
dataset$EDUCATION = as.factor(dataset$EDUCATION)

#plotting bar graph pada atribut EDUCATION
ggplot(data = dataset, mapping = aes(x = EDUCATION, fill = DEFAULT)) +
  geom_bar() +
  ggtitle("EDUCATION") +
  stat_count(aes(label = ..count..), geom = "label")

#Default % in all education type
default_count_perc = calDefaultPerc(x = dataset, colInd = 4, 
                                    data.level = levels(dataset$EDUCATION))

#Plotting a Bar graph to show defaulters %AGE
ggplot(data = default_count_perc, mapping = aes(x = reorder(level, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "#b3e569") +
  coord_flip() +
  xlab("Education") +
  ylab("Percentage of Defaulters") +
  ggtitle("Education level vs default") +
  geom_label(label = paste(default_count_perc$percentage, "%"))

#Checking the unique value of MARRIAGE variable
unique(dataset$MARRIAGE)

# Merging nilai 0 ke 3(others)
dataset$MARRIAGE = ifelse(dataset$MARRIAGE == 3, 0, dataset$MARRIAGE)

#Convert atribut MARRIAGE to categorical variable
dataset$MARRIAGE = as.factor(dataset$MARRIAGE)

table(dataset$MARRIAGE)

#Adding new feature MARITALSTATUS
dataset$MARITALSTATUS = NA
dataset = addMaritalStatus(dataset, which(colnames(dataset) == "MARRIAGE"), 
                            which(colnames(dataset) == "MARITALSTATUS"))

## convert to categorical data
dataset$MARITALSTATUS = as.factor(dataset$MARITALSTATUS)

#Plotting a bar graph for MARITALSTATUS
# Bar graph for marital status
ggplot(data = dataset, mapping = aes(x = MARITALSTATUS, fill = DEFAULT)) +
  geom_bar() +
  xlab("Marital status") +
  ggtitle(" Defaulters on Marital Status") +
  stat_count(aes(label = ..count..), geom = "label")

#Find out the defaulter %age as per MARITALSTATUS
marital_default_per_df = calDefaultPerc(x = dataset, 
                                        colInd = which(colnames(dataset) == "MARITALSTATUS"), 
                                        data.level = levels(dataset$MARITALSTATUS))

marital_default_per_df

#Plotting a Bar graph for defaulters %age as per their MARITALSTATUS
# Bar Graph
ggplot(data = marital_default_per_df, mapping = aes(x = reorder(level, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "#f47d42") +
  coord_flip() +
  xlab("Marital Status") +
  ylab("Percentage of Defaulters") +
  ggtitle("Marital Status vs default") +
  geom_label(label = paste(marital_default_per_df$percentage, "%"))

#Finding all the unique value PAY_1
unique(dataset$PAY_1)

#Changing PAY_1 into categorical variable
dataset$PAY_1 = as.factor(dataset$PAY_1)

#Finding out the defaulters %age as per PAY_1 values
# Explore PAY_1 level vs Default
pay_1_default_perc.df = calDefaultPerc(x = dataset, 
                                       colInd = which(colnames(dataset) == "PAY_1"), 
                                       data.level = levels(dataset$PAY_1))

pay_1_default_perc.df

#Plotting the graph defaulter %age graph as per PAY_1 values
#PAY_1
ggplot(data = pay_1_default_perc.df, mapping = aes(x = reorder(level, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "#ce9b2d") +
  coord_flip() +
  xlab("PAY_1") +
  ylab("Percentage of Defaulters") +
  ggtitle("Credit Behaviour(PAY_1) vs default") +
  geom_label(label = paste(pay_1_default_perc.df$percentage, "%"))

#Add one feature REMAINING_AMT_PER
check = calDiffBillAndPayPer(x = dataset, bill_amt_ind = c(13:18), pay_amt_ind = c(19:24))
dataset$REMAINING_AMT_PER = check

#Add one more feature REMAINING_AMT_PER_RANGE
check = getRemPerRange(x = dataset, 
                       colInd = which(colnames(dataset) == 'REMAINING_AMT_PER'))
dataset$REMAINING_AMT_PER_RANGE = check

#Converting it into categorical variable
# change it to factors
dataset$REMAINING_AMT_PER_RANGE = as.factor(dataset$REMAINING_AMT_PER_RANGE)

# Finding the %age of defaulters as per remaining_amt_perc
# Explore REMAINING_AMT_PER_RANGE vs default
RemainingAmtPerc_per_df = calDefaultPerc(x = dataset, 
                                         colInd = which(colnames(dataset) == "REMAINING_AMT_PER_RANGE"), 
                                         data.level = levels(dataset$REMAINING_AMT_PER_RANGE))
print(RemainingAmtPerc_per_df)

#Plotting a Bar graph of defaulters % as per their remaining amount percentage
#REMAINING_AMT_PER_RANGE
ggplot(data = RemainingAmtPerc_per_df, mapping = aes(x = reorder(level, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "#42f498") +
  coord_flip() +
  xlab("REMAINING_AMT_PER_RANGE") +
  ylab("Percentage of Defaulters") +
  ggtitle("Credit Behaviour(Remaining amount) vs default") +
  geom_label(label = paste(RemainingAmtPerc_per_df$percentage, "%"))

#Find the min and max of LIMIT_BAL
summary(dataset$LIMIT_BAL)

#Plotting a Density curve for LIMIT_BAL
# LIMIT_BAL density plot
ggplot(data = dataset, mapping = aes(x = LIMIT_BAL)) + 
  geom_density(fill = "#f0f9a7") +
  ggtitle("LIMIT_BAL Distribution") +
  xlab("LIMIT_BAL") +
  geom_vline(xintercept = mean(dataset$LIMIT_BAL), col = "red", 
             linetype = "dashed", size = 0.6) +
  annotate("text", 
           x = -Inf, y = Inf, 
           label = paste("Mean:", round(mean(dataset$LIMIT_BAL), digits = 2)), 
           hjust = 0, vjust = 1, col = "red", size = 3)

#Divide LIMIT_BAL into segement
limit_bal_seg = getLimitBalRange(x = dataset, 
                                 colInd = which(colnames(dataset) == 'LIMIT_BAL'))

#Add a new feature LIMIT_BAL_SEG
dataset$LIMIT_BAL_SEG = limit_bal_seg

#Convert it into categorical variable
# change it into factor
dataset$LIMIT_BAL_SEG = as.factor(dataset$LIMIT_BAL_SEG)


#MODELING

#Taking significant columns
remove_feature = c(1, 26, 27, 29, 30)
our.modified.data = dataset[, -remove_feature]

#Structure of our final data set
str(our.modified.data)

#Sampling
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
                             data = trd, ntree = 10)

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
                                 data = trd_fold, ntree = 20)
  our.predict.rf_f = predict(classifier.rf_f, newdata = tsd_fold, type = "class")
  
  rf_cm_f = confusionMatrix(our.predict.rf_f, tsd_fold$DEFAULT)
  accuracy = rf_cm_f$overall[1]
  
  return(accuracy)
})

#Accuracy
mean(as.numeric(cv_rf))
