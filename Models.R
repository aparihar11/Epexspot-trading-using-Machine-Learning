
##########################
# 3 - Variable Selection #
##########################
set.seed(1)
path <- 'C:/Users/mrudrappa/Desktop/Hackaton/basetableApril14.csv'
basetable <-read.csv(path)


# Load-in the libraries required
library(pROC)
library(rpart)
library(randomForest)

# To Randomize the data
shuffle_index <- sample(1:nrow(basetable))
head(shuffle_index)

basetable <- basetable[shuffle_index, ]
head(basetable)


# Create a duplicate basetable with only the required columns for modelling
basetable_2 <- basetable
# Remove from date and from time 
basetable_2$fromdate <- NULL
basetable_2$fromtime <- NULL

# Remove DayaheadPrice._EUR_MWh,IntradayPrice and Price_Diff as it is used for the creation of target variable
basetable_2$IntradayPrice <- NULL
basetable_2$DayaheadPrice._EUR_MWh <- NULL
basetable_2$PriceDIFF <- NULL

# Convert dependant in numeric data type to perform pearson correlation
basetable_2$Target = as.numeric(basetable_2$Target)

# check for 0s and 1s are in equal proportion
table(basetable_2$Target)

# 0s = 16961 
# 1s = 18127

# Split the basetable into Test and Train
# Train 80% and Test 20%

# function that splits train and test
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}


data_train <- create_train_test(basetable_2, 0.8, train = TRUE)
data_test <- create_train_test(basetable_2, 0.8, train = FALSE)
dim(data_train)
dim(data_test)


# We use the function prop.table() combined with table() to verify if the randomization process is correct.

prop.table(table(data_train$Target))
prop.table(table(data_test$Target))

# In both dataset, the amount of 1s are the same, about 51 percent.


# Custom function to calculate AUC:
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}


###########################################
# Logistic regression Model
###########################################

library(caret)

logistic = glm(Target ~ .,data=data_train, family=binomial("logit"),maxit = 500)

predictions_test_logistic_regression <- predict(logistic,newdata= data_test,type="response")
predictions_train_logistic_regression <- predict(logistic,newdata= data_train,type="response")


auc_train_logistic_regression <- auc(data_train$Target,predictions_train_logistic_regression)
auc_test_logistic_regression <- auc(data_test$Target,predictions_test_logistic_regression)


auc_train_logistic_regression 
auc_test_logistic_regression 


# Confusion Matrix Test
predicted_Test_log <- as.numeric(predictions_test_logistic_regression > 0.5)
reference <- data_test$Target
u <- union(predicted_Test_log, reference)
xtab <- table(factor(predicted_Test_log, u), factor(reference, u))
confusionMatrix(xtab)

###########################################
# Descion Tree Model
###########################################
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

fit <- rpart(Target~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)

predictions_train_tree <- as.data.frame(predict(fit, data_train, type = 'prob'))
predictions_test_tree <- as.data.frame(predict(fit, data_test, type = 'prob'))

predictions_train_tree <- as.numeric(predictions_train_tree$`1`)
predictions_test_tree <-  as.numeric(predictions_test_tree$`1`)


auc_train_tree <- auc(data_train$Target,predictions_train_tree)
auc_test_tree <- auc(data_test$Target,predictions_test_tree)

auc_train_tree  
auc_test_tree 




# Confusion Matrix Test
predicted_Test_Tree <- as.numeric(predictions_test_tree > 0.5)
reference <- data_test$Target
u <- union(predicted_Test_Tree, reference)
xtab <- table(factor(predicted_Test_Tree, u), factor(reference, u))
confusionMatrix(xtab)



roc5=roc(data_test$Target ~ predictions_test_tree)
plot(roc5)

roc6=roc(data_train$Target ~ predictions_train_tree)
plot(roc6,add= TRUE, col='red')


#################################
# RANDOM FOREST
#################################
library(randomForest)

rForest <- randomForest(Target ~ .,data = data_train, ntree=200)

predictions_test_random_forest <- as.data.frame(predict(object = rForest, newdata = data_test, type = "prob"))
predictions_train_random_forest <- as.data.frame(predict(object = rForest, newdata = data_train, type = "prob"))

predictions_test_random_forest <- as.numeric(predictions_test_random_forest$`1`)
predictions_train_random_forest <- as.numeric(predictions_train_random_forest$`1`)

auc_train_random_forest <- auc(data_train$survived,predictions_train_random_forest)
auc_test_random_forest <- auc(data_test$survived,predictions_test_random_forest)

auc_train_random_forest  
auc_test_random_forest 


cor(data_train,data_train$Target)

