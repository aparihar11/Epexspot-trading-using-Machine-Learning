########################################################
#load the required packages
########################################################

library(xgboost)
library(dplyr)
library(pROC)
library(data.table)
library(DMwR)
library(MASS)
library(lubridate)
library(MLmetrics)
library(caret)
library(corrplot)

########################################################
#reading and cleaning the data
########################################################

path <- 'C:/Users/mrudrappa/Desktop/Hackaton/basetableApril14.csv'
path1 <- 'C:/Users/mrudrappa/Desktop/Hackaton/weather.csv'
basetable <-read.csv(path)
weather <- read.csv(path1)



# remove unwanted columns from weather dataset
weather$time1 <- NULL
weather$weatherIconUrl <- NULL
weather$winddir16point <- NULL
weather$weatherDesc <- NULL

basetable1 <- basetable

# keep only distinct rows
weather = unique(weather)

# merge basetable1 and weather data
basetable1 <- merge(x=basetable1,y=weather,by=c('fromdate','fromtime'),all.x=TRUE)


# keep only distinct rows
basetable1 <- basetable1 %>% distinct(fromdate, fromtime, .keep_all = TRUE)

# convert fromdate to date format
basetable1$fromdate <- mdy(basetable1$fromdate)

# sorting basetable with date and time
basetable1 <- arrange(basetable1,fromdate,fromtime)


# Create Lag Variables
basetable1$lag_24_intraday <- shift(basetable1$IntradayPrice, n=24, fill=NA, type="lag")
basetable1$lag_36_intraday <- shift(basetable1$IntradayPrice, n=36, fill=NA, type="lag")
basetable1$lag_48_intraday <- shift(basetable1$IntradayPrice, n=48, fill=NA, type="lag")
basetable1$lag_60_intraday <- shift(basetable1$IntradayPrice, n=60, fill=NA, type="lag")
basetable1$lag_72_intraday <- shift(basetable1$IntradayPrice, n=72, fill=NA, type="lag")
basetable1$lag_84_intraday <- shift(basetable1$IntradayPrice, n=84, fill=NA, type="lag")
basetable1$lag_96_intraday <- shift(basetable1$IntradayPrice, n=96, fill=NA, type="lag")


basetable1$lag_24_dayahead <- shift(basetable1$DayaheadPrice._EUR_MWh, n=24, fill=NA, type="lag")
basetable1$lag_36_dayahead <- shift(basetable1$DayaheadPrice._EUR_MWh, n=36, fill=NA, type="lag")
basetable1$lag_48_dayahead <- shift(basetable1$DayaheadPrice._EUR_MWh, n=48, fill=NA, type="lag")
basetable1$lag_60_dayahead <- shift(basetable1$DayaheadPrice._EUR_MWh, n=60, fill=NA, type="lag")
basetable1$lag_72_dayahead <- shift(basetable1$DayaheadPrice._EUR_MWh, n=72, fill=NA, type="lag")
basetable1$lag_84_dayahead <- shift(basetable1$DayaheadPrice._EUR_MWh, n=84, fill=NA, type="lag")
basetable1$lag_96_dayahead <- shift(basetable1$DayaheadPrice._EUR_MWh, n=96, fill=NA, type="lag")

basetable1$lag_24_PriceDIFF <- shift(basetable1$PriceDIFF, n=24, fill=NA, type="lag")
basetable1$lag_36_PriceDIFF <- shift(basetable1$PriceDIFF, n=36, fill=NA, type="lag")
basetable1$lag_48_PriceDIFF <- shift(basetable1$PriceDIFF, n=48, fill=NA, type="lag")
basetable1$lag_60_PriceDIFF <- shift(basetable1$PriceDIFF, n=60, fill=NA, type="lag")
basetable1$lag_72_PriceDIFF <- shift(basetable1$PriceDIFF, n=72, fill=NA, type="lag")
basetable1$lag_84_PriceDIFF <- shift(basetable1$PriceDIFF, n=84, fill=NA, type="lag")
basetable1$lag_96_PriceDIFF <- shift(basetable1$PriceDIFF, n=96, fill=NA, type="lag")



# Remove all nas
basetable1 <- na.omit(basetable1)

# Create Price diff variable
basetable1$PriceDIFF <- basetable1$IntradayPrice - basetable1$DayaheadPrice._EUR_MWh

# create a Target variable
basetable1$Target=ifelse(basetable1$PriceDIFF > (sd(basetable1$PriceDIFF) - (2 * mean(basetable1$PriceDIFF))),1,0)

# Convert Target as Factor
basetable1$Target <- as.factor(basetable1$Target)

basetable1=as.data.frame(basetable1)


#################################################################
#splitting the data to train (60 %) validation (20%) test (20 %) 
#################################################################

set.seed(111)
var=0.6
var2=0.8
train_len=round(nrow(basetable1)*var)
val_len=round(nrow(basetable1)*var2)
test_len=nrow(basetable1)

#### Training Set
data_train<-slice(basetable1,1:20921)

#### Validation Set
data_validation<-slice(basetable1,20922:27893)

#### Test Set
data_test<-slice(basetable1,27894:test_len)
data_test1<-slice(basetable1,27894:test_len)



# check the proportion of 0s nand 1s
table(data_train$Target) # imbalanced target    

# remove the variables in train dataset that are highly correlated with target
data_train$Year=year(data_train$fromdate)
data_train$Month=month(data_train$fromdate)
data_train$day=day(data_train$fromdate)
data_train$fromdate=NULL
data_train$PriceDIFF=NULL
data_train$IntradayPrice=NULL
data_train$DayaheadPrice._EUR_MWh=NULL

# remove the variables in validation dataset that are highly correlated with target
data_validation$Year=year(data_validation$fromdate)
data_validation$Month=month(data_validation$fromdate)
data_validation$day=day(data_validation$fromdate)
data_validation$fromdate=NULL
data_validation$PriceDIFF=NULL
data_validation$IntradayPrice=NULL
data_validation$DayaheadPrice._EUR_MWh=NULL


# remove the variables in test dataset that are highly correlated with target
data_test$Year=year(data_test$fromdate)
data_test$Month=month(data_test$fromdate)
data_test$day=day(data_test$fromdate)
data_test$fromdate=NULL
data_test$PriceDIFF=NULL
data_test$IntradayPrice=NULL
data_test$DayaheadPrice._EUR_MWh=NULL

############################################################
#smote the train data to balance the proportion of 0s and 1s 
############################################################

data_balanced_over <- SMOTE(Target ~ ., data_train,k=10, perc.over =500,perc.under = 100)

# check for the proportions of 0s and 1s
table(data_balanced_over$Target)

names(data_train)

############################################################
# Modelling  
############################################################

# FILTER (PEARSON CORRELATION)

# For each variable, calculate the pearson correlation between the variable and churn, and the corresponding p-value

# Construct a model on train data that has only those variables that have a p-value lower than 0.001

# Calculate train and test AUC of this model


# Custom function to calculate AUC: 
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}


# STEPWISE LOGISTIC REGRESSION

set.seed(111)
# All possible variables:
variables = names(data_balanced_over)[-14]
variablesorder = c()

# Construct a logistic regression model with no variables
model = glm(Target ~ 1,data=data_balanced_over,family=binomial)

# Construct a formula with all the variables
formula<-formula(paste("Target","~",paste(variables,collapse="+")))

# Stepwise procedure
for(i in c(1:length(variables))){
  #calculate AIC of each model
  info = add1(model,scope=formula,data=data_balanced_over)
  print(info)
  #get variable with lowest AIC
  orderedvariables = rownames(info[order(info$AIC),])
  v = orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  variablesorder = append(variablesorder,v)
  formulanew = formula(paste("Target","~",paste(variablesorder,collapse = "+")))
  model = glm(formulanew,data=data_balanced_over,family=binomial)
  print(v)
}



auctrain = rep(0,length(variablesorder)-1)
auctest = rep(0,length(variablesorder)-1)
for(i in c(1:(length(variablesorder)-1))){
  vars = variablesorder[0:i+1]
  print(vars)
  formula<-paste("Target","~",paste(vars,collapse="+"))
  model<-glm(formula,data=data_balanced_over,family="binomial")	
  predicttrain<-predict(model,newdata=data_balanced_over,type="response")
  predicttest<-predict(model,newdata=data_validation,type="response")
  auctrain[i] = auc(data_balanced_over$Target,predicttrain)
  auctest[i] = auc(data_validation$Target,predicttest)
  
} 

# Plot AUC of train and test

plot(auctrain, main="AUC", col="red",ylim=c(0.5,0.9))
par(new=TRUE)
plot(auctest,col="blue",ylim=c(0.5,0.75))

#Select the model with optimal number of variables:

# LOGISTIC REGRESSION
finalvariables = variablesorder[c(1:25)]
formula<-paste("Target","~",paste(finalvariables,collapse="+"))
set.seed(111)
model<-glm(formula,data=data_balanced_over,family="binomial")	
predicttrain<-predict(model,newdata=data_balanced_over,type="response")
predicttest<-predict(model,newdata=data_validation,type="response")
auctrain = auc(data_balanced_over$Target,predicttrain)
auctest = auc(data_validation$Target,predicttest)

auctrain
auctest



#BUILD THE CONFUSION MATRIX TO CHECK SPECIFICITY AND SENSITIVITY WITH CUT OFF 0.5

predicted <- ifelse(predicttest>0.5,1,0)
reference <- data_validation$Target
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)

# F1 Score
F1_Score(as.numeric(data_validation$Target), predicted, positive = 1)


############################################################
# XG BOOST MODEL 
############################################################


#d2$datetime=as.POSIXct(d2$datetime,origin="1970-01-01")
dtrain <- xgb.DMatrix(data = as.matrix(data_balanced_over[,-14]), label = as.matrix(data_balanced_over$Target))
dvalidate <- xgb.DMatrix(data = as.matrix(data_validation[,-14]),label=as.matrix(data_validation$Target))

#Building model
set.seed(111)
xgb <-  xgboost(booster="gbtree",data = dtrain, nfold = 5,nrounds = 200, verbose = T,
                objective = "binary:logistic", eval_metric = "auc", nthread = 8, eta = 0.01,
                gamma = 0.0468, max_depth = 4, min_child_weight = 1.3, subsample = 0.769, colsample_bytree =0.283)

# View the variables that have the highest predictive power
mat <- xgb.importance(feature_names = colnames(dtrain), model = xgb)
dev.off() # to reset the plotting configurations
xgb.plot.importance(importance_matrix = mat[1:6])

# Create predictions using the model
prediction_xgb <- predict(xgb, newdata = dvalidate)
xgbpred <- ifelse (prediction_xgb > 0.5,1,0)

cm = table(data_validation$Target, xgbpred)
cm
Accuracy=(cm[1]+cm[4])/sum(cm)
Accuracy

##### AUC and ROC Curve #########
roc(xgbpred,as.numeric(data_validation$Target))
F1_Score(data_validation$Target, xgbpred, positive = 1)


##########################################################################

# AUC for logistic regression is best and we use same to calculate profit

# Calculate Profit for Logistic regression model : Test set
##########################################################################

# LOGISTIC REGRESSION On Test set
finalvariables = variablesorder[c(1:25)]
formula<-paste("Target","~",paste(finalvariables,collapse="+"))
set.seed(111)

predicttest<-predict(model,newdata=data_test,type="response")

auctest = auc(data_test$Target,predicttest)

auctest


#BUILD THE CONFUSION MATRIX TO CHECK SPECIFICITY AND SENSITIVITY WITH CUT OFF 0.5

predicted <- ifelse(predicttest>0.5,1,0)
reference <- data_test$Target
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)

# F1 Score
F1_Score(as.numeric(data_test$Target), predicted, positive = 1)

data_test$pricediff <- NULL
data_test$predicted <- NULL
data_test <- cbind(data_test, predicted)
data_test <- cbind(data_test, data_test1$PriceDIFF)

colnames(data_test)[colnames(data_test) == "data_test1$PriceDIFF"] <- "pricediff"


# calculate profit
# amount without running the model
amt_without_model <- sum(data_test$pricediff) - (0.07 * nrow(data_test))


amt1 <- data_test %>% group_by(predicted, Target) %>% summarize(amt1 = sum(pricediff))

amt2 <- amt1$amt1[c(3)] + amt1$amt1[c(4)]

amt3 <- sum( predicted == 1 ) 

amt_with_model <- amt2 - (0.07 *  amt3 )

profit = amt_with_model - amt_without_model
profit


