library(xgboost)
library(dplyr)
library(pROC)
library(data.table)
library(DMwR)
library(MASS)
library(lubridate)
library(MLmetrics)
path <- 'C:/Users/mrudrappa/Desktop/Hackaton/basetableApril14.csv'
basetable <-read.csv(path)

basetable1 <- basetableApril14

# keep only distinct rows
basetable1 <- basetable1 %>% distinct(fromdate, fromtime, .keep_all = TRUE)

# convert fromdate to date format
#basetable1$fromdate <- as.Date(basetable1$fromdate, format = "%Y-%m-%d")

# sorting basetable with date and time
basetable1 <- arrange(basetable1,fromdate,fromtime)

# Create Lag Variables
basetable1$lag_24_intraday <- shift(basetable1$IntradayPrice, n=24, fill=0, type="lag")
basetable1$lag_48_intraday <- shift(basetable1$IntradayPrice, n=48, fill=NA, type="lag")
basetable1$lag_72_intraday <- shift(basetable1$IntradayPrice, n=72, fill=NA, type="lag")
basetable1$lag_96_intraday <- shift(basetable1$IntradayPrice, n=96, fill=NA, type="lag")


basetable1$lag_24_dayahead <- shift(basetable1$DayaheadPrice._EUR_MWh, n=24, fill=0, type="lag")
basetable1$lag_48_dayahead <- shift(basetable1$DayaheadPrice._EUR_MWh, n=48, fill=NA, type="lag")
basetable1$lag_72_dayahead <- shift(basetable1$DayaheadPrice._EUR_MWh, n=72, fill=NA, type="lag")
basetable1$lag_96_dayahead <- shift(basetable1$DayaheadPrice._EUR_MWh, n=96, fill=NA, type="lag")

basetable1$lag_24_PriceDIFF <- shift(basetable1$PriceDIFF, n=24, fill=0, type="lag")
basetable1$lag_48_PriceDIFF <- shift(basetable1$PriceDIFF, n=48, fill=NA, type="lag")
basetable1$lag_72_PriceDIFF <- shift(basetable1$PriceDIFF, n=72, fill=NA, type="lag")
basetable1$lag_96_PriceDIFF <- shift(basetable1$PriceDIFF, n=96, fill=NA, type="lag")

##########################
# still working on this: Add weather variables
##########################
# 
# install.packages("rjson")
# library(rjson)
# 
# json_file <- "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=cf8d3c04b4b445f49bf174310190505&q=France&format=json&date=2015-01-01&enddate=2018-12-31&includelocation=yes&tp=1"
# json_data <- fromJSON(json_file)
# 
# t <- json_data$data[[3]]
# 
# r <- do.call(rbind.data.frame, t)
# 
# install.packages("plyr")
# library (plyr)
# df <- ldply (t, data.frame)



#####################Modeling ####################
basetable1$Target=ifelse(basetable1$PriceDIFF>sd(basetable1$PriceDIFF),1,0)

basetable1=as.data.frame(basetable1)
############# Balancing Data ###############
basetable1$Target <- as.factor(basetable1$Target)

#06/01/15-31/12/18

set.seed(111)
var=0.6
var2=0.8
train_len=round(nrow(basetable1)*var)
val_len=round(nrow(basetable1)*var2)
test_len=nrow(basetable1)

#### Training Set
train<-slice(basetable1,1:train_len) 
#### Validation Set
validation<-slice(basetable1,20965:27952)
#validation<-slice(basetable1,train_len+1:val_len)
#### Test Set
test<-slice(basetable1,27953:test_len)
#test<-slice(basetable1,val_len+1:test_len)

table(train$Target)

train$Year=year(train$fromdate)
train$Month=month(train$fromdate)
train$day=day(train$fromdate)
train$fromdate=NULL
train$PriceDIFF=NULL
train$IntradayPrice=NULL
train$DayaheadPrice._EUR_MWh=NULL
train=train[,c(18:20,1:13,15:17,14)]

validation$Year=year(validation$fromdate)
validation$Month=month(validation$fromdate)
validation$day=day(validation$fromdate)
validation$fromdate=NULL
validation$PriceDIFF=NULL
validation$IntradayPrice=NULL
validation$DayaheadPrice._EUR_MWh=NULL
validation=validation[,c(18:20,1:13,15:17,14)]

test$Year=year(test$fromdate)
test$Month=month(test$fromdate)
test$day=day(test$fromdate)
test$fromdate=NULL
test$PriceDIFF=NULL
test$IntradayPrice=NULL
test$DayaheadPrice._EUR_MWh=NULL
test=test[,c(18:20,1:13,15:17,14)]

smoted_data <- SMOTE(Target ~ ., train,k=10, perc.over =1000,perc.under = 150)

table(smoted_data$Target)
names(train)
#smoted_data$Target=as.numeric(smoted_data$Target)-1
#validation$Target=as.numeric(validation$Target)-1

# Fit the full model 
full_model <-  glm(Target~.,smoted_data,family="binomial")
summary(full_model)
step_model <- stepAIC(full_model, direction = "both", trace = FALSE)
summary(step_model)
prediction_step <- predict(step_model, validation, type="response")
pred_glm <- ifelse (prediction_step > 0.5,1,0)

cm = table(validation$Target, pred_glm)
cm
Accuracy=(cm[1]+cm[4])/sum(cm)
Accuracy

##### AUC and ROC Curve #########
roc(pred_glm,as.numeric(validation$Target))
F1_Score(as.numeric(validation$Target), pred_glm, positive = NULL)

#############XGBOOST############

#d2$datetime=as.POSIXct(d2$datetime,origin="1970-01-01")
dtrain <- xgb.DMatrix(data = as.matrix(smoted_data[,-20]), label = as.matrix(smoted_data$Target))
dvalidate <- xgb.DMatrix(data = as.matrix(validation[,-20]),label=as.matrix(validation$Target))

#Building model
set.seed(111)
xgb <-  xgboost(booster="gbtree",data = dtrain, nfold = 5,nrounds = 100, verbose = T,
                objective = "binary:logistic", eval_metric = "auc", nthread = 8, eta = 0.01,
                gamma = 0.0468, max_depth = 4, min_child_weight = 1.41, subsample = 0.769, colsample_bytree =0.283)

# View the variables that have the highest predictive power
mat <- xgb.importance(feature_names = colnames(dtrain), model = xgb)
dev.off() # to reset the plotting configurations
xgb.plot.importance(importance_matrix = mat[1:20])

# Create predictions using the model
prediction_xgb <- predict(xgb, newdata = dvalidate)
xgbpred <- ifelse (prediction_xgb > 0.5,1,0)

cm = table(validation$Target, xgbpred)
cm
Accuracy=(cm[1]+cm[4])/sum(cm)
Accuracy

##### AUC and ROC Curve #########
roc(xgbpred,as.numeric(validation$Target))
F1_Score(validation$Target, xgbpred, positive = NULL)



