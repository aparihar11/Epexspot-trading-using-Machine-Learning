library(anytime)
library(timeDate)
library(lubridate)
library(dplyr)
library(readxl)
library(tidyr)
Sys.setenv(tz="Europe/Berlin")
Sys.getenv("tz")
t <- "01-01-14 09:40"
as.POSIXct(t, tz=getOption("tz"))

setwd("C:/Users/aparihar/Desktop/project")
basetable<-read.csv(file = "basetable.csv",header=TRUE)

basetable$DateTime1<-as.Date(basetable$DateTime, format="%d-%m-%y")
basetable$DateTime2<-weekdays(basetable$DateTime1)
basetable$weekday<-basetable$DateTime2
basetable$Category[basetable$weekday=="Saturday"] <- 1
basetable$Category[basetable$weekday=="Sunday"] <- 1
basetable$Category[basetable$weekday=="Monday"] <- 0
basetable$Category[basetable$weekday=="Tuesday"] <- 0
basetable$Category[basetable$weekday=="Wednesday"] <- 0
basetable$Category[basetable$weekday=="Thursday"] <- 0
basetable$Category[basetable$weekday=="Friday"] <- 0



holidays2014<-list("2014-01-01","2014-04-18","2014-04-21","2014-05-01","2014-05-08","2014-05-25","2014-05-29","2014-06-09","2014-06-15","2014-07-14","2014-08-15","2014-11-01","2014-11-11","2014-12-25","2014-12-26")
holidays2015<-list("2015-01-01","2015-04-03","2015-04-06","2015-05-01","2015-05-08","2015-05-14","2015-05-25","2015-05-31","2015-06-21","2015-07-14","2015-08-15","2015-11-01","2015-11-11","2015-12-25","2015-12-26")
holidays2016<-list("2016-01-01","2016-03-25","2016-03-28","2016-05-01","2016-05-05","2016-05-08","2016-05-16","2016-05-29","2016-06-19","2016-07-14","2016-08-15","2016-11-01","2016-11-11","2016-12-25","2016-12-26")
holidays2017<-list("2017-01-01","2017-04-14","2017-04-17","2017-05-01","2017-05-08","2017-05-25","2017-05-29","2017-06-05","2017-06-18","2017-07-14","2017-08-15","2017-11-01","2017-11-11","2017-12-25","2017-12-26")
holidays2018<-list("2018-01-01","2018-03-30","2018-04-02","2018-05-01","2018-05-08","2018-05-10","2018-05-21","2018-05-25","2018-05-27","2018-06-17","2018-07-14","2018-08-15","2018-11-01","2018-11-11","2018-12-25","2018-12-26")
holi<-c(holidays2014,holidays2015,holidays2016,holidays2017,holidays2018)
holi<-as.Date(holi)
out<-ymd(holi)
out<-as.data.frame(out)
out$Datehol<-out$out
out<-select(out, -out)
out$holiday<- "holiday"
out$DateTime1<-out$Datehol
out<-select(out, -Datehol)

basetable_holiday<-merge(x=basetable,y=out,by="DateTime1",all.x=TRUE)
basetable_holiday<-basetable_holiday %>% separate(DateTime, c("FromDate", "FromTime"), " ")
basetable_holiday<-basetable_holiday %>% separate(FromTime, c("Hour", "Min"), ":")
basetable_holiday<-select(basetable_holiday, -Min)
basetable_holiday<-basetable_holiday[order(basetable_holiday$DateTime1,basetable_holiday$X),]
basetable_holiday<-select(basetable_holiday,-c("FromDate","DateTime2"))
head(basetable_holiday)
write.csv(basetable_holiday,file="basetable-weekend-holiday.csv")
####rename colums for merging
colnames(basetable_holiday)[colnames(basetable_holiday)=="DateTime1"] <- "fromdate"
colnames(basetable_holiday)[colnames(basetable_holiday)=="Hour"] <- "fromtime"
colnames(basetable_holiday)[colnames(basetable_holiday)=="Category"] <- "Weekend"

basetable_holiday$Hour<-as.integer(basetable_holiday$Hour)
################ DATAMART########################################
datamart<-read.csv(file = "datamart.csv",header=TRUE)
datamart$fromdate<-as.Date(datamart$fromdate, format="%d/%m/%Y")
datamart_clean<-datamart[,c("fromdate","fromtime","Dayahead_Load.Forecast","ActualTotalLoad","DayaheadPrice._EUR_MWh","BasePrice")]


######MERGE COLUMNS#####
basetable_holiday_merge<-merge(x=basetable_holiday,y=datamart_clean,by=c("fromdate","fromtime"),all.x=TRUE)
basetable_holiday_merge<-select(basetable_holiday_merge, -weekday)
colnames(basetable_holiday_merge)[colnames(basetable_holiday_merge)=="Category"] <- "Weekend"


####holiday column  change to 1 and 0
basetable_holiday_merge$holiday<- ifelse(basetable_holiday_merge$holiday == "holiday",1,0)

basetable_holiday_merge$holiday[is.na(basetable_holiday_merge$holiday)] <- 0
write.csv(basetable_holiday_merge,file="basetable-full.csv")
###https://www.officeholidays.com/countries/france/2015.php"

