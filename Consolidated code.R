#######################################################################
### IESEG MBD Hackathon Group 8: Delaware Consulting ##################
### Consolidated R code file (pre-processing, basetable, modelling) ###
#######################################################################

#Gathering of weather data

install.packages('data.table')
library(data.table)

#Importing of 5-year weather trend CSV files for each Paris, FR

parisweather <- fread("C:\\Users\\John\\Desktop\\IESEG 2nd Semester\\Hackathon\\paris1419.csv")

#remove uneccessary columns 

parisweather <- parisweather[, -c(1, 3, 4, 5, 8, 9, 10, 12, 14, 16)]

#convert temperature from fahrenheit to celcius
install.packages('weathermetrics')
library(weathermetrics)

parisweather[, c("TAVG", "TMAX", "TMIN")] <- lapply(parisweather[, c("TAVG", "TMAX", "TMIN")], fahrenheit.to.celsius)

#Calculate temperature variance in single day
parisweather$TVAR = parisweather$TMAX - parisweather$TMIN

#viewing summary of each city so far
summary(parisweather)

#In Paris, FR there are multiple stations, so lets separate them
paris1 = parisweather[c(1:1821), ]
paris2 = parisweather[c(1821:3603), ]

#new column for average temperature difference per day
install.packages('dplyr')
library(dplyr)

paris1 = paris1 %>%
  group_by(NAME) %>%
  arrange(DATE) %>%
  mutate(TDIFF = TAVG - lag(TAVG, default = first(TAVG)))

paris2 = paris2 %>%
  group_by(NAME) %>%
  arrange(DATE) %>%
  mutate(TDIFF = TAVG - lag(TAVG, default = first(TAVG)))

#We make a column for Significant
#Significant = 1 if next day temperature change is > +/-5 degrees than the day before, otherwise = 0

paris1$Significant = ifelse(paris1$TDIFF >= 5 | paris1$TDIFF <= -5, 1, 0)
paris2$Significant = ifelse(paris2$TDIFF >= 5 | paris2$TDIFF <= -5, 1, 0)

#Reordering columns
paris1 = paris1[, c(1, 2, 3, 5, 6, 7, 4, 8, 9)]
paris2 = paris2[, c(1, 2, 3, 5, 6, 7, 4, 8, 9)]

#Adding column to make 24 rows per day (to match with Intraday)
paris1$Periods =  rep(24,nrow(paris1))

#Expanding rows of weather data to get 24 rows pr day, one for each hour
install.packages('splitstackshape')
library(splitstackshape)
paris1 = expandRows(paris1, "Periods")

#Loading of Intraday dataset
intraday = fread('C:\\Users\\John\\Desktop\\IESEG 2nd Semester\\Hackathon\\IntradayContinuousEPEXSPOT.csv')
colnames(intraday)[colnames(intraday) == 'Date'] <- 'DATE'

#Merging with Paris1 weather data
intraday2 = cbind(intraday, paris1[!names(paris1) %in% names(intraday)])
intraday2$DATE = NULL
intraday2$time = NULL
intraday2$NAME = NULL

names(intraday)
names(paris1)

#write CSV
write.csv(intraday2, "intradaywithweather.csv")

#map time of day to certain temperature variable (TPRED aka predicted temperature for that hour)
#Middle Night (01-04) = #min temp 
#Early Morning (05-08) = #min temp
#Late Morning (09-12) = #avg temp
#Early Afternoon (13-16) = #max temp
#Rush Hour (17-20) = #avg temp
#Off-Peak 2 (21-24) = #min temp

install.packages('lubridate')
library(lubridate)
library(dplyr)

intraday2 = fread('C:\\Users\\John\\Desktop\\IESEG 2nd Semester\\Hackathon\\intradaywithweather.csv')

intraday2$V1 = NULL
intraday2$hour = hour(intraday2$DateTime)

intraday2$TPRED[intraday2$hour==1 | intraday2$hour==2 | intraday2$hour==3 | intraday2$hour==4] = 1
intraday2$TPRED[intraday2$hour==5 | intraday2$hour==6 | intraday2$hour==7 | intraday2$hour==8] = 2
intraday2$TPRED[intraday2$hour==9 | intraday2$hour==10 | intraday2$hour==11 | intraday2$hour==12] = 3
intraday2$TPRED[intraday2$hour==13 | intraday2$hour==14 | intraday2$hour==15 | intraday2$hour==16] = 4
intraday2$TPRED[intraday2$hour==17 | intraday2$hour==18 | intraday2$hour==19 | intraday2$hour==20] = 5
intraday2$TPRED[intraday2$hour==21 | intraday2$hour==22 | intraday2$hour==23 | intraday2$hour==0] = 6

intraday2 = intraday2 %>%
  mutate(TPRED = ifelse(TPRED == 1, TMIN, ifelse(TPRED == 2, TMIN, ifelse(TPRED == 3, TAVG,
                 ifelse(TPRED == 4, TMAX, ifelse(TPRED == 5, TAVG, ifelse(TPRED == 6, TMIN, NA)))))))
      
#Removing uneccessary columns and renaming basetable
intraday2$hour = NULL
basetable = intraday2

#write CSV of basetable
write.csv(basetable, "basetable.csv")

#create holiday and weekend variables

#import neccessary packages
install.packages('anytime')
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


class(holi)
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

basetable = fread('C:\\Users\\John\\Desktop\\IESEG 2nd Semester\\Hackathon\\basetable-full.txt')

basetable$V1 = NULL
basetable$X = NULL

#start data from January 6th, 2015
basetable = basetable[8881:43968,]

#replacing NAs with mean values (or 0)
summary(basetable)

basetable$Last[is.na(basetable$Last)] <- 42.84
basetable$PRCP[is.na(basetable$PRCP)] <- 0.07
basetable$TMAX[is.na(basetable$TMAX)] <- 17.01
basetable$TMIN[is.na(basetable$TMIN)] <- 8.18
basetable$TVAR[is.na(basetable$TVAR)] <- 8.75
basetable$TAVG[is.na(basetable$TAVG)] <- 12.41
basetable$TDIFF[is.na(basetable$TDIFF)] <- 0
basetable$Significant[is.na(basetable$Significant)] <- 0
basetable$TPRED[is.na(basetable$TPRED)] <- 11.15
basetable$ActualTotalLoad[is.na(basetable$ActualTotalLoad)] <- 54014
basetable$DayaheadPrice._EUR_MWh[is.na(basetable$DayaheadPrice._EUR_MWh)] <- 42.57

#create variable for Direct SUn Hours
library(lubridate)

basetable$fromdate = as.Date(basetable$fromdate, "%m/%d/%Y")
basetable$month = month(basetable$fromdate)

basetable$DirSunHours[basetable$month==1] = 2.1
basetable$DirSunHours[basetable$month==2] = 2.8
basetable$DirSunHours[basetable$month==3] = 4.3
basetable$DirSunHours[basetable$month==4] = 5.5
basetable$DirSunHours[basetable$month==5] = 6.5
basetable$DirSunHours[basetable$month==6] = 6.7
basetable$DirSunHours[basetable$month==7] = 7.1
basetable$DirSunHours[basetable$month==8] = 7.1
basetable$DirSunHours[basetable$month==9] = 5.6
basetable$DirSunHours[basetable$month==10] = 3.9
basetable$DirSunHours[basetable$month==11] = 2.3
basetable$DirSunHours[basetable$month==12] = 1.7

basetable$month = NULL
basetable$ActualTotalLoad = NULL
basetable$BasePrice = NULL

#create variable for price difference and target
basetable$Last <- ifelse(basetable$Last < 0, 0, basetable$Last)
summary(basetable$Last)

basetable$PriceDIFF = basetable$DayaheadPrice._EUR_MWh - basetable$Last
basetable$DayaheadPrice._EUR_MWh <- ifelse(basetable$DayaheadPrice._EUR_MWh < 0, 0, basetable$DayaheadPrice._EUR_MWh)

basetable$Target=ifelse(basetable$PriceDIFF>sd(basetable$PriceDIFF),1,0)
table(basetable$Target)

#Renaming Last column
colnames(basetable)[3] <- "IntradayPrice"

#save basetable at this point
write.csv(basetable,file="basetableapril14.csv")

