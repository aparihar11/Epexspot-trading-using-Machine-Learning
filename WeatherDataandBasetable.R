install.packages('data.table')
library(data.table)

#Importing of 5-year weather trend CSV files for each 4 major cities of seperate geographical location

parisweather <- fread("C:\\Users\\John\\Desktop\\IESEG 2nd Semester\\Hackathon\\paris1419.csv")
marseilleweather <- fread("C:\\Users\\John\\Desktop\\IESEG 2nd Semester\\Hackathon\\marseille1419.csv")
toulouseweather <- fread("C:\\Users\\John\\Desktop\\IESEG 2nd Semester\\Hackathon\\toulouse1419.csv")
lyonweather <- fread("C:\\Users\\John\\Desktop\\IESEG 2nd Semester\\Hackathon\\lyon1419.csv")

#remove uneccessary columns 

parisweather <- parisweather[, -c(1, 3, 4, 5, 8, 9, 10, 12, 14, 16)]
marseilleweather <- marseilleweather[, -c(1, 3, 4, 5, 8, 9, 10, 12, 14, 16)]
toulouseweather <- toulouseweather[, -c(1, 3, 4, 5, 8, 9, 10, 12, 14, 16)] 
lyonweather <- lyonweather[, -c(1, 3, 4, 5, 8, 9, 10, 12, 14, 16)]  

#convert temperature from fahrenheit to celcius
install.packages('weathermetrics')
library(weathermetrics)

parisweather[, c("TAVG", "TMAX", "TMIN")] <- lapply(parisweather[, c("TAVG", "TMAX", "TMIN")], fahrenheit.to.celsius)
marseilleweather[, c("TAVG", "TMAX", "TMIN")] <- lapply(marseilleweather[, c("TAVG", "TMAX", "TMIN")], fahrenheit.to.celsius)
toulouseweather[, c("TAVG", "TMAX", "TMIN")] <- lapply(toulouseweather[, c("TAVG", "TMAX", "TMIN")], fahrenheit.to.celsius)
lyonweather[, c("TAVG", "TMAX", "TMIN")] <- lapply(lyonweather[, c("TAVG", "TMAX", "TMIN")], fahrenheit.to.celsius)

#Calculate temperature variance in single day
parisweather$TVAR = parisweather$TMAX - parisweather$TMIN
marseilleweather$TVAR = marseilleweather$TMAX - marseilleweather$TMIN
toulouseweather$TVAR = toulouseweather$TMAX - toulouseweather$TMIN
lyonweather$TVAR = lyonweather$TMAX - lyonweather$TMIN

#viewing summary of each city so far
summary(parisweather)
summary(marseilleweather)
summary(toulouseweather)
summary(lyonweather)

#for some cities there were multiple stations, so lets separate them
paris1 = parisweather[c(1:1821), ]
paris2 = parisweather[c(1821:3603), ]
marseille1 = marseilleweather[c(1:1821), ]
marseille2 = marseilleweather[c(1822:3555), ]
lyonweather = lyonweather[c(1:1821), ]

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

marseille1 = marseille1 %>%
  group_by(NAME) %>%
  arrange(DATE) %>%
  mutate(TDIFF = TAVG - lag(TAVG, default = first(TAVG)))

marseille2 = marseille2 %>%
  group_by(NAME) %>%
  arrange(DATE) %>%
  mutate(TDIFF = TAVG - lag(TAVG, default = first(TAVG)))

lyonweather = lyonweather %>%
  group_by(NAME) %>%
  arrange(DATE) %>%
  mutate(TDIFF = TAVG - lag(TAVG, default = first(TAVG)))

toulouseweather = toulouseweather %>%
  group_by(NAME) %>%
  arrange(DATE) %>%
  mutate(TDIFF = TAVG - lag(TAVG, default = first(TAVG)))

#We make a column for Significant
#Significant = 1 if next day temperature change is > +/-5 degrees than the day before, otherwise = 0

paris1$Significant = ifelse(paris1$TDIFF >= 5 | paris1$TDIFF <= -5, 1, 0)
paris2$Significant = ifelse(paris2$TDIFF >= 5 | paris2$TDIFF <= -5, 1, 0)
marseille1$Significant = ifelse(marseille1$TDIFF >= 5 | marseille1$TDIFF <= -5, 1, 0)
marseille2$Significant = ifelse(marseille2$TDIFF >= 5 | marseille2$TDIFF <= -5, 1, 0)
lyonweather$Significant = ifelse(lyonweather$TDIFF >= 5 | lyonweather$TDIFF <= -5, 1, 0)
toulouseweather$Significant = ifelse(toulouseweather$TDIFF >= 5 | toulouseweather$TDIFF <= -5, 1, 0)

#Reordering columns
paris1 = paris1[, c(1, 2, 3, 5, 6, 7, 4, 8, 9)]
paris2 = paris2[, c(1, 2, 3, 5, 6, 7, 4, 8, 9)]
marseille1 = marseille1[, c(1, 2, 3, 5, 6, 7, 4, 8, 9)]
marseille2 = marseille2[, c(1, 2, 3, 5, 6, 7, 4, 8, 9)]
toulouseweather = toulouseweather[, c(1, 2, 3, 5, 6, 7, 4, 8, 9)]
lyonweather = lyonweather[, c(1, 2, 3, 5, 6, 7, 4, 8, 9)]

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
         
         
         
      
