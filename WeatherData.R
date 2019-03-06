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

#Loading of Intraday dataset
intraday = fread('C:\\Users\\John\\Desktop\\IESEG 2nd Semester\\Hackathon\\IntradayContinuousEPEXSPOT.csv')
colnames(intraday)[colnames(intraday) == 'Date'] <- 'DATE'

#Merging with Paris1 weather data
#intraday2 = merge(x = intraday, y = paris1, by = "DATE", all = TRUE)

#paris1$DATE <- as.Date(paris1$DATE)
#intraday$TAVG <- sapply(as.Date(intraday$DateTime), function(x) paris1[paris1$DATE==x, "TAVG"])


