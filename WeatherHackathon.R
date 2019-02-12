### Group 8 Hackathon: Weather Data ###
### Jan 2015 - Dec 2018: Paris (N), Marseille (S), Toulouse (W), Lyon (E) ###

library(data.table)

#Importing of 5-year weather trend CSV files for each 4 major cities of seperate geographical location

parisweather <- fread("C:\\Users\\pdundon\\Desktop\\Hackathon\\parisweather.csv")
marseilleweather <- fread("C:\\Users\\pdundon\\Desktop\\Hackathon\\marseilleweather.csv")
toulouseweather <- fread("C:\\Users\\pdundon\\Desktop\\Hackathon\\toulouseweather.csv")
lyonweather <- fread("C:\\Users\\pdundon\\Desktop\\Hackathon\\lyonweather.csv")

#remove uneccessary columns 

parisweather <- parisweather[, -c(1, 9, 10)]
marseilleweather <- marseilleweather[, -c(1, 9, 10)]
toulouseweather <- toulouseweather[, -c(1, 9, 10)] 
lyonweather <- lyonweather[, -c(1, 9, 10)]  

#convert temperature from fahrenheit to celcius
install.packages('weathermetrics')
library(weathermetrics)

parisweather[, c("TAVG", "TMAX", "TMIN")] <- lapply(parisweather[, c("TAVG", "TMAX", "TMIN")], fahrenheit.to.celsius)
marseilleweather[, c("TAVG", "TMAX", "TMIN")] <- lapply(marseilleweather[, c("TAVG", "TMAX", "TMIN")], fahrenheit.to.celsius)
toulouseweather[, c("TAVG", "TMAX", "TMIN")] <- lapply(toulouseweather[, c("TAVG", "TMAX", "TMIN")], fahrenheit.to.celsius)
lyonweather[, c("TAVG", "TMAX", "TMIN")] <- lapply(lyonweather[, c("TAVG", "TMAX", "TMIN")], fahrenheit.to.celsius)


