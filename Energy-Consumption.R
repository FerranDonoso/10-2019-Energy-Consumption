## Load libraries
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(lattice, ggplot2, caret, readr, corrplot, reshape2, caTools, dplyr, RMySQL, lubridate,
               tidyr, ggfortify, forecast, plotly, fracdiff, tseries, devtools, vars, xts, astsa,
               ggpubr, prophet, matrixStats)

## Create a database connection 
con = dbConnect(MySQL(), user = "deepAnalytics", password = "Sqltask1234!",
                dbname = "dataanalytics2018",
                host = "data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com")

## Initial understanding of the data set
dbListFields(con,"yr_2006")
iu2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
View(iu2007)
which(is.na(iu2007))

rm("iu2007")

poa2006 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,Sub_metering_1,
                      Sub_metering_2, Sub_metering_3, Voltage FROM yr_2006")
poa2007 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,Sub_metering_1,
                      Sub_metering_2, Sub_metering_3, Voltage FROM yr_2007")
poa2008 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,Sub_metering_1,
                      Sub_metering_2, Sub_metering_3, Voltage FROM yr_2008")
poa2009 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,Sub_metering_1,
                      Sub_metering_2, Sub_metering_3, Voltage FROM yr_2009")
poa2010 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,Sub_metering_1,
                      Sub_metering_2, Sub_metering_3, Voltage FROM yr_2010")

#2007, 2008, 2009 entire year
str(poa2006)
summary(poa2006)
head(poa2006)
tail(poa2006)

## Combine tables into one dataframe
poa07to10 <- bind_rows(poa2007, poa2008, poa2009, poa2010)
rm("poa2006", "poa2007", "poa2008", "poa2009", "poa2010")

View(poa07to10)
str(poa07to10)
summary(poa07to10)
head(poa07to10)
tail(poa07to10)

## Combine Date and Time attribute values in a new attribute column
poa07to10dt <- cbind(poa07to10, paste(poa07to10$Date, poa07to10$Time), stringsAsFactors = FALSE)
colnames(poa07to10dt)[8] <- "DateTime"

## Move the DateTime attribute within the dataset
poa07to10dt <- poa07to10dt[,c(ncol(poa07to10dt), 1:(ncol(poa07to10dt)-1))]

## Convert DateTime from POSIXlt to POSIXct 
poa07to10dt$DateTime <- as.POSIXct(poa07to10dt$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(poa07to10dt$DateTime, "tzone") <- "UTC"
str(poa07to10dt)

#repeat lines 62 and 65 if the changes have not been made
poa07to10dt$DateTime <- as.POSIXct(poa07to10dt$DateTime, "%Y/%m/%d %H:%M:%S")
attr(poa07to10dt$DateTime, "tzone") <- "UTC"

## Create time granularity attributes
poa07to10dt$year <- year(poa07to10dt$DateTime)
poa07to10dt$month <- month(poa07to10dt$DateTime)
poa07to10dt$week <- week(poa07to10dt$DateTime)
poa07to10dt$weekdays <- weekdays(poa07to10dt$DateTime)
poa07to10dt$day <- day(poa07to10dt$DateTime)
poa07to10dt$hour <- hour(poa07to10dt$DateTime)
poa07to10dt$minute <- minute(poa07to10dt$DateTime)

## Exploratory analysis
## Boxplots of the 3 sub meters
boxplot(poa07to10dt$Sub_metering_1)
boxplot(poa07to10dt$Sub_metering_3)
boxplot(poa07to10dt$Sub_metering_2)

## Histograms of the 3 sub meters, global active power and voltage
a <- hist(poa07to10dt$Sub_metering_1)
text(a$mids, a$counts, labels = a$counts, adj = c(0.5, -0.5))

b <- hist(poa07to10dt$Sub_metering_2)
text(b$mids, b$counts, labels = b$counts, adj = c(0.5, -0.5))

c <- hist(poa07to10dt$Sub_metering_3, ylim = c(0, 1500000))
text(c$mids, c$counts, labels = c$counts, adj = c(0.5, -0.5))

d <- hist(poa07to10dt$Global_active_power, ylim = c(0, 1200000))
text(d$mids, d$counts, labels = d$counts, adj = c(0.5, -0.5))

e <- hist(poa07to10dt$Voltage, ylim = c(0, 600000))
text(e$mids,e$counts, labels = e$counts, adj = c(0.5, -0.5))

## Remove temporal data frames
rm("a", "b", "c", "d", "e")
gc()

## Histograms of the 3 sub meters without outliers
poa07to10dtout1 <- poa07to10dt[poa07to10dt$Sub_metering_1 > 1, ]
poa07to10dtout2 <- poa07to10dt[poa07to10dt$Sub_metering_2 > 1, ]
poa07to10dtout3 <- poa07to10dt[poa07to10dt$Sub_metering_3 > 1, ]

aa <- hist(poa07to10dtout1$Sub_metering_1)
text(aa$mids, aa$counts, labels = aa$counts, adj = c(0.5, -0.5))

bb <- hist(poa07to10dtout2$Sub_metering_2)
text(bb$mids, bb$counts, labels = bb$counts, adj = c(0.5, -0.5))

cc <- hist(poa07to10dtout3$Sub_metering_3)
text(cc$mids, cc$counts, labels = cc$counts, adj = c(0.5, -0.5))

rm("aa", "bb", "cc", "poa07to10dtout1", "poa07to10dtout2", "poa07to10dtout3")

## Group energy by year, month, day, hour ####
energyhour <- poa07to10dt %>%
  group_by(year, month, day, hour) %>% 
  summarise(Sub_metering_1hg = sum(Sub_metering_1, na.rm = TRUE),
            Sub_metering_2hg = sum(Sub_metering_2, na.rm = TRUE),
            Sub_metering_3hg = sum(Sub_metering_3, na.rm = TRUE),
            Global_active_powerhg = sum(Global_active_power, na.rm = TRUE))
View(energyhour)

## Convert submeters watt hour into kilowatt hour
energyhour <- mutate(energyhour,Sub_metering_1kwh = Sub_metering_1hg / 1000)
energyhour <- mutate(energyhour,Sub_metering_2kwh = Sub_metering_2hg / 1000)
energyhour <- mutate(energyhour,Sub_metering_3kwh = Sub_metering_3hg / 1000)

## Convert global active power kilowatt minute into kilowatt hour
energyhour <- mutate(energyhour, Global_active_powerkwh = Global_active_powerhg / 60)

## Plot energy hour
str(energyhour)
energyhour$hour <- as.factor(energyhour$hour)

plot(energyhour$hour, energyhour$Sub_metering_3kwh,
     ylab = "Submeter 3 consumption",
     xlab = "Hour of the day",
     main = "Submeter 3 consumption by Hour of the day",
     cex.lab = 1.4,
     cex.main = 1.4,
     cex.axis = 1.4)

plot(energyhour$hour, energyhour$Sub_metering_2kwh,
     ylab = "Submeter 2 consumption",
     xlab = "Hour of the day",
     main = "Submeter 2 consumption by Hour of the day",
     cex.lab = 1.4,
     cex.main = 1.4,
     cex.axis = 1.4)

plot(energyhour$hour, energyhour$Sub_metering_1kwh,
     ylab = "Submeter 1 consumption",
     xlab = "Hour of the day",
     main = "Submeter 1 consumption by Hour of the day",
     cex.lab = 1.4,
     cex.main = 1.4,
     cex.axis = 1.4)

plot(energyhour$hour, energyhour$Global_active_powerkwh,
     ylab = "Global active power consumption",
     xlab = "Hour of the day",
     main = "Global active power consumption by Hour of the day",
     cex.lab = 1.4,
     cex.main = 1.4,
     cex.axis = 1.4)

energyhour$hour <- as.integer(energyhour$hour)

## Group energy by year and month ####
energymonth <- poa07to10dt %>%
  group_by(year, month) %>% 
  summarise(Sub_metering_1mg = sum(Sub_metering_1, na.rm = TRUE),
            Sub_metering_2mg = sum(Sub_metering_2, na.rm = TRUE),
            Sub_metering_3mg = sum(Sub_metering_3, na.rm = TRUE),
            Global_active_powermg = sum(Global_active_power, na.rm = TRUE))

## Convert submeters watt hour into kilowatt hour
energymonth <- mutate(energymonth,Sub_metering_1kwh = Sub_metering_1mg / 1000)
energymonth <- mutate(energymonth,Sub_metering_2kwh = Sub_metering_2mg / 1000)
energymonth <- mutate(energymonth,Sub_metering_3kwh = Sub_metering_3mg / 1000)

## Convert global active power kilowatt minute into kilowatt hour
energymonth <- mutate(energymonth, Global_active_powerkwh = Global_active_powermg / 60)

## Plot energy month
plot(energymonth$month, energymonth$Sub_metering_3kwh,
     ylab = "kilowatt hour consumption",
     xlab = "",
     col = "deepskyblue4",
     cex.lab = 1.4,
     pch = c(19),
     ylim = c(0, 1500),
     xlim = c(1, 20),
     xaxt = "n",
     yaxt = "n",
     frame.plot = FALSE)
points(energymonth$month, energymonth$Sub_metering_2kwh, col = "turquoise3", pch = c(19))
points(energymonth$month, energymonth$Sub_metering_1kwh, col = "tomato1", pch = c(19))
points(energymonth$month, energymonth$Global_active_powerkwh, col = "yellow3", pch = c(19))
title("Energy Consumption by Month",
      adj = 0.25,
      line = 2,
      cex.main = 1.4)
legend(14, 1500,
       pch = c(19),
       col = c("yellow3", "tomato1", "turquoise3", "deepskyblue4"),
       c("Global active power", "Sub Meter 1", "Sub Meter 2", "Sub Meter 3"),
       box.lty = 0,
       cex = 1.4)
axis(1, at = 1:12, cex.axis = 1.4,
     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
axis(2, at = seq(0, 1500, by = 250), cex.axis = 1.4,)

## Energy consumption by month ####
energymonthg <- energymonth %>%
  group_by(month) %>% 
  summarise(Sub_metering_1kwhg = sum(Sub_metering_1kwh, na.rm = TRUE),
            Sub_metering_2kwhg = sum(Sub_metering_2kwh, na.rm = TRUE),
            Sub_metering_3kwhg = sum(Sub_metering_3kwh, na.rm = TRUE),
            Global_active_powerkwhg = sum(Global_active_powerkwh, na.rm = TRUE))
View(energymonthg)

## Energy by month plot
plot_ly(energymonthg, x = ~energymonthg$month, y = ~energymonthg$Sub_metering_1kwhg,
        name = "Kitchen", type = "scatter", mode = "lines") %>%
  add_trace(y = ~energymonthg$Sub_metering_2kwhg, name = "Laundry Room", mode = "lines") %>%
  add_trace(y = ~energymonthg$Sub_metering_3kwhg, name = "Water Heater & AC", mode = "lines") %>%
  add_trace(y = ~energymonthg$Global_active_powerkwhg, name = "Entire House", mode = "lines") %>%
  layout(title = "Power Consumption by month 2007-08-09-10",
         yaxis = list (title = "Power (kilowatt-hours)"),
         xaxis = list(title = "", ticktext = list("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                  "Aug", "Sep", "Oct", "Nov", "Dec"), 
                      tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), tickmode = "array"))

## Energy usage pie chart
energymonthg <- mutate(energymonthg,Other_kwh = Global_active_powerkwhg -
                         Sub_metering_3kwhg - Sub_metering_2kwhg - Sub_metering_1kwhg)

energymonthgg <- energymonthg %>%
  gather(SubMeter, kwh, Sub_metering_1kwhg, Sub_metering_2kwhg,
         Sub_metering_3kwhg, Other_kwh)

plot_ly(energymonthgg, labels = ~SubMeter, values = ~kwh, type = "pie") %>%
  layout(title = "submeter usage 2007-08-09-10",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

rm("energymonthgg", "energymonthg")



#### Explore time granularity
## Subset a sample week - All Observations
houseWeek <- filter(poa07to10dt, year == 2008 & week == 2)

## Plot subset houseWeek
plot_ly(houseWeek, x = ~houseWeek$DateTime, y = ~houseWeek$Sub_metering_1,
        type = "scatter", mode = "lines")

## Subset a sample day - All observations
houseDay <- filter(poa07to10dt, year == 2008 & month == 1 & day == 9)

## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1,
        type = "scatter", mode = "lines")

## Plot sub-meter 1, 2 and 3 - All observations 
plot_ly(houseDay,
        x = ~houseDay$DateTime,
        y = ~houseDay$Sub_metering_1,
        name = "Kitchen",
        type = "scatter",
        mode = "lines") %>%
  add_trace(y = ~houseDay$Sub_metering_2,
            name = "Laundry Room",
            mode = "lines") %>%
  add_trace(y = ~houseDay$Sub_metering_3,
            name = "Water Heater & AC",
            mode = "lines") %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset sample day - 10 Minute frequency
houseDay10 <- filter(poa07to10dt, year == 2008 & month == 1 & day == 9 &
                       (minute == 0 | minute == 10 | minute == 20 |
                          minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10,
        x = ~houseDay10$DateTime,
        y = ~houseDay10$Sub_metering_1,
        name = "Kitchen",
        type = "scatter",
        mode = "lines") %>%
  add_trace(y = ~houseDay10$Sub_metering_2,
            name = "Laundry Room",
            mode = "lines") %>%
  add_trace(y = ~houseDay10$Sub_metering_3,
            name = "Water Heater & AC",
            mode = "lines") %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Plot sub-meter 1, 2 and 3 - Subset the second week of 2008 by hour
houseWeekHour <- filter(poa07to10dt, year == 2008 & week == 2
                        & (hour == 1 | hour == 2 | hour == 3 | hour == 4 | hour == 5 | hour == 6 |
                             hour == 7 | hour == 8 | hour == 9 | hour == 10 | hour == 11 | hour == 12 |
                             hour == 13 | hour == 14 | hour == 15 | hour == 16 | hour == 17 |
                             hour == 18 | hour == 19 | hour == 20 | hour == 21 | hour == 22 |
                             hour == 23 | hour == 00))

plot_ly(houseWeekHour,
        x = ~houseWeekHour$DateTime,
        y = ~houseWeekHour$Sub_metering_1,
        name = "Kitchen",
        type = "scatter",
        mode = "lines") %>%
  add_trace(y = ~houseWeekHour$Sub_metering_2,
            name = "Laundry Room",
            mode = "lines") %>%
  add_trace(y = ~houseWeekHour$Sub_metering_3,
            name = "Water Heater & AC",
            mode = "lines") %>%
  layout(title = "Power Consumption second week, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

rm("houseDay", "houseDay10", "houseWeek", "houseWeekHour")
gc()

## Time Series Forecasting
##### train and test set #####
training <- energymonth %>% filter(year == 2007 | year == 2008 | year == 2009)
testing <- energymonth %>% filter(year == 2010)

## Create TS training and testing energy month 07-10
trainingTS <- ts(training$Global_active_powerkwh, frequency=12, start=c(2007,1))
autoplot(trainingTS)
testingTS <- ts(testing$Global_active_powerkwh, frequency=11, start=c(2010,1))
autoplot(testingTS)

## Decompose into trend, seasonal and remainder
CtrainingTS <- decompose(trainingTS)
plot(CtrainingTS)

## Seasonal adjusting by subtracting the seasonal component & plot
CtrainingTSAdjusted <- trainingTS - CtrainingTS$seasonal
autoplot(CtrainingTSAdjusted)

## Test Seasonal Adjustment by running Decompose again.
## Note the very, very small scale for Seasonal
plot(decompose(CtrainingTSAdjusted))

rm("CtrainingTS", "CtrainingTSAdjusted")

## Linear regression forecast & plot to monthly 07-10 global active power ####
TrainingTSlm <- tslm(trainingTS ~ trend + season) 
forecastTrainingTSlm <- forecast(TrainingTSlm, h = 12, level = c(80, 90))
plot(forecastTrainingTSlm, fcol = "deepskyblue4", ylab = "kiloWatt-Hours",
     xlab = "Time - Global Active Power")
lines(testingTS, col = "gold3", lwd = 2)

accuracy(forecastTrainingTSlm, testingTS)
checkresiduals(forecastTrainingTSlm)
checkresiduals(testingTS)

## HoltWinters forecast & plot to monthly 07-10 global active power ####
TrainingTShw <- HoltWinters(trainingTS, beta = FALSE, gamma = TRUE)
forecastTrainingTShw <- forecast(TrainingTShw, h = 12, level = c(80, 90))
plot(forecastTrainingTShw, fcol = "deepskyblue4", ylab = "kiloWatt-Hours",
     xlab = "Time - Global Active Power")
lines(testingTS, col = "gold3", lwd = 2)

accuracy(forecastTrainingTShw, testingTS)
checkresiduals(forecastTrainingTShw)
checkresiduals(testingTS)

## Arima check for stationarity, forecast & plot to monthly 07-10 global active power ####
station <- function(ts) {
  if (adf.test(ts)$p.value > 0.05) {
    print("The time series is not stationary, the ADF test has not been passed")
    sprintf("The p-value is: %5.3f", adf.test(ts)$p.value)
    ggAcf(ts)
  } else {
    print("This time series is stationary, the ADF test has been passed")
    ggAcf(ts)
  }
}

station(trainingTS)

trainingTSd1 <- diff(trainingTS)
station(trainingTSd1)

trainingTSd2 <- diff(trainingTSd1)
station(trainingTSd2)

trainingTSd3 <- diff(trainingTSd2)
station(trainingTSd3) #diff 3

Acf(trainingTSd3)
Pacf(trainingTSd3)

TrainingTSar <- arima(trainingTS, order = c(3, 3, 3))
forecastTrainingTSar <- forecast(TrainingTSar, h = 12, level = c(80, 90))
plot(forecastTrainingTSar, fcol = "deepskyblue4", ylab = "kiloWatt-Hours",
     xlab = "Time - Global Active Power")
lines(testingTS, col = "gold3", lwd = 2)

accuracy(forecastTrainingTSar, testingTS)
checkresiduals(forecastTrainingTSar)
checkresiduals(testingTS)

## Autoa arima check for stationarity, forecast & plot to monthly 07-10 global active power ####
TrainingTSaa <- auto.arima(trainingTS, stepwise = FALSE, approximation = FALSE)
forecastTrainingTSaa <- forecast(TrainingTSaa, h = 12, level = c(80, 90))
plot(forecastTrainingTSaa, fcol = "deepskyblue4", ylab = "kiloWatt-Hours",
     xlab = "Time - Global Active Power")
lines(testingTS, col = "gold3", lwd = 2)

accuracy(forecastTrainingTSaa, testingTS)
checkresiduals(forecastTrainingTSaa)
checkresiduals(testingTS)

## Prophet forecast & plot to monthly 07-10 global active power ####
## Create prophet object
trainingPR <- training
trainingPR <- trainingPR[, -c(3:9)]

trainingPR$ds <- as.yearmon(paste(trainingPR$year, trainingPR$month, sep = "-"))

trainingPR <- trainingPR[, -c(1:2)]

trainingPR <- trainingPR[, c(2,1)]

colnames(trainingPR)[2] <- "y"

trainingPR$ds <- as.Date(trainingPR$ds)

## Prophet forecast & plot
TrainingTSpr <- prophet(trainingPR)
futureTrainingTSpr <- make_future_dataframe(TrainingTSpr, periods = 365)
forecastTrainingTSpr <- predict(TrainingTSpr, futureTrainingTSpr)
plot(TrainingTSpr, forecastTrainingTSpr)
dyplot.prophet(TrainingTSpr, forecastTrainingTSpr)

prophet_plot_components(TrainingTSpr, forecastTrainingTSpr)

Prophet_errors <- cross_validation(TrainingTSpr, initial = 36, period = 6, horizon = 12, units = "days")
performance_metrics(Prophet_errors)

rm("TrainingTSlm", "forecastTrainingTSlm", "TrainingTShw", "forecastTrainingTShw", "trainingTSd1",
   "trainingTSd2", "trainingTSd3", "TrainingTSar", "forecastTrainingTSar", "TrainingTSaa",
   "forecastTrainingTSaa", "trainingPR", "TrainingTSpr", "futureTrainingTSpr",
   "forecastTrainingTSpr", "Prophet_errors")
gc()

## Predict 2011 with linear model ####
# Global Active Power
energymonthTS <- ts(energymonth$Global_active_powerkwh, frequency = 12, start = c(2007, 1))
energymonthTSlm <- tslm(energymonthTS ~ trend + season) 
forecastenergymonthTSlm <- forecast(energymonthTSlm, h = 13, level = c(80, 90))
plot(forecastenergymonthTSlm, fcol = "deepskyblue4", ylab = "kiloWatt-Hours",
     xlab = "Time - Global Active Power")
plot(forecastenergymonthTSlm, fcol = "deepskyblue4", ylab = "kiloWatt-Hours",
     xlab = "Time - Global Active Power", start(2010))
summary(forecastenergymonthTSlm)

## Sub Meter 1
energymonthTSs1 <- ts(energymonth$Sub_metering_1kwh, frequency = 12, start = c(2007, 1))
energymonthTSs1lm <- tslm(energymonthTSs1 ~ trend + season) 
forecastenergymonthTSs1lm <- forecast(energymonthTSs1lm, h = 13, level = c(80, 90))
plot(forecastenergymonthTSs1lm, fcol = "deepskyblue4", ylab = "kiloWatt-Hours",
     xlab = "Time - Global Active Power")
plot(forecastenergymonthTSs1lm, fcol = "deepskyblue4", ylab = "kiloWatt-Hours",
     xlab = "Time - Global Active Power", start(2010))
summary(forecastenergymonthTSs1lm)

## Sub Meter 2
energymonthTSs2 <- ts(energymonth$Sub_metering_2kwh, frequency = 12, start = c(2007, 1))
energymonthTSs2lm <- tslm(energymonthTSs2 ~ trend + season) 
forecastenergymonthTSs2lm <- forecast(energymonthTSs2lm, h = 13, level = c(80, 90))
plot(forecastenergymonthTSs2lm, fcol = "deepskyblue4", ylab = "kiloWatt-Hours",
     xlab = "Time - Global Active Power")
plot(forecastenergymonthTSs2lm, fcol = "deepskyblue4", ylab = "kiloWatt-Hours",
     xlab = "Time - Global Active Power", start(2010))
summary(forecastenergymonthTSs2lm)

## Sub Meter 3
energymonthTSs3 <- ts(energymonth$Sub_metering_3kwh, frequency = 12, start = c(2007, 1))
energymonthTSs3lm <- tslm(energymonthTSs3 ~ trend + season) 
forecastenergymonthTSs3lm <- forecast(energymonthTSs3lm, h = 13, level = c(80, 90))
plot(forecastenergymonthTSs3lm, fcol = "deepskyblue4", ylab = "kiloWatt-Hours",
     xlab = "Time - Global Active Power")
plot(forecastenergymonthTSs3lm, fcol = "deepskyblue4", ylab = "kiloWatt-Hours",
     xlab = "Time - Global Active Power", start(2010))
summary(forecastenergymonthTSs3lm)

rm("energymonthTS", "energymonthTSlm", "forecastenergymonthTSlm",
   "energymonthTSs1", "energymonthTSs1lm", "forecastenergymonthTSs1lm",
   "energymonthTSs2", "energymonthTSs2lm", "forecastenergymonthTSs2lm",
   "energymonthTSs3", "energymonthTSs3lm", "forecastenergymonthTSs3lm")

## Additional data for dashboard ####
energymonthpbi <- energymonth
energymonthpbi$ID <- seq.int(nrow(energymonthpbi))
energymonthpbi <- mutate(energymonthpbi, Other_kwh = Global_active_powerkwh -
                           Sub_metering_3kwh - Sub_metering_2kwh - Sub_metering_1kwh)
energymonthpbi <- energymonthpbi[, -c(3:6)]
names(energymonthpbi) <- c("Year", "Month", "Kitchen", "Laundry", "Climatization",
                           "Global", "ID", "Other")

energymonthpbi$Date <- as.yearmon(paste(energymonthpbi$Year, energymonthpbi$Month, sep = "-"))
energymonthpbi$Date <- as.Date(energymonthpbi$Date)

energymonthpbi <- energymonthpbi[, c(7, 9, 1, 2, 3, 4, 5, 6, 8)]

energymonthpbi <- energymonthpbi %>% 
  group_by(Month) %>% 
  mutate(Kitchen_Average = mean(Kitchen))

energymonthpbi <- energymonthpbi %>% 
  group_by(Month) %>% 
  mutate(Laundry_Average = mean(Laundry))

energymonthpbi <- energymonthpbi %>% 
  group_by(Month) %>% 
  mutate(Climatization_Average = mean(Climatization))

energymonthpbi <- energymonthpbi %>% 
  group_by(Month) %>% 
  mutate(Global_Average = mean(Global))

energymonthpbi <- energymonthpbi %>% 
  group_by(Month) %>% 
  mutate(Other_Average = mean(Other))

energymonthpbi <- mutate(energymonthpbi, Total_Cost = Global * 0.1472)

energymonthpbi <- mutate(energymonthpbi, Savings = ((Global - Global_Average) / Global_Average) * -1)

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(1 * x, format = format, digits = digits, ...), "%")
}

energymonthpbi[nrow(energymonthpbi) + 1, ] = c("48", "2010-12-01", "2010", "12", "52.19732", "42.04363",
                                               "391.6279", "1026.9136", "541.0448", "58.99933",
                                               "58.62667", "351.2260", "1057.4616", "588.6096",
                                               "151.1617", "0.02888805")

getwd()
write.csv(energymonthpbi, file = "energymonth0710pbi2.csv")

## power bi forecast
forecast = data.frame(Forecast = as.numeric(forecastenergymonthTSlm$mean),
                      Hi80 = as.numeric(forecastenergymonthTSlm$upper[, 1]),
                      Hi90 = as.numeric(forecastenergymonthTSlm$upper[, 2]),
                      Lo80 = as.numeric(forecastenergymonthTSlm$lower[, 1]),
                      Lo90 = as.numeric(forecastenergymonthTSlm$lower[, 2]),
                      Date = seq(as.Date("2010-12-01"), as.Date("2011-12-01"), by = "1 month"))

forecast <- forecast[-c(1), ]
forecast$ID <- seq.int(nrow(forecast))
forecast <- forecast[, c(7, 6, 1, 4, 2, 5, 3)]

write.csv(forecast, file = "forecastmonth0710pbi.csv")

rm("forecastenergymonthTSlm")









