# Advance Time Series 

# What is time Series ? 
# Study/prediction/forecast of any variable ( Sales/Pressure/Vibration/Visitor/Diseases)
# across time ( year, months, days, hours, mins, secs)

# Components of a time-series ?
# TCS-I ( Trend, Cycle, Season and Irregular)

# Analysis the Stock price of J&J 
mydata <- read.csv("C:\\JS\\TimeSeries\\TS_Advance\\HistoricalQuotes.csv")
dim(mydata)
View(mydata)
# Lets take a look at the structure of dataset 
str(mydata)

# Date column in this dataframe is actually not the date columns
class(mydata$date[1])
mydata$date[1] + 2

# Convert date column into proper date col 
mydata$date <- as.Date(mydata$date, "%Y/%m/%d")
class(mydata$date[1])
mydata$date[1] + 10

head(mydata)
tail(mydata)

# Lowest date should be at the top and highest date should be at the bottom of dataframe 
# Sorting of dataframe on date column
mydata <- mydata[order(mydata$date), ]  # Sorting in ascending order of dates 
head(mydata)
tail(mydata)

# For timeseries, we need two column ( Date, Close)
mydata <- mydata[ , 1:2]
View(mydata)

# Let's change the columname - Close to be renamed to Price 
colnames(mydata) <- c("Date", "Price")
View(mydata)

# Import some libraries 
library(MASS)
library(tseries)
library(forecast)

# Prepare the data for timeseries training and prediction 
# How to make training and testing set for time-series ?
nrow(mydata)

# Pick first 900 obs from the data and use them for TS training 
stock   <- mydata$Price[1:900]
lnstock <- log(mydata$Price[1:900]) # log basically reduces the spread of data points  
lnstock

# acf and pacf ?
# Auto correlation factor and Partial Auto correlation factor ?

# They both are used to find out correlation between the values 

# ACF -> If I want to know 
# if today's value is correlated with yesterday's value
#  OR if today's value is correlated with day before yesterday's value
#  OR if today's value is correlated with 2 day before yesterday's value

# PACF -> If I want to know ( PURE CORRLEATION)
# of how today's value is correlated with previous days value by ignoring/REMOVING THE IMPACT OF 
# HOW PREVIOUS DAYS VALUE IS CORRELATED WITH ITS PREVIOUS DAYS. 

# PACF will tell you what is the order of "AR" model. ( Auto Regressive or not )
#  ACF will tell you what is the order of "MA" model. ( Moving average or not )
# ARMA or ARIMA ( Auto Regressive Integrated Moving Average) model. 

# If you will plot ACF and PACF you can decide which order of ARMA model this is. 

acf(lnstock, lag.max = 20)
pacf(lnstock, lag.max = 20)

# Order of MA model 
# Order of AR Model 
# Order of ARMA model 
# Order of ARIMA model 

# Auto Regressive Integrated Moving Avergae
# "Integrated" - Lag or efforts to make Time Series stationary
# ARIMA( 1, 0, 2) => AR =1, Diff = 0, MA = 2
# ARIMA( 2, 1, 3) => AR =2, Diff = 1, MA = 3

# How many difference we need to apply to make time series stationary

# Check if Time series is stationary 
# Dicky Fuller test 
adf.test(lnstock) # Null hypo -> Timeseries is stationary
# If p-value High means -> timeseries is already stationary

difflnstock <- diff(lnstock, 1)
adf.test(difflnstock)

difflnstock2 <- diff(lnstock, 2)
adf.test(difflnstock2)

# Step 1 - Plot ACF and  find out what order of MA TS is 
# Step 2 - Plot PACF and find out what order of AR TS is
# Step 3 - Pick the simplest of AR or MA or ARMA model 
# Step 4 - Run ADF test of TS, 
#         if p is high => time series is stationary 
#         if p is low  => apply a diff of 1 and do adf again 
#  from step 4, you will get the "I" value 
# ARIMA order. 

# auto.arima 
pricearima <- ts(lnstock, start = c(2015,9), frequency = 365)
# frequency = 12 => Data is collected for each month
plot(pricearima)
# Fitting or building an ARIMA time series model 
fitlnstock <- auto.arima(pricearima)
fitlnstock

# TS is an ARIMA with (2,1,2) => AR=2, Diff=1, MA=2

# Forecast value of ARIMA 
forecastvalues_ln <- forecast(fitlnstock, 107)  # Prediction of next 107 values 
forecastvalues_ln  # These forecasted values are in log scale 
# Forecasted values in actual terms 
exp(forecastvalues_ln$x)
plot(forecastvalues_ln)

# Precentage of error in prediction 
forecastvaluesextreacted <- as.numeric(forecastvalues_ln$mean)
finalforecastvalue <- exp(forecastvaluesextreacted)
finalforecastvalue

# Let's create a dataframe with actual and predicted values 
df <- data.frame(mydata$Price[901:1007], finalforecastvalue)
View(df)
colnames(df) <- c("Actual Values", "forecasted Price")


percentage_error <- ((df$`Actual Values` - df$`forecasted Price`)/df$`Actual Values`)
percentage_error

# Overall mean prediction error for 107 records
mean(percentage_error)
# 0.08% is the error in these prediction 

# PROPHET - It is an open source package by facebook basically for time-series prediction
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)


# Load data 
mydata <- read.csv("C:\\JS\\TimeSeries\\TS_Advance\\HistoricalQuotes.csv")
# Convert date column into proper date col 
mydata$date <- as.Date(mydata$date, "%Y/%m/%d")

head(mydata)
tail(mydata)

# Lowest date should be at the top and highest date should be at the bottom of dataframe 
# Sorting of dataframe on date column
mydata <- mydata[order(mydata$date), ]  # Sorting in ascending order of dates 
head(mydata)
tail(mydata)

# For timeseries, we need two column ( Date, Close)
mydata <- mydata[ , 1:2]
View(mydata)

# train and test split 
train <- mydata[1:900, ]
test  <- mydata[901:1007, ]
View(train)

# For Prophet package - colnames should be "ds" and "y". 
colnames(train) <- c("ds", "y")
head(train)

# model building 
m <- prophet(train)
m  # Please closely check various objects of this "m". 

# Create a future dataset 
future <- make_future_dataframe(m, periods = 107)
# in "m" we have details of 900 records 
# in "future" we have 900+107 => 1007 records 
future11 <- predict(m, future)

# Visualize 
plot(m, future11)

# Compare forecasted values against actual values 
final <- cbind( test, future11$yhat[901:1007])
View(final)

# %error in each prediction 
# overall mean of %error for each prediction 

prophet_plot_components(m, future11)

