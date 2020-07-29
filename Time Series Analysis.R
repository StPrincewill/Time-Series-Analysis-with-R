##################################################
###                                             ## 
##################################################
#                                               ##
##################################################
# Written by Princewill Iheanacho
# 
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("C:/Users/Dell/Desktop/Data Analysis")

options(scipen=9)  



##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(tseries)){install.packages("tseries")}
library("tseries")

if(!require(TTR)){install.packages("TTR")}
library("TTR")

if(!require(smooth)){install.packages("smooth")}
library("smooth")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

####################################################
## Welland Rain Time Series Analysis              ##
####################################################

#Reading the data

WellandPrecip_PI <- read.csv("Welland_Rain.csv", header=TRUE)
head(WellandPrecip_PI)
Precip_PI <- WellandPrecip_PI[c(-1)]
head(Precip_PI)

#Convert to Time series Datatype

PrecipStudy_PI <- ts(Precip_PI, frequency = 12, start=c(1995,1))
head(PrecipStudy_PI)
PrecipStudy_PI

##Descriptive Summary Statistics
#Summary of the precipitation information

summary(PrecipStudy_PI)
stat.desc(PrecipStudy_PI)
par(mfrow=c(3,2))

# BoxPlot for variable
# loop over column *names* instead of actual columns
sapply(names(WellandPrecip_PI), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(WellandPrecip_PI[[cname]]))
    # use the `main` param to put column name as plot title
    print(boxplot(WellandPrecip_PI[[cname]], main=cname))
})

par(mfrow=c(1,1))

###PLOT THE TIME SERIES ####

plot.ts(PrecipStudy_PI, main="Average Precipitation - Welland") 

### Decomposing the Time Series

decompPrecip_PI <- decompose(PrecipStudy_PI, type="additive")  
decompPrecip_PI
plot(decompPrecip_PI)

adf.test(PrecipStudy_PI) 

#Deseasonalize the time series

PrecipStudySea_PI <- PrecipStudy_PI - decompPrecip_PI$seasonal

plot.ts(PrecipStudySea_PI, main="Deseasonalized - Average Precipitation - Welland")



####################################################
## Waterloo Precipitation Time Series Analysis    ##
####################################################

#Let's read the data

WaterlooPrecip_PI <- read.csv("Waterloo_Precip.csv", header=TRUE)
head(WaterlooPrecip_PI)

WPrecip_PI <- WaterlooPrecip_PI[c(-1)]
head(WPrecip_PI)

#Convert to Time series Datatype

WPrecipStudy_PI <- ts(WPrecip_PI, frequency = 1, start=c(1950))
head(WPrecipStudy_PI)
#WPrecipStudy_PI

summary(WPrecipStudy_PI)
stat.desc(WPrecipStudy_PI)


###PLOT THE TIME SERIES ####

plot.ts(WPrecipStudy_PI, main="Average Precipitation - Waterloo") 

#Let us smooth the series using different moving averages and Spot the  trends 
# Let's see n = 5

WPrecipSMA5_PI <- SMA(WPrecipStudy_PI,n=5)
plot.ts(WPrecipSMA5_PI)

#Let's see for n= 8


WPrecipSMA8_PI <- SMA(WPrecipStudy_PI,n=8)
plot.ts(WPrecipSMA8_PI)


#Let's see for n= 10


WPrecipSMA10_PI <- SMA(WPrecipStudy_PI,n=10)
plot.ts(WPrecipSMA10_PI)

#Let's check to see if the time series is stationary

adf.test(WPrecipStudy_PI)

#Let's create an auto correlation chart

acf(WPrecipStudy_PI)


#Moving Average Forecast

move_avg_PI <- sma(WPrecipStudy_PI)
move_avg_PI
move_avg_PI <- forecast(move_avg_PI, h=5,level=0.75)   
move_avg_PI
plot(move_avg_PI)


#Exponential Smoothing Forecast

ES_avg_PI <- es(WPrecipStudy_PI)
ES_avg_PI
ES_avg_PI <- forecast(ES_avg_PI, h=5,level=0.75)
ES_avg_PI
plot(ES_avg_PI)








