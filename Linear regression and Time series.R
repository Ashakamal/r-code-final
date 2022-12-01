#install the packages
install.packages("dplyr") 
install.packages(c("devtools", "astsa", "mgcv"))
install.packages("expsmooth")
install.packages("TSA")
install.packages("AER")
install.packages("x12")
install.packages("predict3d")
install.packages("psych")
install.packages("magrittr")
install.packages("gtsummary")
install.packages("DescTools")
install.packages("nortest")
install.packages("lmtest")
install.packages("sandwich")

#loading the library

library(dplyr)
library(tidyr)
library(psych)
library(forecast)
library(expsmooth)
library(TSA)
library(x12)
library(AER)
library(car)
library(Hmisc)
library(tidyverse)
library(lubridate)
library(dplyr)                                
library(astsa)
library(mgcv)
library(rpart)
library("ggplot2")
library("predict3d")
library("psych")
library("magrittr") ## allows for rounding using the %>%
library("dplyr")
library("gtsummary")
library("DescTools") ## Needed for normality testing
library("nortest") ## Needed for normality testing
library("lmtest") ## Need for heteroskedasticity testing
library("sandwich")  ## Needed for estimating Huber-White sandwich standard errors

#load the dataset
getwd()
setwd("C:/Applied_Statistics/assesment/RData")
co2_data<-read.csv("MER_T12_06.csv")
temp_data<-read.csv("FAOSTAT_data_1-10-2022.csv")

#selecting only the variables needed in the temperature dataset
temp_data <- temp_data[,c("Domain","Element","Year","Value")]

#selecting only the variables needed in CO2 data
co2_data<-co2_data[,c("MSN","YYYYMM","Value")]

#Subsetting the data using the Apply filter function
co2_data<-filter(co2_data, substr(co2_data$YYYYMM,5,6) == "13")

#creating the dataframe with the selected columns
co2_data<-data.frame(co2_data$MSN,substr(co2_data$YYYYMM,1,4),co2_data$Value)

#converting the MSN variable into wide format to facilitate the join
co2_wideFormat <- spread(co2_data, key = co2_data.MSN, value = co2_data.Value)

#rename just the "Year" column in co2_wideFormat
names(co2_wideFormat)[names(co2_wideFormat)=="substr.co2_data.YYYYMM..1..4."] <- "Year"
names(co2_wideFormat)[names(co2_wideFormat)=="CLEIEUS"] <- "Coal"
names(co2_wideFormat)[names(co2_wideFormat)=="DKEIEUS"] <- "Distillate_Fuel"
names(co2_wideFormat)[names(co2_wideFormat)=="GEEIEUS"] <- "Geothermal_Energy"
names(co2_wideFormat)[names(co2_wideFormat)=="NNEIEUS"] <- "Natural_Gas"
names(co2_wideFormat)[names(co2_wideFormat)=="NWEIEUS"] <- "Non_Biomass"
names(co2_wideFormat)[names(co2_wideFormat)=="PAEIEUS"] <- "Petroleum"
names(co2_wideFormat)[names(co2_wideFormat)=="PCEIEUS"] <- "Petroleum_Coke"
names(co2_wideFormat)[names(co2_wideFormat)=="RFEIEUS"] <- "Residual_Fuel"
names(co2_wideFormat)[names(co2_wideFormat)=="TXEIEUS"] <- "Total_Energy"
#converting the Year column into the number
co2_wideFormat$Year<-as.numeric(co2_wideFormat$Year)

# Group by the Year the value of temperature
temp_data_year <- temp_data %>%  group_by(Year)

# Calculate mean temperature by year
temp_data_mean <- temp_data_year %>% 
  summarise(mean_temp = mean(Value, na.rm = TRUE),
            count = n()
  )
#Create the dataframe for the temperture data
temp_data_mean<-data.frame(temp_data_mean$Year,temp_data_mean$mean_temp)

#Rename the year column in the temperature data.
names(temp_data_mean)[names(temp_data_mean)=="temp_data_mean.Year"] <- "Year"

#Joining 'co2 data ' and 'temerature' data
dataset_join1 <- left_join( co2_wideFormat,temp_data_mean,by="Year" )

#Rename the Temperature column in the joined data set.
names(dataset_join1)[names(dataset_join1)=="temp_data_mean.mean_temp"] <- "Temp"
names(dataset_join1)

#Converting the data type 
dataset_join1$Coal<-as.numeric(dataset_join1$Coal)
dataset_join1$`Distillate_Fuel`<-as.numeric(dataset_join1$Distillate_Fuel)
dataset_join1$`Geothermal_Energy`<-as.numeric(dataset_join1$Geothermal_Energy)
dataset_join1$`Natural_Gas`<-as.numeric(dataset_join1$Natural_Gas)
dataset_join1$`Non_Biomass`<-as.numeric(dataset_join1$Non_Biomass)
dataset_join1$Petroleum<-as.numeric(dataset_join1$Petroleum)
dataset_join1$`Petroleum_Coke`<-as.numeric(dataset_join1$Petroleum_Coke)
dataset_join1$`Residual_Fuel`<-as.numeric(dataset_join1$Residual_Fuel)
dataset_join1$`Total_Energy`<-as.numeric(dataset_join1$Total_Energy)

# Understand the data using str()
str(dataset_join1)

#checking the NA values for all the variables
colSums(is.na(dataset_join1))

#Ploting the chart
ggplot(dataset_join1, aes(x = Year , y = Coal )) + 
  geom_point()

IQR(dataset_join1$Temp)

#Exploratory Data Analayis
ggplot(dataset_join1, aes(x = Year , y = Temp )) + 
  geom_point()

# study the response variable, co2
ggplot(data = dataset_join1) +
  ggtitle("Histogram of the CO2 emissions") +
  geom_histogram(mapping = aes(x = Coal), na.rm = TRUE, binwidth = 10)

#Scatter plot1
scatterplot1 <- ggplot(dataset_join1, aes(Temp, Coal , alpha=0.2)) + geom_point()
scatterplot1

#Scatterplot2
scatterplot9 <- ggplot(dataset_join1, aes(Temp,Total_Energy, alpha=0.2)) + geom_point()
scatterplot9

# Exploratory Data Analysis
mu <- dataset_join1 %>% 
  group_by(Year) %>%
  summarise(grp.mean = mean(Total_Energy))
mu
# Simple Linear regression
#fit linear regression model using 'x' as predictor and 'y' as response variable
model <- lm(dataset_join1$Temp ~ dataset_join1$Coal , data=dataset_join1)

#view summary of regression model
summary(model,confint = TRUE, digits = 3)

#plot model
plot(model)

#adjust plot margins
par(mar = c(1, 1, 1, 1))

#create diagnostic plots
### Plot the association between the Temperature Change and carbon emission from Electric power generation.
ggplot(dataset_join1, aes(x = Temp, y = Coal)) +
  geom_point() +
  stat_smooth()

### Generate the 95% CI
confint

#### Plot the residuals against the predicted model (Is it homoscedastic?)
#check Equal variance / Homoscedasticity
plot(model$fitted.values,model$res)
abline(0,0, col = 3)
#### Set up the matrix
par(mfrow = c(1, 2))
#### Histogram of the residuals
hist(model$res)
#### QQ-plot of the residuals against the QQ line
qqnorm(model$res); qqline(model$res, col = "2", lwd = 1, lty = 1)
#### Test normality using Shapiro-Wilk's test
shapiro.test(model$res)
str(dataset_join1)
bptest(model)

#Multiple Linear Regression
### Plot the association between the Temperature Change and carbon emission from Electric power generation.
ggplot(dataset_join1, aes(x = Temp, y = Total_Energy)) +
  geom_point() +
  stat_smooth()

Model2<-lm(dataset_join1$Temp ~ dataset_join1$Coal+dataset_join1$Distillate_Fuel+dataset_join1$Natural_Gas+dataset_join1$Petroleum+dataset_join1$Petroleum_Coke+dataset_join1$Residual_Fuel , data=dataset_join1)

#Summary of the model
summary(Model2)
#Plot th emodel
plot(Model2)
#### Plot the residuals against the predicted model (Is it homoscedastic?)
#check Equal variance / Homoscedasticity
dev.off()
plot(Model2$fitted.values,Model2$res)
abline(0,0, col = 3)

#### Set up the matrix
par(mfrow = c(1, 2))
#### Histogram of the residuals
hist(Model2$res)
#### QQ-plot of the residuals against the QQ line
qqnorm(Model2$res); qqline(Model2$res, col = "2", lwd = 1, lty = 1)
#### Test normality using Shapiro-Wilk's test
shapiro.test(Model2$res)
bptest(Model2)
####Time Series Forecasting ######

# Returns best ARIMA model according to either BIC value.
# The "auto.arima" function conducts a search over possible model within the period

#forecasting the Carbon emission

#Convert the data into time series data format and fiiting th emodel

ts.fit= auto.arima(ts(dataset_join1$Total_Energy, 1990,
                      2015), ic="bic")
ts.fit
# check the residuals
checkresiduals(ts.fit,lag=10)
# forecast for 2050
dev.off()
forecast(ts.fit,h=35)
#plotting the forecast
  plot(forecast(ts.fit,h=35),main="Predicting Carbon Emission on 2050")
#Ljung Box test on the model
  Box.test(resid(ts.fit),type="Ljung",lag=20,fitdf=1)

####################################################################
  
 
  