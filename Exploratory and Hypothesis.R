#install required Packages
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("Hmisc")
install.packages("outliers")
install.packages("forecast")
install.packages("e1071")

#Loading the libraries
library(readr)
library(dplyr)
library(tidyr)
library(Hmisc)
library(outliers)
library(forecast)
library(tidyverse)
library(dplyr)
library("ggplot2")
library(e1071)
#theme_set(theme_bw())
library("sf")
library("rgeos")
library("rnaturalearth")
library("rnaturalearthdata")
library("mapview")
library("tidycensus")
library("sp")
library("spatialEco")
library("tigris")
library("maps")
library("leaflet")
library("tmap")
#importing dataset
getwd()
setwd("C:/Applied_Statistics/assesment/RData")
data_co2<-read.csv("owid-co2-data.csv")
Country_info<-read.csv("country_info.csv")
#Reading Co2_Energy and temperature data 
#co2_eng<-read.csv("MER_T12_06.csv")
temp_data<-read.csv("FAOSTAT_data_1-10-2022.csv")
co2_Road <- read.csv("CO2_emission.csv",header = TRUE, stringsAsFactors = FALSE)

#Selecting only the variables needed from the Country_info data

Country_info <- Country_info %>% select(geo,'World.bank..4.income.groups.2017','World.bank.region','four_regions')

#Renaming 'Code' as 'Country_Code' for merging

Country_info <- rename(Country_info,Country_Code=geo,Income_group='World.bank..4.income.groups.2017',Region='World.bank.region',Four_Regions='four_regions')

# selecting only the variables needed from the owid-co2-data.

data_co<-data_co2 %>% select('iso_code','country','year','co2','co2_growth_prct','co2_growth_abs'
                             ,'co2_per_capita','population','gdp','co2_per_gdp')

#Renaming 'iso_Code' as 'Country_Code' for merging

data_co <- rename(data_co,Country_Code=iso_code)

# Group by the Year the value of temperature

data_co1 <- data_co %>%  group_by(Country_Code,country,year)

# Calculate mean co2 emmision group by year, country and country code

data_co2 <- data_co1 %>% 
  summarise(co2=mean(co2),co2_growth_prct=mean(co2_growth_prct),
            co2_growth_abs=mean(co2_growth_abs),co2_per_capita=mean(co2_per_capita),
            population=mean(population),gdp=mean(gdp),co2_per_gdp=mean(co2_per_gdp))

# converting the goe column to Uppercase to help merging 

Country_info[,1]<-toupper(Country_info[,1])

#Joining 'data_co' and 'country_info'

dataset_join <- left_join( data_co,Country_info,by="Country_Code" )

head(dataset_join)
names(Country_info)

# Understand the data using str()

str(dataset_join)

#checking the NA values for all the variables

colSums(is.na(dataset_join))

#finding the length to calculate the % of missing values in column co2

length(dataset_join$co2)

#Since the % of the missing values in the Co2 is less than 5% we can omit the observations with missing values

dataset_join <- dataset_join[-which(is.na(dataset_join$Income_group)),]
colSums(is.na(dataset_join))
dataset_join <- dataset_join[-which(is.na(dataset_join$co2_per_capita)),]
colSums(is.na(dataset_join))


#Subsetting the data by Rows using the Apply filter function(To compare between two set of years)

dataset_join1<-filter(dataset_join, (dataset_join$year < 1990))
dataset_join2<-filter(dataset_join, (dataset_join$year > 1991))

# Central Tendency

summary(dataset_join1$co2_per_capita)
summary(dataset_join2$co2_per_capita)

#Summary1

skewness(dataset_join1$co2_per_capita)
IQR(dataset_join1$co2_per_capita)
sd(dataset_join1$co2_per_capita)

#summary2

skewness(dataset_join2$co2_per_capita)
IQR(dataset_join2$co2_per_capita)
sd(dataset_join2$co2_per_capita)


length(dataset_join1$co2)
length(dataset_join2$co2)

#Histogram of co2_per_capita

ggplot(data = dataset_join, aes(x = co2_per_capita,fill=Region)) +
  geom_histogram(bins = 100)+ xlim(0,7.5)+ylim(0,400) +theme(legend.position="bottom")

#box plot of co2_per_capita
ggplot(dataset_join, aes(co2_per_capita, co2_per_gdp, fill=Four_Regions)) + geom_boxplot() +theme(legend.position = "bottom")

#selecting only the variables needed in the temperature dataset
temp_data <- temp_data[,c("Domain","Element","Year","Value")]

#selecting only the variables needed in CO2 data
#co2_eng<-co2_eng[,c("MSN","YYYYMM","Value")]

#Subsetting the data using the Apply filter function
#co2_eng<-filter(co2_eng, substr(co2_eng$YYYYMM,5,6) == "13")

# Centeral Tendency
#str(co2_eng)
str(temp_data)

summary(temp_data$Value)

#checking the NA values for all the variables

colSums(is.na(temp_data))

# finding the length of the dataset to determine the percentage of the missing value.

length(temp_data$Value)

#Since the % of the missing values in the Co2 is less than 5% we can omit the observations with missing values

temp_data <- temp_data[-which(is.na(temp_data$Value)),]
colSums(is.na(temp_data))

# Group by the Year the value of temperature
temp_data_year <- temp_data %>%  group_by(Year)

# Calculate mean temperature change by year
temp_data_mean <- temp_data_year %>% 
  summarise(mean_temp = mean(Value, na.rm = TRUE),
            count = n()
  )
#Subsetting the data by Rows using the Apply filter function
temp_data_mean1<-filter(temp_data_mean, (temp_data_mean$Year<=1990))
temp_data_mean2<-filter(temp_data_mean, (temp_data_mean$Year>=1991))

#Summary of the statistics.
summary(temp_data_mean1$mean_temp)
summary(temp_data_mean2$mean_temp)

#Skewness
skewness(temp_data_mean1$mean_temp)
skewness(temp_data_mean2$mean_temp)

#Standard Deviation
sd(temp_data_mean1$mean_temp)
sd(temp_data_mean2$mean_temp)

#IQR
IQR(temp_data_mean1$mean_temp)
IQR(temp_data_mean2$mean_temp)

length(temp_data_mean1$mean_temp)
length(temp_data_mean2$mean_temp)

# Basic box plot
ggplot(temp_data_mean, aes(y = mean_temp)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15) + 
  geom_boxplot() 
##Histogram

ggplot(data = temp_data_mean, aes(x = mean_temp)) +
  geom_histogram(bins = 35) + xlim(0,2)+ylim(0,5)

# Density plot
a <- ggplot(temp_data_mean, aes(x = mean_temp))
# y axis scale = ..density.. (default behaviour)
a + geom_density() +
  geom_vline(aes(xintercept = mean(mean_temp)), 
             linetype = "dashed", size = 0.6)

# Change y axis to count instead of density
a + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(mean_temp)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")


##############################Hypothesis#######################################

#hypothesis testing 
high_income <-filter(dataset_join,(dataset_join$Income_group == ("High income")|dataset_join$Income_group == ("Upper middle income")))
mid_income <-filter(dataset_join,(dataset_join$Income_group == ("Upper middle income")|dataset_join$Income_group == ("Lower middle income")))

#conduct a two sample test


t.test(high_income$co2,mid_income$co2,conf.level=0.99)

wilcox.test(high_income$co2,mid_income$co2,exact = FALSE)
