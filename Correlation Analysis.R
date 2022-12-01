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
co2_Road <- read.csv("CO2_emission.csv",header = TRUE, stringsAsFactors = TRUE)


#Selecting only the variables needed from the Country_info data

Country_info <- Country_info %>% select(geo,'World.bank..4.income.groups.2017','World.bank.region','four_regions')

#Renaming 'Code' as 'Country_Code' to facilitate merging

Country_info <- rename(Country_info,Country_Code=geo,Income_group='World.bank..4.income.groups.2017',Region='World.bank.region',Four_Regions='four_regions')

# selecting only the variables needed from the owid-co2-data.

data_co<-data_co2 %>% select('iso_code','country','year','co2','co2_per_capita','population','gdp','co2_per_gdp')

#Renaming 'iso_Code' as 'Country_Code' for merging

data_co <- rename(data_co,Country_Code=iso_code)

# Group by the Year the value of temperature

data_co1 <- data_co %>%  group_by(Country_Code,country,year)

# Calculate mean co2 emission group by year, country and country code

data_co2 <- data_co1 %>% 
  summarise(co2=mean(co2),
            co2_per_capita=mean(co2_per_capita),
            population=mean(population),gdp=mean(gdp),co2_per_gdp=mean(co2_per_gdp))

# converting the column to Uppercase

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
dataset_join <- dataset_join[-which(is.na(dataset_join$co2_per_gdp)),]
colSums(is.na(dataset_join))

#Correlation Analysis 1:

# creating a plot

  # Pearson correlation
  cor.test(dataset_join$population, dataset_join$co2,"two.sided","pearson")
  
  cov(dataset_join$population, dataset_join$co2)
  cov(dataset_join$gdp, dataset_join$co2)
  r<- cov(dataset_join$gdp, dataset_join$co2)/(sd(dataset_join$gdp)*sd(dataset_join$co2))
  r1<- cov(dataset_join$population, dataset_join$co2)/(sd(dataset_join$population)*sd(dataset_join$co2))
  
#plot correlation matrix
  data<-dataset_join[,4:8]
  group<-dataset_join[,1:3]
  pairs(data)

  # Function to add histograms
  panel.hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    his <- hist(x, plot = FALSE)
    breaks <- his$breaks
    nB <- length(breaks)
    y <- his$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = rgb(0, 1, 1, alpha = 0.5), ...)
    # lines(density(x), col = 2, lwd = 2) # Uncomment to add density lines
  }
  
  # Creating the scatter plot matrix
  pairs(data,
        upper.panel = NULL,         # Disabling the upper panel
        diag.panel = panel.hist)    # Adding the histograms
  
  # Function to add correlation coefficients
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    Cor <- abs(cor(x, y)) # Remove abs function if desired
    txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
    if(missing(cex.cor)) {
      cex.cor <- 0.4 / strwidth(txt)
    }
    text(0.5, 0.5, txt,
         cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
  }
  
  # Plotting the correlation matrix
  pairs(data,
        upper.panel = panel.cor,    # Correlation panel
        lower.panel = panel.smooth,diag.panel = panel.hist) # Smoothed regression lines
  
  #perform correlation test between the two vectors
  cor.test(dataset_join$gdp, dataset_join$co2)
  cor.test(dataset_join$population, dataset_join$co2)
  #Correlation Analysis 2:
  
  # creating a plot
  
  # Pearson correlation
  cor.test(dataset_join$population, dataset_join$co2,"two.sided","pearson")
  
  cov(dataset_join$population, dataset_join$co2)
  cov(dataset_join$gdp, dataset_join$co2)
  r<- cov(dataset_join$gdp, dataset_join$co2)/(sd(dataset_join$gdp)*sd(dataset_join$co2))
  r1<- cov(dataset_join$population, dataset_join$co2)/(sd(dataset_join$population)*sd(dataset_join$co2))
  
  #plot correlation matrix
  data<-dataset_join[,4:8]
  group<-dataset_join[,1:3]
  pairs(data)
  
  # Function to add histograms
  panel.hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    his <- hist(x, plot = FALSE)
    breaks <- his$breaks
    nB <- length(breaks)
    y <- his$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = rgb(0, 1, 1, alpha = 0.5), ...)
    # lines(density(x), col = 2, lwd = 2) # Uncomment to add density lines
  }
  
  # Creating the scatter plot matrix
  pairs(data,
        upper.panel = NULL,         # Disabling the upper panel
        diag.panel = panel.hist)    # Adding the histograms
  
  # Function to add correlation coefficients
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    Cor <- abs(cor(x, y)) # Remove abs function if desired
    txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
    if(missing(cex.cor)) {
      cex.cor <- 0.4 / strwidth(txt)
    }
    text(0.5, 0.5, txt,
         cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
  }
  
  # Plotting the correlation matrix
  pairs(data,
        upper.panel = panel.cor,    # Correlation panel
        lower.panel = panel.smooth) # Smoothed regression lines
  
  #perform correlation test between the two vectors
  cor.test(dataset_join$gdp, dataset_join$co2)
  cor.test(dataset_join$population, dataset_join$co2)
 
   #Correlation Analysis 2:
  
  # creating a plot
  str(co2_Road)
  # Pearson correlation
  cor.test(co2_Road$Fuel_Consumption_in_City, co2_Road$CO2_Emissions,"two.sided","pearson")
  
  cov(co2_Road$CO2_Emissions, co2_Road$Smog_Level)
  cov(co2_Road$CO2_Emissions, co2_Road$Fuel_Consumption_in_City)
  r<- cov(co2_Road$CO2_Emissions, co2_Road$Smog_Level)/(sd(co2_Road$CO2_Emissions)*sd(co2_Road$Smog_Level))
  r1<- cov(co2_Road$CO2_Emissions, co2_Road$Fuel_Consumption_in_City)/(sd(co2_Road$CO2_Emissions)*sd(co2_Road$Fuel_Consumption_in_City))
  
  #plot correlation matrix
  data1<-data.frame(co2_Road$Engine_Size,co2_Road$Cylinders,co2_Road$Fuel_Consumption_in_City,co2_Road$CO2_Emissions,co2_Road$Smog_Level)
  group1<-co2_Road[,1:4]
  pairs(data1)
  # Function to add histograms
  panel.hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    his <- hist(x, plot = FALSE)
    breaks <- his$breaks
    nB <- length(breaks)
    y <- his$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = rgb(0, 1, 1, alpha = 0.5), ...)
    # lines(density(x), col = 2, lwd = 2) # Uncomment to add density lines
  }
  
  # Creating the scatter plot matrix
  pairs(data1,
        upper.panel = NULL,         # Disabling the upper panel
        diag.panel = panel.hist)    # Adding the histograms
  
  # Function to add correlation coefficients
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    Cor <- abs(cor(x, y)) # Remove abs function if desired
    txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
    if(missing(cex.cor)) {
      cex.cor <- 0.4 / strwidth(txt)
    }
    text(0.5, 0.5, txt,
         cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
  }
  
  # Plotting the correlation matrix
  pairs(data1,
        upper.panel = panel.cor,    # Correlation panel
        lower.panel = panel.smooth,
        diag.panel = panel.hist) # Smoothed regression lines
  
  #perform correlation test between the two vectors
  cor.test(co2_Road$CO2_Emissions, co2_Road$Smog_Level)
  cor.test(co2_Road$CO2_Emissions, co2_Road$Fuel_Consumption_in_City)
  cor.test(co2_Road$CO2_Emissions, co2_Road$Cylinders)
  cor.test(co2_Road$CO2_Emissions, co2_Road$Engine_Size)
  cov(co2_Road$CO2_Emissions, co2_Road$Smog_Level)
  cov(co2_Road$CO2_Emissions, co2_Road$Fuel_Consumption_in_City)
  
  summary(co2_Road)