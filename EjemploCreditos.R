#Agregar un Cambio

library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library(magrittr)
library(corrplot)
#Read Data
myData <- read_csv("~/Desktop/Creditos.csv")
View(myData)
#Define my own deviance function 
myStd <- function(x){(x-min(x))/(max(x)-min(x))}

#Modify categories to binary to allow PCA analysis
myData %<>% mutate(status = 
                               ifelse(status == "paid",1,0),
                            gender = 
                              ifelse(gender == "Mujer",1,0),
                            urban_city = 
                              ifelse(urban_city == "rural",1,0),
                            device = 
                              ifelse(device == "android",1,0))

myData %<>% mutate(status = 
                     as.numeric(status),
                   gender = 
                     as.numeric(gender),
                   urban_city = 
                     as.numeric(urban_city),
                   device = 
                     as.numeric(device))

#Select variables of interest 
PCAData <- myData %>% select(status,loan_amount,risk_score,gender,age,dependants,device,urban_city)
#Store Correlation Matrix 
CorrelationMatrix <- cor(PCAData)
correlationPlot <- corrplot(CorrelationMatrix)
#PCA and factor unloading
pca <- princomp(PCAData, cor = TRUE)
unclass(pca$loadings)
#Paste PCA Scores using component 7 which was the most convenient factor to use beacause of loadings closer to explain paid status 
PCAData %<>% mutate(pca_index = pca$scores [ ,7])
#Normalize index by ranking them 1 to 0. One being the most positive characteristics of paid status. 
PCAData %<>% mutate(pca_index = myStd(pca_index))
#DIvide Data by Quartile
PCAData %<>%mutate(
  quartile = ntile(pca_index, 4)
) 
TopUsers <- PCAData %>% filter(quartile == "4")
AverageTopCharacteristics <- summary(TopUsers)

#renameColumns 

TopUsers %<>% mutate(status = 
         ifelse(status == 1,"paid","loan"),
       gender = 
         ifelse(gender == 1,"Mujer","hombre"),
       urban_city = 
         ifelse(urban_city == 1,"rural","urban"),
       device = 
         ifelse(device == 1,"android","ios"))


#Ellaborate Plots to do exploratory analysis
DevicePlot <- TopUsers %>%
  ggplot( aes(x=device, y=loan_amount)) + geom_boxplot(aes(fill=device))

UrbanPlot <- TopUsers %>% 
  ggplot( aes(x=age, y=loan_amount, shape=urban_city, color=urban_city)) +
  geom_point()

GenderPlot <- TopUsers %>% 
  ggplot( aes(x=pca_index, color=gender)) +
  geom_histogram(fill="white", alphagenderposition="identity")



#After doing exploratory analysis .9124109 was selected as the because it will allow a population of size 496 this will allwo the two agents call every single number in a period of 3 days, as the maximum number of numbers they can dial is 540. 
TopUsers <- PCAData %>% filter(pca_index >.9124109)
AverageTopCharacteristics <- summary(TopUsers)

#renameColumns 

TopUsers %<>% mutate(status = 
                       ifelse(status == 1,"paid","loan"),
                     gender = 
                       ifelse(gender == 1,"Mujer","hombre"),
                     urban_city = 
                       ifelse(urban_city == 1,"rural","urban"),
                     device = 
                       ifelse(device == 1,"android","ios"))



TopDevicePlot <- TopUsers %>%
  ggplot( aes(x=device, y=loan_amount)) + geom_boxplot(aes(fill=device))

TopUrbanPlot <- TopUsers %>% 
  ggplot( aes(x=age, y=loan_amount, shape=urban_city, color=urban_city)) +
  geom_point()

TopGenderPlot <- TopUsers %>% 
  ggplot( aes(x=pca_index, color=gender)) +
  geom_histogram(fill="white", alphagenderposition="identity")

TopAgePlot <- TopUsers %>% 
  ggplot( aes(x=age, color=gender)) +
  geom_histogram(fill="white", alphagenderposition="identity")




