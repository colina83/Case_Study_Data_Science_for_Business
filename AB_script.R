######## R Script A.Bürger
######## Case Study: "Solid as Steel: Production Planning at thysenkrupp"

#Import Libraries
library(Hmisc)
library(Rmisc)
library(jtools)
library(tidyverse)
library(UsingR)
library(lmtest)


rm(list=ls())

# check current working directory
getwd()

# load the 2 data files (different formats)
ThyssenKrupp_PPL_Data_Final_20150513 <- read_excel("ThyssenKrupp_PPL Data_Final_20150513.xls")
ThyssenKrupp_Data <- readRDS("ThyssenKrupp_Data.rds")

#Rename data set for usability
d1<-ThyssenKrupp_PPL_Data_Final_20150513
d2<-ThyssenKrupp_Data

#3 Cleaning & Preparing Data
## 3.1 - identify variables that needs to be converted to factors (Day & Night Shift)
## 3.2 - Cleanup the date variable, we only need the date, the time is irrelevant
## 3.3 - Removing variables that won't be use in the study

# convert characters of weekday, shift type and binary variables for shift group into factors
dCS$weekday<-as.factor(dCS$weekday)
dCS$`shift type`<-as.factor(dCS$`shift type`)
dCS$`shift 1`<-as.factor(dCS$`shift 1`)
dCS$`shift 2`<-as.factor(dCS$`shift 2`)
dCS$`shift 3`<-as.factor(dCS$`shift 3`)
dCS$`shift 4`<-as.factor(dCS$`shift 4`)
dCS$`shift 5`<-as.factor(dCS$`shift 5`)

# first glance on the data
View(d1)
str(dCS)
summary(dCS)

table(d1$`grade 1`==100)
# plausibility checks:
# weekday: number of weekdays within 6 months is unequal (Tuesday:only 63 observations, Friday/Saturday/Sunday: 74 observations)
      # shifts were omitted by Schultze by missing data or obviously erroneous data.
# shift type: in the data set, there are more Midday shifts reported (174) than night shifts (170) or early shifts (156)


