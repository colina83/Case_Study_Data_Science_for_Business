###############################################
## Tasks to do Prior Starting the project

#Import Libraries
library(Hmisc)
library(jtools)
library(UsingR)
library(tidyverse)

#1.- Uploading Data to R

d <- ThyssenKrupp_PPL_Data_Final_20150513 # Excel File
e <- ThyssenKrupp_Data # RDS File from 
all(e == d) # JUst checking that all values are the same in both datasets

#2.- Information regarding size and Variables  ##

dim(d) # Number of Variables -> Rows (Observations ) & Variables

names(d) # Variable Names

#3.- Cleaning & Preparing Data
## 3.1 - identify variables that needs to be converted to factors (Day & Night Shift)
## 3.2 - Cleanup the date variable, we only need the date
d$shift <- str_remove(d$shift,"22:00:00")
d$shift <- str_remove(d$shift,"06:00:00")
d$shift <- str_remove(d$shift,"14:00:00")

## 3.3 - Removing variables that won't be use in the study


#4 Exploratory Data Analysis and Plots

#######################################################

#PART A
#Answering Questions:
#1.Perform a univariate analysis and answer the following questions: 