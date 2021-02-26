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



#4 Exploratory Data Analysis and Plots

#######################################################

#PART A
#Answering Questions:
#1.Perform a univariate analysis and answer the following questions: 
#a.- What is the average number of strips per shift? 

d$Total_Strips <- (d$`thickness 1`+d$`thickness 2`+d$`thickness 3`)
Average_strips <- mean(d$Total_Strips)
t1 <- sum(d$`thickness 1`)

#2.- Which is the most commen and least common
sum(d$`thickness 1`)/sum(d$Total_Strips)*100
sum(d$`thickness 2`)/sum(d$Total_Strips)*100
sum(d$`thickness 3`)/sum(d$Total_Strips)*100

#3 Values of delta
summary(d$`run time ratio`)
summary(d$MPT)

#4 & % 5 are just questions