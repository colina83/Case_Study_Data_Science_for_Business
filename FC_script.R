###############################################
## Tasks to do Prior Starting the project

#Import Libraries
library(Hmisc)
library(Rmisc)
library(jtools)
library(UsingR)
library(tidyverse)
library(lmtest)
library(sandwich)
library(mctest)

#1.- Uploading Data to R

d <- ThyssenKrupp_PPL_Data_Final_20150513 # Excel File
e <- ThyssenKrupp_Data # RDS File from 
all(e == d) # JUst checking that all values are the same in both datasets

#2.- Information regarding size and Variables  ##

dim(d) # Number of Variables -> Rows (Observations ) & Variables

names(d) # Variable Names

#Arrange the names of variables
names(d) <- gsub(" ", "_", names(d))

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

d$Total_Strips <- (d$thickness_1+d$thickness_2+d$thickness_3)
Average_strips <- mean(d$Total_Strips)
t1 <- sum(d$thickness_1)

#2.- Which is the most commen and least common
sum(d$thickness_1)/sum(d$Total_Strips)*100
sum(d$thickness_2)/sum(d$Total_Strips)*100
sum(d$thickness_3)/sum(d$Total_Strips)*100

#3 Values of delta
summary(d$run_time_ratio)
summary(d$MPT)


#####################################################

# y = Delta Throughput
# Possible X = Shift,Shift Type, thickness, width, 

plot(d$delta_throughput ~ d$Total_Strips, xlab="Number of Strips per Shift", ylab="Delta Throughput", main="Scatterplot Delta Througput")
plot(d$delta_throughput ~ d$run_time_ratio, xlab="RTR", ylab="Delta Throughput", main="Scatterplot Delta Througput")

# Model 1
lm1 <- lm(d$delta_throughput ~ d$thickness_1+ d$thickness_2 + d$thickness_3 + d$run_time_ratio)
summary(lm1)

# Model 1
lm2 <- lm(d$delta_throughput ~ d$width_1 + d$width_2 +d$width_3 + d$run_time_ratio)
summary(lm2)

#Obviously Model 1 is the same as Model 2

# It should be obvious that the reason is that width and thickness accounts to 1, is interesting that when using the total the R value goes down, 
# can think why this is the reason?

lm3 <- lm(d$delta_throughput ~ d$Total_Strips + d$run_time_ratio)
summary(lm3)

lm4 <- lm(d$delta_throughput ~ d$grade_1 + d$grade_2 + d$grade_3 + d$grade_4 + d$grade_5 + d$grade_rest + d$run_time_ratio)
summary(lm4)

lm5 <- lm(d$delta_throughput ~ d$thickness_1+ d$thickness_2 + d$thickness_3 + d$grade_1 + d$grade_2 + d$grade_3 + d$grade_4 + d$grade_5 + d$grade_rest + d$run_time_ratio)
summary(lm5)


lm6 <- lm(d$delta_throughput ~ d$thickness_1+ d$thickness_2 + d$thickness_3 + d$width_1 + d$width_2 + d$run_time_ratio)
summary(lm6)

# testing lm6

coeftest(lm6, vcov = vcovHC(lm6))  
imcdiag(lm6) # Low VIF's no correlation


##########
# Confidence Interval

confint(lm6, level = 0.90)





