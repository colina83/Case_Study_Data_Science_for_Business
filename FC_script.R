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
library(ggplot2)
library(expss)

#Uploading Data to R

d <- ThyssenKrupp_PPL_Data_Final_20150513 # Excel File
e <- ThyssenKrupp_Data # RDS File from 
all(e == d) # JUst checking that all values are the same in both datasets
#Arrange the names of variables
names(d) <- gsub(" ", "_", names(d))
#Information regarding size and Variables  ##

dim(d) # Number of Variables -> Rows (Observations ) & Variables

names(d) # Variable Names
table(d$shift_type)

#Cleaning & Preparing Data
## identify variables that needs to be converted to factors (Day & Night Shift)
## Cleanup the date variable, we only need the date
d$shift <- str_remove(d$shift,"22:00:00")
d$shift <- str_remove(d$shift,"06:00:00")
d$shift <- str_remove(d$shift,"14:00:00")


#Exploratory Data Analysis and Plots

#######################################################

#PART A
#Answering Questions:
#1.Perform a univariate analysis and answer the following questions: 
#a.- What is the average number of strips per shift? 

d$Total_Strips <- (d$thickness_1+d$thickness_2+d$thickness_3)
Average_strips <- mean(d$Total_Strips)
t1 <- sum(d$thickness_1)
summary(d$MPT)

#1.b- Which is the most common and least common
a = sum(d$thickness_1)/sum(d$Total_Strips)*100
b = sum(d$thickness_2)/sum(d$Total_Strips)*100
c = sum(d$thickness_3)/sum(d$Total_Strips)*100
## Thickness 2 is the most common and thickness 3 is the least common


## FC to create graph
jpeg('1b.jpg')
Bar_Plot_Q1b <- barplot(c(a,b,c), b,col=c("darkblue","lightblue","gray"), main = "Thickness Values", xlab = "Thickness", legend = c("Thickness 1", "Thickness 2", "Thickness 3"))
dev.off()


#1.C Values of delta
par(mfrow =c(1,2))
boxplot(summary(d$run_time_ratio))
boxplot(summary(d$delta_throughput))
## Add a summary table, and make the plot prettier 

Delta_Throughput_Summary <- summary(d$delta_throughput)
Run_Time_Ratio <- summary(d$run_time_ratio)
rbind(table_DT,table_TR)

#####################################################

table(d$grade_1 == 100)
table(d$grade_2 == 100)
table(d$grade_3 == 100)
table(d$grade_4 == 100)
table(d$grade_5 == 100)
table(d$grade_rest == 100)

##grade 1, 4, 5  and rest have 100 %

## 2.- Can the RTR Theory explain deviations
valcol <- (d$MPT + abs(min(d$MPT)))/max(d$MPT + abs(min(d$MPT)))
# This variable is to identify the low MPT values in light blue and the high MPT values in darker color
plot(d$delta_throughput ~ d$run_time_ratio, xlab="RTR", ylab="Delta Throughput", main="Scatterplot Delta Througput", col = rgb(0, 0, valcol))
abline(h= 0,col ="black")
lm_dt <- lm(d$delta_throughput ~ d$run_time_ratio)
abline(lm_dt,col = "red")
abline(h= 0,col ="black") # Points below zero 
summary(lm_dt)


#3.- MPT
plot(d$delta_throughput ~ d$MPT, xlab = "MPT", ylab = "Delta Throughput", main = "Scatterplot")
lm_mpt <- lm(d$delta_throughput ~ d$MPT)
abline(lm_mpt,col = "red")



plot(d$delta_throughput ~ d$Total_Strips, xlab="Number of Strips per Shift", ylab="Delta Throughput", main="Scatterplot Delta Througput")

# 4.- Regression model to predict delta throughput
# The question alludes that all independent variables should be based on the characteristic of the strips
# We did not include MPT (meters per ton), a dimension indicator, but a theoretical factor calculated by the engineers
# Schulze did not account for MPT, so we based our models on the characteristics of the material
# We have left thickness 1 to 3 and Width and Width3 , as this complements each other to account to a total number of strips
# Grade is significant, and we have included 3 variables out of 6 
# Test - heteroskedasticity - Removal of Grade 3
# Test Pass - Model with Grade 1 and Grade 5 (Final)

lm_final <- lm(delta_throughput ~ thickness_1+ thickness_2 + thickness_3 + width_1 + width_3 + grade_1  + grade_5 + run_time_ratio, data = d)
summary(lm_final)

# Plotting to see the dispersion between the fitted values (estimations) and the residuals (errors)
plot(fitted(lm_new),residuals(lm_new))


#Breusch-Pagan Test, small p-value, we can assume, we have to reject H:0 
bptest(lm_final)

# We are testing heteroskedasticity,heteroscedasticity is the absence of homoscedasticity.  
coeftest(lm_final, vcov = vcovHC(lm_new,"HC1"))

# Testing Multi-Collinearity 
imcdiag(lm_final) # Low VIF's no correlation

##########
#5.-  Confidence Interval (90%) 
# Coefficient for run_time_ratio = 4.7 to 6.01 
# This means that for each increase in percentage of RTR (Efficiency), we se an increase in the delta throughput of 5.40 

confint(lm_final, level = 0.90)

#############################
#6.- Change in delta throughput 

delta_1_3 <-  15.8708 + 3.4007 - 6.9680 + 6.4173 
delta_1_3


## 7.- Production Forecast - Delta Throughput (tons) Estimation per shift for the month of May 
## The estimate for change delta throughput due to change in thickness and width (from 1 to 3) is 18.72 
# Creating a dataframe for each individual value of X (independent variables)


new <- data.frame(thickness_1 = 996/84, thickness_2 = 1884/84,thickness_3 = 434/84,width_1 = 1242/84,width_3 = 881/84,grade_1 = 109/3314,grade_5 =121/3314,run_time_ratio = 86)

## The prediction is -14.66 tons average delta through per shift for the month of May
predict(lm_final, newdata = new,  interval="prediction")

## Prediction Plot delta-Throughput for Schuze's prediction (86%)
pred.int <- predict(lm_final, interval = "prediction") 

hist(d$delta_throughput)
abline(v= -14.7,col ="red")
###############

##8.- Provide 90% confidence interval for the average delta throughput for 90% interval
predict(lm_new, newdata = new,  interval="confidence", level = 0.9)
# Boundary level between -24.79 and -4.54

####################################################################
# Appendix Statistics ###

par(mfrow = c(2,2))
hist(main = "Run Time Ratio", xlab = "Run Time Ratio", d$run_time_ratio)
hist(main = "MPT", xlab = "MPT", d$MPT)
hist(main = "Total Strips", xlab = "Total Strips", d$Total_Strips)
hist(main = "Throughput", xlab = "Throughput", d$throughput)









