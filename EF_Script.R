# -----------------------------------------------------------------------------------------------
#Case Study I
#Group III
# -----------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------
# Solid as Steel: Production Planning at ThyssenKrupp
# -----------------------------------------------------------------------------------------------

#Import libraries/Install packages
library(Hmisc)
library(jtools)
library(UsingR)
library(arsenal)
library(dplyr)
library(stringr)

# Import the data from an excel file
ThyssenKrupp_PPL_Data_Final_20150513 <- read_excel("ThyssenKrupp_PPL Data_Final_20150513.xls")
ThyssenKrupp_Data <- readRDS("~/R/Case_Study_Data_Science_for_Business/ThyssenKrupp_Data.rds")

#Rename dataset for usability
d1 <- ThyssenKrupp_PPL_Data_Final_20150513
d1
d2 <- ThyssenKrupp_Data
d2

#Check whether data is the same
comparedf(d1,d2)
summary(comparedf(d1,d2))

#Take a first look at the available data
summary(d1)
str(d1)
View(d1)

###Part A

#A.1 Univariate Analysis

#A.1a Average number of strips per shift
#Optimize Data for Part A.1a
#Assumption: Length of a strip = 20 meters
strips <- c((d1$throughput*d1$MPT)/20)
a.data <- data.frame(d1$throughput, d1$MPT, strips)
a.data
#Solution:
strips_per_shift <- sum(a.data$strips)/nrow(a.data)
strips_per_shift

#A.1b Strips of which thickness are the most/least common
b.data <- data.frame(d1$`thickness 1`, d1$`thickness 2`, d1$`thickness 3`)
b.data
barplot(height = c(sum(d1$`thickness 1`), sum(d1$`thickness 2`), sum(d1$`thickness 3`)), 
        names.arg = c("Thickness 1", 
                      "Thickness 2", "Thickness 3"),
        ylab = "Sum",
        xlab = "Category",
        main = "Frequency of Thickness")

#A.1c Minimum, average, maximum values of delta throughput and RTR
#Optimize data frame
c.data <- data.frame(d1$`delta throughput`, d1$`run time ratio`)
c.data
summary(c.data)
#Delta throughput (Min: -661.83, Average: 15.79, Max: 730.28)
#RTR (Min: 21.67, Average: 93.54, Max: 100.00)

#A.1d Are there shifts during which only steel of grade 1 or 
#steel only of grade 2 is processed?
#Optimize data frame
d.data <- data.frame(d1$`grade 1`, d1$`grade 2`, d1$`grade 3`, d1$`grade 4`, d1$`grade 5`)
d.data
#Convert all to logical
cols <- sapply(d.data, is.numeric)
cols
d.data[,cols] <- lapply(d.data[,cols],as.logical)
head(d.data)
#Filter rows