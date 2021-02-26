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
library(broom)
library(ggstance)

# Import the data from an excel file
ThyssenKrupp_PPL_Data_Final_20150513 <- read_excel("ThyssenKrupp_PPL Data_Final_20150513.xls")
ThyssenKrupp_Data <- readRDS("~/R/Case_Study_Data_Science_for_Business/ThyssenKrupp_Data.rds")

#Rename dataset for usability
d1 <- ThyssenKrupp_PPL_Data_Final_20150513
d1
View(d1)
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

#With sum of strips (based on numbers from width)
sum(d1$`width 1`, d1$`width 2`, d1$`width 3`)/nrow(d1)

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

e.data <- data.frame(d1$shift, d1$`shift type`, d.data$grade1, d.data$grade2, d.data$grade3, d.data$grade4, d.data$grade5)
e.data

#Rename
names(e.data)[1]<-"shift"
names(e.data)[2]<-"shift_type"
names(e.data)[3]<-"grade1"
names(e.data)[4]<-"grade2"
names(e.data)[5]<-"grade3"
names(e.data)[6]<-"grade4"
names(e.data)[7]<-"grade5"

#Filter rows only grade 1
filter(e.data, e.data$grade1 == TRUE &
       e.data$grade2 == FALSE &
       e.data$grade3 == FALSE &
       e.data$grade4 == FALSE &
       e.data$grade5 == FALSE)
#Only grade 2
filter(e.data, e.data$grade1 == FALSE &
         e.data$grade2 == TRUE &
         e.data$grade3 == FALSE &
         e.data$grade4 == FALSE &
         e.data$grade5 == FALSE)
#Only grade 3
filter(e.data, e.data$grade1 == FALSE &
         e.data$grade2 == FALSE &
         e.data$grade3 == TRUE &
         e.data$grade4 == FALSE &
         e.data$grade5 == FALSE)

#Only grade 4
filter(e.data, e.data$grade1 == FALSE &
         e.data$grade2 == FALSE &
         e.data$grade3 == FALSE &
         e.data$grade4 == TRUE &
         e.data$grade5 == FALSE)

#Filter rows only grade 5
grade5 <- filter(e.data, e.data$grade1 == FALSE &
         e.data$grade2 == FALSE &
         e.data$grade3 == FALSE &
         e.data$grade4 == FALSE &
         e.data$grade5 == TRUE)
summary(grade5)

#On entire data set
d3 <- d1
d3

d3[18] <- lapply(d3[18],as.logical)
d3[19] <- lapply(d3[19],as.logical)
d3[20] <- lapply(d3[20],as.logical)
d3[21] <- lapply(d3[21],as.logical)
d3[22] <- lapply(d3[22],as.logical)
head(d3)
View(d3)
grade_5 <- filter(d3, d1$`grade 1` == 0 &
         d3$`grade 2` == 0 &
         d3$`grade 3` == 0 &
         d3$`grade 4` == 0 &
         d3$`grade 5` == 1)
grade_5
View(grade_5)

grade_1 <- filter(d3, d3[18] == 1 & d3[19] == 0 & d3[20] == 0 & d3[21] == 0 & d3[22] == 0)
grade_1
View(grade_1)

#A.2 Can the RTR theory adequately explain the deviations from the planned productions figures?
#Explain why or why not.
#Positive correlation.


#Create data frame
d4 <- data.frame(d1$`run time ratio`, d1$throughput, d1$`delta throughput`)
names(d4)[1]<-"RTR"
names(d4)[2]<-"Throughput"
names(d4)[3]<-"Delta_Throughput"
d4
#Correlation: 0.5002921
cor(d4$RTR,d4$Throughput)
cor(d4$RTR,d4$Delta_Throughput)
summary(cor(d4$RTR,d4$Throughput))
# Compute the model: P-Value significant
lm1<- lm(d4$RTR ~ d4$Throughput)
summary(lm1)

#Confident Interval: Rejects 0 Hypothesis
confint(lm1)

#Plot values
plot(d4$RTR,d4$Throughput, xlab="RTR", col = 1:2, ylab="Throughput",main="Correlation between RTR and Throughput")

#A.2 Is the MPT theory sufficient?