# -----------------------------------------------------------------------------------------------
#Case Study I
#Group III
# -----------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------
# Solid as Steel: Production Planning at ThyssenKrupp
# -----------------------------------------------------------------------------------------------

# Import the data from an excel file
ThyssenKrupp_PPL_Data_Final_20150513 <- read_excel("C:/Users/D070724/OneDrive - SAP SE/Documents/SAP/Master/Week 1/Data Science for Business I/ThyssenKrupp_PPL Data_Final_20150513.xls")

#Rename dataset for usability
d <- ThyssenKrupp_PPL_Data_Final_20150513
d

#Take a first look at the available data
summary(d)
str(d)
View(d)

###Part A
#1 Univariate Analysis
#1a Average number of strips per shift

#1b Strips of which thickness are the most/least common

#1c Minimum, average, maximum values of delta throughput and RTR