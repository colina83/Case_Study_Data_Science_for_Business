######## R Script A.Bürger
######## Case Study: "Solid as Steel: Production Planning at thysenkrupp"

# Load the data from Excel files
rm(list=ls())
getwd()
dCS<-ThyssenKrupp_PPL_Data_Final_20150513

# convert characters of weekday, shift type and binary variables for shift group into factors
dCS$weekday<-as.factor(dCS$weekday)
dCS$`shift type`<-as.factor(dCS$`shift type`)
dCS$`shift 1`<-as.factor(dCS$`shift 1`)
dCS$`shift 2`<-as.factor(dCS$`shift 2`)
dCS$`shift 3`<-as.factor(dCS$`shift 3`)
dCS$`shift 4`<-as.factor(dCS$`shift 4`)
dCS$`shift 5`<-as.factor(dCS$`shift 5`)

# first glance on the data
View(dCS)
str(dCS)
summary(dCS)

# plausibility checks:
# weekday: number of weekdays within 6 months is unequal (Tuesday:only 63 observations, Friday/Saturday/Sunday: 74 observations)
      # shifts were omitted by Schultze by missing data or obviously erroneous data.
# shift type: in the data set, there are more Midday shifts reported (174) than night shifts (170) or early shifts (156)


