#Test FB


# 1. load libraries

library(UsingR)
library(jtools)


# Question Nr. 1 how do I know in the beginning which library I need? Since it is a good practice to do it in the beginning.

# 2. Import & view data
  library(readxl)
  ThyssenKrupp_PPL_Data_Final_20150513 <- read_excel("ThyssenKrupp_PPL Data_Final_20150513.xls", 
                                                     +     col_types = c("text", "text", "text", 
                                                                         +         "numeric", "numeric", "numeric", 
                                                                         +         "numeric", "numeric", "numeric", 
                                                                         +         "numeric", "numeric", "numeric", 
                                                                         +         "numeric", "numeric", "numeric", 
                                                                         +         "numeric", "numeric", "numeric", 
                                                                         +         "numeric", "numeric", "numeric", 
                                                                         +         "numeric", "numeric", "numeric"))
  View(ThyssenKrupp_PPL_Data_Final_20150513)
  
# 1. Perform a univariate analysis and answer the questions as follows:
# a) What is the average number of strips per shift
#   The number of strips per shift can be derived when summing up the number of different width per shift and then building the average
    
d <- ThyssenKrupp_PPL_Data_Final_20150513

d$strips <- c(d$`width 1`+d$`width 2`+d$`width 3`)
View(d)
df1 <- data.frame(d$strips, d$`shift 1`,d$`shift 2`, d$`shift 3`, d$`shift 4`, d$`shift 5` )
plot(df1)
d$shift2_help <- c(d$`shift 2`)
d$shift3_help <- c(d$`shift 3`)
d$shift4_help <- c(d$`shift 4`)
d$shift5_help <- c(d$`shift 5`)

d$shift2_help <- replace(d$shift2_help, d$shift2_help>0,2)
d$shift3_help <- replace(d$shift3_help, d$shift3_help>0,3)
d$shift4_help <- replace(d$shift4_help, d$shift4_help>0,4)
d$shift5_help <- replace(d$shift5_help, d$shift5_help>0,5)

d$shift_number <- c(d$`shift 1`+ d$shift2_help+ d$shift3_help+ d$shift4_help+ d$shift5_help)
View(d)

#here is the answer, all the stuff above is probably unnessecary because I thought we need the average for each shift (1-5)

mean(d$strips)

# b) strips of which thickness cluster are the most/least common?

sum(d$`thickness 1`)
sum(d$`thickness 2`) 
sum(d$`thickness 3`)

# Nr. 2 is

# c) descriptive statistics for delta throughput and RTR

summary(d$`delta throughput`)
summary(d$`run time ratio`)

# d) copy to a new object and tranform to logical. then form the sum and see if there are sums of just 1

df2 <- data.frame(d$`grade 1`,d$`grade 2`,d$`grade 3`,d$`grade 4`,d$`grade 5`,d$`grade rest`)
df2$grade.1 <- as.logical(df2$d..grade.1.)
df2$grade.2 <- as.logical(df2$d..grade.2.)
df2$grade.3 <- as.logical(df2$d..grade.3.)
df2$grade.4 <- as.logical(df2$d..grade.4.)
df2$grade.5 <- as.logical(df2$d..grade.5.)
df2$grade.rest <- as.logical(df2$d..grade.rest.)

df2$sum_of_grade_types <- as.numeric(c(df2$grade.1+df2$grade.2+df2$grade.3+df2$grade.4+df2$grade.5+df2$grade.rest))
hist(df2$sum_of_grade_types)
table(df2$sum_of_grade_types)

# Nr.2 RTR Theory: the lower the run time ratio, the higher the negative deviation from the plan

# for this we need to measure the deviation actual to plan

plot.ts(d$`run time ratio`,d$`delta throughput`)
lm1 <- lm(`delta throughput` ~ `run time ratio`, data=d)
summary(lm1)
plot(lm1)

effect_plot(model = lm1, pred = `run time ratio`, plot.points = TRUE, main ="Regression model: RTR Theory")

# there is a statistical significant positive correlation between the two variables and therefore the RTR Theory may explain some deviations.
# However, the R² value of ~ 30 % tells us that there are a lot of other factors that potentially affect the deviation. Hence it is not the single source of truth.


# Nr.3 MPT theory says that Material with a low thickness and/or a low width carried a lower weight per meter and hence
# took longer to process. Accordingly negative deviations in months with average or above RTR could be explained by this metric.
# Hence we need to plot thickness and width against RTR

# I need two additional column giving me the average width and thickness of the shift. then i can compare it to the delta ... Wait, maybe there is something else.

plot.ts(d$`run time ratio`,d$MPT)
plot.ts(d$`delta throughput`,d$MPT)

plot.ts(d$MPT,d$`delta throughput`)
lm2 <- lm(`delta throughput` ~ MPT, data=d)
summary(lm2)

# also seems to be reasonable

# 4. Develop a sound regression model
