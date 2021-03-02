Website
https://casestudy1.tiiny.site/

---
title: "Production Planning at Thyssenkrupp Case Study"
output:
html_document:
df_print: paged
---
```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE)
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
library(knitr)
library(readxl)

#Imports the data
ThyssenKrupp_PPL_Data_Final_20150513 <- read_excel("ThyssenKrupp_PPL Data_Final_20150513.xls")
d <- ThyssenKrupp_PPL_Data_Final_20150513
names(d) <- gsub(" ", "_", names(d))
```

# Business Setting

The case study presented in this document, is the analysis of a particular problem experienced by Steel Manufacturer **Thyssenkrupp**, the manufacturer has struggled to keep up with their planned throughput (tons produced per unit time), and the variations of the expected production (delta throughput), which historically data for the last 10 years shows that the delta throughput is negative, meaning that the production line is constantly underperfoming.

The ruling theory that explains the deviation is the RTR (Run Time Ratio) theory, the lower the run ratio, the higher the negative deviation from the plan. The production personnel gut feeling was that a specific metric of the structure, the ratio *meters per ton* MPT, could explain the negative deviations in months with average or above-average RTR, therefore, this metric simply stated that even if the plant is optimally running, due to the properties of the material, it took longer to put one ton of material through the production line if the process speed remain constant.

The data gathered by Schultz, the manager writing the case study, contains the available past production data beginning with the night shift October 1, 2013, up until the early shit on April4, 2014. 





## 1.Perform a univariate analysis and answer the following questions:

a.- What is the average number of strips per shift? 

```{r, question_1a}
d$Total_Strips <- (d$thickness_1+d$thickness_2+d$thickness_3)
Average_strips <- mean(d$Total_Strips)
Average_strips
```
b.- Strips of which thickness cluster are the most common, and strips of which thickness cluster are the least common?

Thickness 2 is the most common strip, more than 54% of the total strips, and Thickness 3 is the least common

```{r, question_1b}
a = sum(d$thickness_1)/sum(d$Total_Strips)*100
b = sum(d$thickness_2)/sum(d$Total_Strips)*100
c = sum(d$thickness_3)/sum(d$Total_Strips)*100

barplot(c(a,b,c), b,col=c("darkblue","lightblue","gray"), main = "Thickness Values", xlab = "Thickness", legend = c("Thickness 1", "Thickness 2", "Thickness 3"))

```
c.- What are the minimum, average, and maximum values of delta throughput and RTR?

```{r, question_1c}
par(mfrow =c(1,2))
boxplot(summary(d$run_time_ratio))
boxplot(summary(d$delta_throughput))


Delta_Throughput_Summary <- summary(d$delta_throughput)
Run_Time_Ratio <- summary(d$run_time_ratio)
kable(rbind(Delta_Throughput_Summary,Run_Time_Ratio))
```

d.- Are there shifts during which the PPL processes strips of only steel grade 1, or of only steel grade 2, etc.?

Only a few shifts produce a 100% type Grade 1, 4, 5 and rest

```{r, question_1d}
grade_1<- table(d$grade_1 == 100)
grade_2 <- table(d$grade_2 == 100)
grade_3<- table(d$grade_3 == 100)
grade_4 <- table(d$grade_4 == 100)
grade_5 <- table(d$grade_5 == 100)
grade_rest <- table(d$grade_rest == 100)

grade_table <- data.frame(rbind(grade_1[2],grade_4[2],grade_5[2],grade_rest[2]))
row.names(grade_table) <- c("Grade 1","Grade 4","Grade 5","Grade Rest")
kable(grade_table %>% rename("Number of Shifts with 100% Grade" = TRUE.))
```

##2. Can the RTR theory adequately explain the deviations from the planned production figures? Explain why or why not.

We can support the theory: 
- Statistically significant positive correlation between RTR and delta throughput (0.5568)
- The linear regression model shows a positive correlation between the RTR and delta throughput,
  meaning that when a shift is run efficiently we no production problems, the delta throughput is high
- When we add a color classifier based on an MPT value, it is clear that high MPT values correlates to RTR

```{r, question_2}

valcol <- (d$MPT + abs(min(d$MPT)))/max(d$MPT + abs(min(d$MPT)))
plot(d$delta_throughput ~ d$run_time_ratio, xlab="RTR", ylab="Delta Throughput", main="Scatterplot Delta Througput", col = rgb(0, 0, valcol))
abline(h= 0,col ="black")
lm_dt <- lm(d$delta_throughput ~ d$run_time_ratio)
abline(lm_dt,col = "red")
abline(h= 0,col ="black") # Points below zero 
summary(lm_dt)


```

## 3.Is the MPT theory sufficient to explain the deviations? Explain why or why not.

MPT Theory: 
Material Structure is favorable/unfavorable > material with a low thickness or small width carries a lower weight per meter > it takes longer to put 1t of material through the PPL > high MPT figures as cause for high negative delta throughput for shifts with high RTR


We can support the theory:

- Negative correlation (-0.6673)

- If the MPT increases, the negative deviation is higher

- But some other variables causing additional deviation (R² = 0.4452) 

```{r, question_3}

plot(d$delta_throughput ~ d$MPT, xlab = "MPT", ylab = "Delta Throughput", main = "Scatterplot")
lm_mpt <- lm(d$delta_throughput ~ d$MPT)
abline(lm_mpt,col = "red")
```
### 4.Develop a sound regression model that can be used to predict delta throughput based on the characteris-tics of the strips scheduled for production.Include only explanatory variables that have a coefficient with a 10% level of significance. 

After further tests we arrive to the conclusion that using 3 thickness values and 2 grades will provide
the overall number of strips, and therefore, the regression analysis using the independent variables in our model below we can arrive to a model with a low P-Value and a good R-Square Adjusted, which means that we have a balance model according to the Breusch-Pagan Test, furthermore, we have a low heteroskedasticity.

```{r, question_4}

# Test Pass - Model with Grade 1 and Grade 5 (Final)

lm_final <- lm(delta_throughput ~ thickness_1+ thickness_2 + thickness_3 + width_1 + width_3 + grade_1  + grade_5 + run_time_ratio, data = d)
summary(lm_final)

# Plotting to see the dispersion between the fitted values (estimations) and the residuals (errors)
plot(fitted(lm_final),residuals(lm_final))


#Breusch-Pagan Test, small p-value, we can assume, we have to reject H:0 
bptest(lm_final)

# We are testing heteroskedasticity,heteroscedasticity is the absence of homoscedasticity.  
coeftest(lm_final, vcov = vcovHC(lm_final,"HC1"))

imcdiag(lm_final) # Low VIF's no correlation

```

### 5.Interpret the coefficient of RTR for the PPL and provide a 90% confidence interval for the value of the co-efficient (in the population).

We can say with confidence of 90 % that the coefficient for RTR lies within the interval of 4.78 to 6.02 tons.

This means that for each increase in percentage of RTR (efficiency), we see an increase in the delta throughput of 5.40 tons.

```{r, question_5}

confint(lm_final, level = 0.90)

```
## 6.A strip of thickness 1 and width 1 is replaced by a strip of thickness 3 and width 3. This change does not affect any other aspect of the production. Provide an estimate for the change in delta throughput

Calculation: 15.8708 + 3.4007 - 6.9680 + 6.4173 = 18.72 tons​

Solution: The estimate for the change delta throughput due to change in thickness and width (from 1 to 3) is 18.72 tons.

```{r, question_6}

delta_1_3 <-  15.8708 + 3.4007 - 6.9680 + 6.4173 
delta_1_3
```

## 7.- The table below shows the data provided by the production engineers. Because of major upcoming maintenance on the PPL, only 84 shifts were planned for the month of May. Provide an estimate for the average delta throughput per shift in May based on these estimated figures.

The prediction based on our model is a change of –14.66 tons as the average delta throughput per shift for the month of May

```{r, question_7}

new <- data.frame(thickness_1 = 996/84, thickness_2 = 1884/84,thickness_3 = 434/84,width_1 = 1242/84,width_3 = 881/84,grade_1 = 109/3314,grade_5 =121/3314,run_time_ratio = 86)

## The prediction is -14.66 tons average delta through per shift for the month of May
predict(lm_final, newdata = new,  interval="prediction")

## Prediction Plot delta-Throughput for Schuze's prediction (86%)
pred.int <- predict(lm_final, interval = "prediction") 

hist(d$delta_throughput)
abline(v= -14.7,col ="red")
###############

```

## 8.- 8.Provide a 90% confidence interval for the average delta throughput per shift in May.

According to our linear regression model we can assume with 90 % confidence that the average delta throughput per shift in May lies within the range of –24.79 and –4.54 tons. 

```{r, question_8}

predict(lm_final, newdata = new,  interval="confidence", level = 0.9)

```


## Appendix

```{r, Appendix}

# Appendix Statistics ###

par(mfrow = c(2,2))
hist(main = "Run Time Ratio", xlab = "Run Time Ratio", d$run_time_ratio)
hist(main = "MPT", xlab = "MPT", d$MPT)
hist(main = "Total Strips", xlab = "Total Strips", d$Total_Strips)
hist(main = "Throughput", xlab = "Throughput", d$throughput)

```



