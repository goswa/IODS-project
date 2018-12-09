---
title: "Analysis of Longitudinal Data"
author: "Shweta Goswami"
date: "8-12-2018"
output: html_document
---

# Analysis of Longitudinal Data

According to the instructions, the dataset has been swapped. The current analysis includes Chapter 8 of MABS using the RATS data and Chapter 9 of MABS using the BPRS data. 

### **Interpretation of the wide and long form data**

If we compare wide and long form of data, the wide form combines one of the keys with the value variables and their columns represent groups. The wide data has multiple columns for outcomes as we can see in RATS and BPRS original dataset. The long form data presents each subject's response in multiple rows and every row shows a response/observation that belongs to a particular variable. In simple terms, the long form represents columns of outcomes into a single response variable as evident in BPRSL and RATSL dataset. For the regression and ggplot functions, long form data is preferred.

## Analysis of RATS data


```{r, include=FALSE}
setwd("~/GitHub/IODS-project/data")
load("~/GitHub/IODS-project/data/RATSL.Rdata")
library(readr)
RATS <- read_csv("RATS.csv")

```

The RATS data in the current analysis is based on a nutrition study conducted in three groups of rats. Those three groups were put on different diets. Each rat's body weight in grams was recorded approximately on a weekly basis, except in week seven when two recordings were observed over a 9-week period. 

Question: Whether the growth profiles of the three groups differ?

Hypothesis: The growth profiles of three groups of rats differs with time.

### **Overview of RATSL (long form) data**

```{r, echo=FALSE}
library(dplyr)
head(RATSL)
summary(RATSL)
glimpse(RATSL)
```

The original data was in the wide form and had 16 obs. of  13 variables. After reshaping the data to long form, it has 176 obs. of 5 variables. The five variables are "ID", "Group", "WD", "Weight" and "Time". The minimum and maximum weight observed is 225 and 628 grams respectively. The time is observed in days ranging from 1-64.

### **Graphical overview of RATSL data**

```{r, echo=FALSE}
library(tidyr)
library(dplyr)
library(ggplot2)

# Draw the plot
ggplot(RATSL, aes(x = Time, y = Weight, linetype = ID)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ Group, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(RATSL$Weight), max(RATSL$Weight)))

ggplot(RATSL, aes(x = Time, y = Weight, group = ID))+ 
  geom_line(aes(linetype = Group))+ 
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 10)) + 
  scale_y_continuous(name = "Weight (grams)")+ 
  theme_bw() + theme(legend.position = "top")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

```

Graphically, there is an obvious difference in weight growth profile of individual rats in three groups. Group 1 seems to be under 300 grams. Group 2 lies between 400 to approx 600 grams. Group 2 seems to have highest growth and the graph is increasing with time. In Group 3, some subjects have higher growth profile but some seems to have decreased growth with time. Group 1 seems to have slight growth change throughout the study.

### **Graphical overview after standardization**

```{r, echo=FALSE}

# Standardise the variable 
RATSL <- RATSL %>%
  group_by(Group) %>%
  mutate(stdweight = (Weight - mean(Weight))/sd(Weight) ) %>%
  ungroup()

# Glimpse the data
glimpse(RATSL)

# Plot again 
ggplot(RATSL, aes(x = Time, y = stdweight, linetype = ID)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ Group, labeller = label_both) +
  scale_y_continuous(name = "standardized weight")

ggplot(RATSL, aes(x = Time, y = stdweight, group = ID))+ 
  geom_line(aes(linetype = Group))+ 
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 10)) + 
  scale_y_continuous(name = "standardized weight")+ 
  theme_bw() + theme(legend.position = "top")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


```


The rats who has more weight at the beginning seems to have increasing weight profile throughout the study.To see it in more clear way, standardizarion is done. The strandardization includes the values obtained by subtracting the relevant occasion mean from the original observation and then dividing by the corresponding visit standard deviation. There seems to be an increasing trend in all the three groups. Some rat's growth are increasing exponentially. Group 2 seems to have highest growth rate.




### **Plotting the mean profiles**


```{r, echo=FALSE}


n <- RATSL$Time %>% unique() %>% length()

# Summary data with mean and standard error  
RATSS <- RATSL %>%
  group_by(Group, Time) %>%
  summarise( mean = mean(Weight), se = sd(Weight)/sqrt(n) ) %>%
  ungroup()

# Glimpse the data
glimpse(RATSS)

# Plot the mean profiles
ggplot(RATSS, aes(x = Time, y = mean, linetype = Group, shape = Group)) +
  geom_line() +
  geom_point(size=3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, linetype="1"), width=0.3) +
  theme(legend.position = c(0.8,0.8)) +
  scale_y_continuous(name = "mean(Weight) +/- se(Weight)")

```

The graph mentioned above shows average profiles for three groups of rats along with some
indication of the variation of the observations at each time point. There seems to be no overlap in the mean profiles of the three groups of rats suggesting that there is obvious difference between
the three groups with respect to the mean weight profile.


### **Creating plot to check the outlier** 

```{r, echo=FALSE}
ggplot(RATSL, aes(x = Time, y = Weight, fill = Group))+ 
  geom_boxplot(position = position_dodge(width = 0.9))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = c(0.8,0.8))+
  scale_x_discrete(name = "time")
```

The plot suggests the presence of some possible outliers at a number of time points in all three groups and also shows overlap between second and third group of rats.

### **Creating boxplot of the mean vs group to check the outlier** 
```{r, echo=FALSE}


# Create a summary data 
RATSL8S <- RATSL %>%
  group_by(Group, ID) %>%
  summarise( mean=mean(Weight) ) %>%
  ungroup()

# Glimpse the data
glimpse(RATSL8S)

# Draw a boxplot of the mean versus group
ggplot(RATSL8S, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "green") +
  scale_y_continuous(name = "mean(Weight), time 1-64")


```



The graph shows box plots of the observations at each time point. 

The mean summary measure of the second group of rats shows more variation as compared to the other two groups. The boxplot of the second group shows an outlier whose mean weight is around 600. To avoid bias in further analyses, the outlier present in the second group will be removed. 

### **Filtering the outlier, adjusting the ggplot code the drawing the plot again**
```{r, echo=FALSE}
# Create a new data by filtering the outlier and adjust the ggplot code the draw the plot again with the new data
RATSL8S1 <- RATSL8S %>%
  filter(mean < 550)

# Draw a boxplot of the mean versus group
ggplot(RATSL8S1, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "red") +
  scale_y_continuous(name = "mean(Weight), time 1-60")


```

The boxplot mentioned above is without the outlier with respect to the second group and shows clear difference in location of the summary measure distributions in each group.

All the graphs mentioned above clearly indicates difference in the three groups of rats. To be certain, ANOVA will be taken into consideration.

### **Analysis of Variance with outliers**

```{r, echo=FALSE}


# Fit the linear model with the mean as the response 
fit <- lm(mean ~ Group, data = RATSL8S)
anova(fit)


```



### **Analysis of Variance after filtering outliers from Group 2**
```{r, echo=FALSE}
# Fit the linear model with the mean as the response after filtering outlier
fit1 <- lm(mean ~ Group, data = RATSL8S1)

# Compute the analysis of variance table for the fitted model with anova()

anova(fit1)
```


The objective of the ANOVA test is to analyse if there  is a (statistically) significant difference in weight growth between different groups. It is going to compare means of response among the different groups and check if differences are statistically significant. Two hypothesis are there in ANOVA:

*Null Hypothesis: All group means are equal and there is no difference among them.
          
*Alternative Hypothesis: All group means are NOT equal and there is difference among the three groups under observation.

The larger the F value, the greater the relative variance among the group means. The F-value including outliers and without outliers is 88.07 and 483.6 resp which shows the great relative variance among groups. The p-value is also less than 0.05 suggesting the differences among group statistically significant.

Hence the above mentioned hypothesis is true and growth profiles of three groups of rats differs with time.






## Analysis of BPRS data

```{r, include=FALSE}
load("~/GitHub/IODS-project/data/BPRSL.Rdata")
setwd("~/GitHub/IODS-project/data")
library(readr)
BPRS <- read_csv("BPRS.csv")

```

The second part of the analysis is based on BPRS data which constitutes 40 male subjects that were randomly assigned to one of two treatment groups. The subjects were rated on the Brief Psychiatric Rating Scale (BPRS). They were measured before treatment began i.e.week 0 and then at weekly intervals for eight weeks. 

The BPRS score shows the level of 18 symptom constructs, for instance, hostility, suspiciousness, hallucinations and grandiosity. The scale is rated from one (not present) to seven (extremely severe) and is used to investigate patients suspected of having schizophrenia.


Hypothesis: The BPRS score of the subjects decreases over the eight weeks of the study.

### **Overview of BPRSL (long form) data**

```{r, echo=FALSE}
head(BPRSL)
glimpse(BPRSL)
summary(BPRSL)
```



The original data was in the wide form and had 40 obs. of  11 variables. After reshaping the data to long form, it has  360 obs. of  5 variables. The five variables are "treatment", "subject", "weeks", "bprs" and "week". The minimum and maximum bprs observed is 18 and 95 respectively. The time is observed in weeks ranging from 0-8.


### **Plotting the BPRSL data**

Plot of bprs score against time (week) for BPRS data

```{r, echo=FALSE}



# Plot the BPRSL data

p1 <- ggplot(BPRSL, aes(x =week, y = bprs, group = subject))
p2 <- p1 + geom_text(aes(label = treatment))
p3 <- p2 + scale_x_continuous(name = "Time (week)")
p4 <- p3 + scale_y_continuous(name = "bprs (score)")
p5 <- p4 + theme_bw()
p6 <- p5 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p6



ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))


```


There seems to be overlapping between two treatments in the first plot. In the second plot, there seems to have decreasing trend in bprs score over the 8 weeks of time. 

### **Creating a regression model**

```{r, echo=FALSE}


# create a regression model RATS_reg
BPRS_reg <- lm(bprs ~ week + treatment, data = BPRSL)

# print out a summary of the model
summary(BPRS_reg)

summary(BPRS_reg)$coefficient

```


A multiple linear regression model is fitted with bprs score as response variable and week and treatment as explanatory variables.

Tt can be seen that p-value of the F-statistic is < 2.2e-16, which is highly significant. It suggests that one of the explanatory variables is significantly related to the outcome variable. 

The coefficient shows the amount of changes in the response variable when independent variables change one unit. With every increase in the variable week, the bprs score will decrease significantly.


### **Random intercept model for the same two explanatory variables**

```{r, echo=FALSE}


# access library lme4
library(lme4)

# Create a random intercept model
BPRS_ref <- lmer(bprs ~ week + treatment + (1 | subject), data = BPRSL, REML = FALSE)

# Print the summary of the model
summary(BPRS_ref)


```

The variance in random factor tells how much variability there is between subjects across the treatments.
The estimated variance of the bprs random effects is 47.41, indicating variation in the intercepts of the regression fits of the subject's bprs scores. The regression parameters for week and the treatment2 (treatment1 as reference) are similar to those in the previous regression model. The estimated standard error of treatment2 is slightly smaller than the previous regression model whereas it is almost similar with respect to week. Overall, the previous regression model and the random intercept model shows almost similar results.

### **Random intercept and random slope model**

```{r, echo=FALSE}


# create a random intercept and random slope model
BPRS_ref1 <- lmer(bprs ~ week + treatment + (week | subject), data = BPRSL, REML = FALSE)

# print a summary of the model
summary(BPRS_ref1)



```

Fitting the random intercept and random slope model to the BRPS data, here the linear regression fits for each subject to differ in slope. The t-value with respect to week is larger as compared to the previous random intercept. The corelation of fixed effect has also reduced in the slope model. The other observation with respect to random intercept and random intercept and slope model seems to be similar.

### **ANOVA test on two models (BPRS_ref, BPRS_ref1)**

```{r, echo=FALSE}
# perform an ANOVA test on the two models
anova(BPRS_ref, BPRS_ref1)
```

This is to test regression significance. The likelihood ratio test for the random intercept model versus the random intercept and slope model gives a chi-squared statistic of 7.27 with 2 degrees of freedom
the associated p-value is also significant. The random intercept and slope model provides a better fit for the BPRS data.

### **Random Intercept and Random Slope Model with interaction**

```{r, echo=FALSE}
# dplyr, tidyr, lme4, ggplot2, RATS and RATSL are available

# create a random intercept and random slope model
BPRS_ref2 <- lmer(bprs ~ week * treatment + (week | subject), data = BPRSL, REML = FALSE)

# print a summary of the model
summary(BPRS_ref2)







```

Fitting a random intercept and slope model that allows for a week × treatment interaction. 

The estimated regression parameters shows the interaction between week and treatment (0.71). To see the regression significance, let's do ANOVA test again.

### **ANOVA test on two models (BPRS_ref2, BPRS_ref1)**

```{r, echo=FALSE}
# perform an ANOVA test on the two models
anova(BPRS_ref2, BPRS_ref1)
```


The likelihood ratio test of the interaction random intercept and slope model against the corresponding
model without an interaction is 3.17 with 1 DF,  the associated p-value is not significant and we can conclude that the random intercept model without interaction provides a better fit for the BPRS data.


### **Plotting fitted values of BPRSL
**
```{r, echo=FALSE}

# draw the plot
ggplot(BPRSL, aes(x = week, y = bprs, group = treatment)) +
  geom_point(aes(linetype = treatment)) +
  geom_smooth(col = "pink") +
  scale_x_continuous(name = "weeks") +
  scale_y_continuous(name = "bprs") +
  theme(legend.position = "top")
  

# Create a vector of the fitted values
Fitted <- fitted(BPRS_ref1)

# Create a new column fitted to BPRSL
BPRSL <- BPRSL %>%
  mutate(Fitted)

# draw the plot of BPRSL

ggplot(BPRSL, aes(x = week, y = Fitted, group = treatment)) +
  geom_point(aes(linetype = treatment)) +
  geom_smooth(col = "pink") +
  scale_x_continuous(name = "weeks") +
  scale_y_continuous(name = "Fitted bprs") +
  theme(legend.position = "top")

ggplot(BPRSL, aes(x = week, y = Fitted, linetype = treatment)) +
  geom_point() +
  stat_smooth(method = "lm", col = "pink") +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))
  
```

The graph shows almost similar decreasing trend in both treatment 1 & 2. 

Hence, we can conclude from the above analysis that the bprs score of all of the subjects under observation seems to decrease over 8 weeks of time.

### **References:**

https://campus.datacamp.com/courses/helsinki-open-data-science/48773?ex=14

https://github.com/KimmoVehkalahti/MABS/tree/master/Examples

https://mooc.helsinki.fi/course/view.php?id=158&lang=en#section-6

Analysis of longitudinal data ("MABS for IODS") Selected excerpts of the forthcoming book (Vehkalahti and
Everitt, 2019)