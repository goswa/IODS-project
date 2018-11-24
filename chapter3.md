
---
title: "**Student performance including alcohol consumption**"
author: "Shweta Goswami"
date: "17-11-2018"
output:
  html_document: default
  pdf_document: default
---

# **Logistic Regression**

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
### **1. Introduction**:
* The data used in analysis was based on student achievement in secondary education of two Portuguese schools. Student grades, demographic, social and school related features were collected by using school reports and questionnaires.Two datasets were provided to see the performance in Mathematics and Portuguese language. The Mathematics and Portuguese core classes were modeled using binary/five-level classification and regression tasks.


```{r, echo=FALSE}
alc_data <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt", sep=",", header=TRUE)

```


```{r, echo=FALSE}
dim(alc_data)
colnames(alc_data)
```
* 
It has 382 observations of 35 variables. The more detailed description of data attributes for both math and portuguese language course can be found at https://archive.ics.uci.edu/ml/datasets/Student+Performance.

* The two variables alc_use and high_use were added in the the dataset. Alc_use was made using the average of 'Dalc' and 'Walc'. 'Dalc' and 'Walc' numerical attributes were referred to workday and weekend alcohol consumption (1 - very low to 5 - very high). Attribute high_use was true if alc_use is greater than 2 and false otherwise.


### **2. Selecting 4 variables from the data and presenting their relationships with student's alcohol consumption.** 

* Absences, studytime, age and goout were the 4 selected attributes.

1. The absences attribute referred to number of school absences ranging from 0 to 93. My hypothesis is students having more absences tend to have more alcohol consumption.

2. The studytime attribute referred to student's weekly study time and classified as 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours.  My hypothesis is students spending less time on studies tend to have more alcohol consumption.

3. The age attribute referred to student's age ranging from 15 to 22. My hypothesis is students with more age tend to have more alcohol consumption.

4. The goout attribute referred to going out with friends ranging from 1 - very low to 5 - very high. My hypothesis is students who has frequent outings are more inclined towards alcohol use.

### **3. Numerically and graphically exploring the distributions of chosen variables and their relationships with alcohol consumption.** 



```{r, echo=FALSE}
  
library(ggplot2)
g <- ggplot(alc_data, aes(x = high_use, y = absences))
g + geom_boxplot() + ggtitle("Relation between school absent and alcohol consumption among students")

```

* Similar to my above mentioned hypothesis,the students who had more absences tended to consume more alcohol.


```{r, echo=FALSE}


library(ggplot2)
g <- ggplot(alc_data, aes(x = high_use, y = studytime))
g + geom_boxplot() + ggtitle("Relation between weekly study time and alcohol consumption among students")



library(ggplot2)
ggplot(alc_data, aes(x= studytime, fill = high_use)) + geom_bar(position = "fill") +ggtitle("Relation between weekly study time and alcohol consumption among students")


```

```{r, echo=FALSE}
library(dplyr)
library(tidyr)
alc_data %>%
  group_by(studytime, high_use) %>%
  summarise(count = n())
```

* The students who has spent less time on studies tended to have more alcohol consumption is in afreement with my hypothesis.


```{r, echo=FALSE}



library(ggplot2)
g <- ggplot(alc_data, aes(x = high_use, y = age))
g + geom_boxplot() + ggtitle("Relation between age and alcohol consumption among students")


```
```{r, echo=FALSE}
library(dplyr)
library(tidyr)

alc_data %>%
  group_by(age, high_use) %>%
  summarise(count = n())


```

* Students with more age tended to have more alcohol consumption. This is in line with my hypothesis.


```{r, echo=FALSE}



library(ggplot2)
g <- ggplot(alc_data, aes(x = high_use, y = goout))
g + geom_boxplot() + ggtitle("Relation between outing and alcohol consumption among portuguese students")


library(dplyr)
library(tidyr)
alc_data %>%
  group_by(goout, high_use) %>%
  summarise(count = n())


```

* Students who had frequent outings tended to have more alcohol consumption and in line with my hypothesis.

### **4. Using logistic regression to statistically explore the relationship between the chosen variables and high_use as the target variable. Interpreting suumary of the fitted model.**

```{r, echo=FALSE}
m <- glm(high_use ~ absences + studytime + age + goout, data = alc_data, family = "binomial")
summary(m)
coef(m)

```

* A logistic regression model helps to determine a relationship between a binary outcome variable and a group of predictors.The output here has shown the coefficient, SE, Wald z-statistic, p-values. The attributes 'absences', 'studytime' and 'goout' were statisitcally significant. The coefficient gives change in the log odd of the output with a one unit increase in the predictor. For every one unit change in student absences, the log odds of alcohol consumption increases by o.o5. For every unit increase in studytime, the log odds of alcohol consumption changes by -o.6. For a one unit increase in age, the log odds of alcohol consumption increases by 0.07 but the relation is not statistically significant. The attribute goout shows log odds of alcohol consumption increases by 0.07 with a one unit change.

```{r, echo=FALSE}
OR <- coef(m) %>% exp
CI <- confint(m) %>% exp
cbind(OR, CI)


```

* If odds ratio is greater than 1 than the event is more likely to happen with increase in the predictor variable. On the contrary, if it is less than 1, the event is less likely to occur with increase in the predictor variable. With a one unit increase in goout , the odds of consuming alcohol increases by a factor of 2.06. Similarly, the odds are more than 1 with 'absences' and 'age', thus, the event is more likely to occur with a one unit increase in predictor variable. On the contrary, the odds of studytime is less than 1  which means the alcohol consumption among students is less likely to occur as the studytime increases.

* The output here is in line with the previously stated hypothesis but the variable "age" is not statistically signicant. 

### **5. Using variables that according to the logistic regression mode had a statistical relationship with alcohol consumption.** 

```{r, echo=FALSE}
probabilities <- predict(m, type = "response")
alc_data <- mutate(alc_data, probability = probabilities)
alc_data <- mutate(alc_data, prediction = probability > 0.5)
select(alc_data,  absences, studytime, goout, high_use, probability, prediction) %>% tail(10)
table(high_use = alc_data$high_use, prediction = alc_data$prediction)
```

* Number of true positive (model predicts the positive class) results were 46, true negative (model predicts the negative class) were 24, false positive (model incorrectly predicts the positive class) were 66 and false negative (model incorrectly predicts the negative class) were 246. 



```{r, echo=FALSE}



library(dplyr)
library(ggplot2)


g <- ggplot(alc_data, aes(x = probability, y = high_use, col = prediction))
g + geom_point()
table(high_use = alc_data$high_use, prediction = alc_data$prediction) %>% prop.table %>% addmargins

```

*The output here has shown the true negative and false positive rates as 17% and 6% respectively. The model has accurately predicted 12% of the observations.

### **6. Performing 10-fold cross-validation on the model.**

```{r, echo=FALSE}
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}
loss_func(class = alc_data$high_use, prob = alc_data$probability)
```

```{r, echo=FALSE}
library(boot)
cv <- cv.glm(data = alc_data, cost = loss_func, glmfit = m, K = 10)
cv$delta[1]

```

* The model seemed to have better test set performance compared to the model introduced in datacamp which had about 0.26 error. 

### **7. References**

https://archive.ics.uci.edu/ml/datasets/Student+Performance

https://campus.datacamp.com/courses/helsinki-open-data-science/logistic-regression?ex=7

http://www3.dsi.uminho.pt/pcortez/student.pdf


