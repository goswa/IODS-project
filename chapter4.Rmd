---
title: "Clustering and Classification"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



## **INTRODUCTION**

* This analysis is based on *Boston* Housing Values in Suburbs of Boston, United States. The data was taken from the **MASS** package. The data attributes reflect various aspects realted to housing values incuding including per capita crime rate by town, average number of rooms per dwelling, index of accessibility to radial highways etc. More information about data attributes can be found at https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/Boston.html.

* There are 506 obs. of  14 variables. Here is a description of the data attributes:

```{r, echo=FALSE}
# access the MASS package
library(MASS)

# load the data
data("Boston")

# explore the dataset
str(Boston)



```

### Attribute type

* The data type of each attribute in BOSTON dataset.


```{r, echo=FALSE}
sapply(Boston, class)
```


## Summaries of the variables 

```{r, echo=FALSE}

summary(Boston)

```

* The table mentioned above listed a breakdown of each attribute. The numerical attributes are divided into Min, 25th percentile, Median, Mean, 75th percentile, Max. As we can from the table, the attribute varies a lot between min and max ranging from 0.00 to 711. 


## Graphical overview of the data.
```{r, echo=FALSE}
library(MASS)
library(corrplot)
library(tidyverse)

cor_matrix<-cor(Boston) %>% round(digits = 2)

cor_matrix

corrplot(cor_matrix, method="circle", type="upper", cl.pos="b", tl.pos="d", tl.cex = 0.6)
```
* The table shows the attribute corelation. Any deviation from 0 shows positive or negative corelation. Values above 0.75 shows a high corelation. The positive corelation (0.91) seems to be in between tax (full-value property-tax rate per \$10,000) and rad (index of accessibility to radial highways). On the contrary, negative corelation (-0.77) appears to be in between nox (nitrogen oxides concentration (parts per 10 million) and dis(weighted mean of distances to five Boston employment centres).

* Graphically, the red and blue dot represents negative and positive corelation respectively. The corelation gets strong with the larger dot in the plot. The attributes tax (full-value property-tax rate per \$10,000) and rad (index of accessibility to radial highways) shows strong positive corelation. 

### Standardizing the dataset and summarizing the scaled data. 

```{r, echo=FALSE}
#Standardizing  the dataset and printing out summaries of the scaled data. 
library(MASS)
boston_scaled <- scale(Boston)
summary(boston_scaled)
class(boston_scaled)
boston_scaled <- as.data.frame(boston_scaled)




```
* Standardizing the dataset involves scaling the original variables to have equal range. As mentioned earlier, the attributes varied a lot ranging from 0.00 to 711. After scaling the original dataset, the attributes now have approximate values with a mean of 0.

### Creating a categorical variable of the crime rate in the Boston dataset


```{r, echo=FALSE}

# Create a categorical variable of the crime rate in the Boston dataset (from the scaled crime rate).


# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)


# create a categorical variable 'crime'
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))

# look at the table of the new factor crime
table(crime)

# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)

# add the new categorical value to scaled data
boston_scaled <- data.frame(boston_scaled, crime)

# number of rows in the Boston dataset 
n <- nrow(boston_scaled)

# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)

# create train set
train <- boston_scaled[ind,]

# create test set 
test <- boston_scaled[-ind,]

# save the correct classes from test data
correct_classes <- test$crime

# remove the crime variable from test data
test <- dplyr::select(test, -crime)


```


### Linear discriminant analysis 
```{r, echo=FALSE}


# linear discriminant analysis
lda.fit <- lda(crime ~ ., data = train)

# print the lda.fit object
lda.fit

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "orange", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 1)



```

* 


```{r, echo=FALSE}
# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)

```


```{r, echo=FALSE}
# access the MASS package
library(MASS)

# load the data
data("Boston")

boston_scaled <- scale(Boston)
boston_scaled <- as.data.frame(boston_scaled)

# euclidean distance matrix
dist_eu <- dist(boston_scaled)

# look at the summary of the distances
summary(dist_eu)

# manhattan distance matrix
dist_man <- dist(boston_scaled, method = 'manhattan')

# look at the summary of the distances
summary(dist_man)






```

```{r, echo=FALSE}

set.seed(123)

# determine the number of clusters
k_max <- 10

# calculate the total within sum of squares
twcss <- sapply(1:k_max, function(k){kmeans(boston_scaled, k)$tot.withinss})

# visualize the results
library(ggplot2)
qplot(x = 1:k_max, y = twcss, geom = 'line')


```


```{r, echo=FALSE}
# k-means clustering
km <-kmeans(boston_scaled, centers = 2)

# plot the Boston dataset with clusters
pairs(boston_scaled, col = km$cluster)


```

