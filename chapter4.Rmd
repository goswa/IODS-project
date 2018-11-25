---
title: "Clustering and Classification"
author: "Shweta Goswami"
date: "24-11-2018"
output:
  html_document: default
  pdf_document: default
---

# Clustering and classification

### **INTRODUCTION**

* This analysis is based on *Boston* Housing Values in Suburbs of Boston, United States. The data was taken from the **MASS** package. The data attributes reflect various aspects related to housing values incuding per capita crime rate by town, average number of rooms per dwelling, index of accessibility to radial highways etc. More information about data attributes can be found at https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/Boston.html.

* There are 506 obs. of  14 variables. Here is a description of the data attributes:

```{r, echo=FALSE}
# access the MASS package
library(MASS)

# load the data
data("Boston")

# explore the dataset
str(Boston)



```

### **Attribute type**

* The data type of each attribute in BOSTON dataset.


```{r, echo=FALSE}

sapply(Boston, class)

```


### **Summaries of the variables** 

```{r, echo=FALSE}

summary(Boston)

```

* The table mentioned above listed a breakdown of each attribute. The numerical attributes are divided into Min, 25th percentile, Median, Mean, 75th percentile, Max. As we can from the table, the attribute varies a lot between min and max ranging from 0.00 to 711. 


### **Attribute Corelation.**
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

### **Standardizing the dataset and summarizing the scaled data.**

```{r, echo=FALSE}
#Standardizing  the dataset and printing out summaries of the scaled data. 
library(MASS)
boston_scaled <- scale(Boston)
summary(boston_scaled)
class(boston_scaled)
boston_scaled <- as.data.frame(boston_scaled)




```


* Standardizing the dataset involves scaling the original variables to have equal range. As mentioned earlier, the attributes varied a lot ranging from 0.00 to 711. After scaling the original dataset, the attributes now have approximate values with a mean of 0.

### **Creating a categorical variable of the crime rate in the Boston dataset**


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


### **Linear discriminant analysis**

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

* The dataset is divided into train and test sets and 80% of the data belongs to the train set that is used in LDA.

* The linear discriminant analysis (LDA) helps to find the linear combinations of the original variables (13 variables after dropping crime rate variable from the dataset) that gives the possible separation between the groups (low, med_low, med_high, high) of the crime rate (target variable). 

* As mentioned earlier, the value of each discriminant function are standarized so that their mean value is zero. We can see the linear combination of the variables in the discriminant function, for instance, 0.11*zn + 0.04*indus + 0.20*medtv etc. 

* The proportion of trace represents percentage separation achieved by each discriminant function. As we can see, LD1, LD2 AND LD3 shows 95%, 37% and 12% of the variance respectively.

* Graphical depiction: 

              1.  There are four distinct groups with overlapping between low, med_low ad med_high. The 'high' category in the crime rate shows clear clustering. 
              
              2. The LDA biplot helps in interpretation with the projections of the points, the angles between the arrows, and the length of the arrows.The arrows in the biplot represent the variables. The longer arrows represents larger variation. 
              
              3. The 'rad' variable shows more variation than other variables. The angle between arrows represents the relationship between measures, for instance, the variables 'rad', 'zn' and 'vox' are not strongly related. 
              
              

### **Prediction**


```{r, echo=FALSE}


# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)

```

* Training a model and then presenting it with the test data to make predictions. The model appears to be accurate with the category "high" crime rate. The 'high' crime rate is clustered in the right middle of the biplot.

* Based on the earlier plot, low, med_low and med_high observations seems to be miscategorized. 

* Overall, the model seems to perform well with the testing set.

### **Calculating distances between the observations (Euclidean and Manhattan distance matrix)**

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


### **K-means algorithm**

```{r, echo=FALSE}

set.seed(123)

# determine the number of clusters
k_max <- 10

# calculate the total within sum of squares
twcss <- sapply(1:k_max, function(k){kmeans(boston_scaled, k)$tot.withinss})

# visualize the results
library(ggplot2)
qplot(x = 1:k_max, y = twcss, geom = 'line')

elbow <-data.frame(1:k_max, twcss)

ggplot(elbow, aes(x = 1:k_max, y = twcss)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(1, 10, by = 1))


```

* K Means Clustering is an unsupervised learning algorithm and helps to find pattern in the data. The number of clusters specified here are 10. 

* It seems from the graph that the optimal k is two where the curve starts to have a diminishing return. 

### **K means algorithm with two clusters and graphical visualization of clusters**

```{r, echo=FALSE}
# k-means clustering
km <-kmeans(boston_scaled, centers = 2)


# plot the Boston dataset with clusters
pairs(boston_scaled, col = km$cluster)

library(cluster) 
clusplot(boston_scaled, km$cluster, color=TRUE, shade=TRUE, 
  	labels=2, lines=0)

library(factoextra)
fviz_cluster(km, data = boston_scaled, ellipse.type = "norm", geom = "point",
             stand = FALSE) + theme_bw()

```

* As mentioned earlier, the optimal k is 2, so running the algorithm again with k equals to 2. 

* The first principle component accounts for 46.8% of the variation. The second principle component accounts for 11.8% of the variation. In total, they account for 58.6% of the variation. The first cluster are plotted with the red and the second with blue color. The largesr red circle and blue triangle represent cluster means. 

### **For bonus point**
```{r, echo=FALSE}
library (MASS)
boston_scaled <- scale(Boston)
boston_scaled <- as.data.frame(boston_scaled)
km <-kmeans(Boston, centers = 4)


# linear discriminant analysis
lda.fit <- lda( km$cluster~ ., data=boston_scaled)

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


# plot the lda results
plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 1)

library(factoextra)
fviz_cluster(km, data = boston_scaled, ellipse.type = "norm", geom = "point",
             stand = FALSE) + theme_bw()


```

* The tax, rad and black seems to be the most influential variables in the LDA plot. 

### **References**

https://campus.datacamp.com/courses/helsinki-open-data-science/clustering-and-classification?ex=11

https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/Boston.html

https://rpubs.com/Nolan/298913
