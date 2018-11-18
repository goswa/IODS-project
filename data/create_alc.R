---
  
author: "Shweta Goswami"
date: "16-11-2018"
about: data wrangling exercise for week 3
Reference: "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/"
output: html_document

---
# reading csv files and exploring dim and str of student performance data

math <- read.csv("student-mat.csv", sep = ";" , header=TRUE)

por <- read.csv("student-por.csv" , sep = ";" , header=TRUE)

str(math)
dim(math)
colnames(math)

str(por)
dim(por)
colnames(por)

# joining two data sets using variables mentioned below as student identifiers.
library(dplyr)
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
math_por <- inner_join(math, por, by = join_by, suffix = c(".math", ".por"))
colnames(math_por)
str(math_por)
dim(math_por)

# The if-else structure
# print out the column names of 'math_por'
colnames(math_por)

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# columns that were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data
glimpse(alc)

#Taking the average of the answers related to weekday and weekend alcohol consumption
library(dplyr)
library(ggplot2)
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

#using 'alc_use' to create a new logical column 'high_use' which is TRUE for students for which 'alc_use' is greater than 2 (and FALSE otherwise).
alc <- mutate(alc, high_use = alc_use > 2)
glimpse(alc)
dim(alc)

# Saving the joined and modified data set to the 'data' folder using write.csv function
write.csv(alc, file = "create_alc.csv", row.names=FALSE)

#reading the csv to make sure everything is in order
MyData <- read.csv(file="create_alc.csv", header=TRUE, sep=",")
dim(MyData)
# The dataset has 382 observations of 35 variables.
