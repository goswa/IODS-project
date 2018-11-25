
# Read the "Human development" and "Gender inequality" datas into R. 

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

 
# Exploring the datasets 

# look at the (column) names of Human development and Gender inequality
names(hd)
names(gii)

# look at the structure of human development and Gender inequality

str(hd)
str(gii)

# print out summaries of the variables
summary(hd)
summary(gii)


# print out the column names of the data
colnames(hd)
colnames(gii)


# Modifying column names of Human development
colnames(hd)[3] <- "HD_index"
colnames(hd)[4] <- "life_exp"
colnames(hd)[5] <- "edu_years"
colnames(hd)[6] <- "edu_mean"
colnames(hd)[7] <- "GNI"
colnames(hd)[8] <- "GNI_HDI"

# printing out the modified column name
colnames(hd)

# Modifying column names of Human development
colnames(gii)[3] <- "GI Index"
colnames(gii)[4] <- "mat mor ratio"
colnames(gii)[5] <- "adol brth rate"
colnames(gii)[6] <- "Par_percent"
colnames(gii)[7] <- "secedu_f"
colnames(gii)[8] <- "secedu_m"
colnames(gii)[9] <- "labfor_f"
colnames(gii)[10] <- "labfor_m"

# printing out the modified column name
colnames(gii)

# Mutate the "Gender inequality" data and create two new variables. 
# The first one should be the ratio of Female and Male populations with secondary education in each country. (i.e. edu2F / edu2M). The second new variable should be the ratio of labour force participation of females and males in each country (i.e. labF / labM)

library(dplyr)
library(ggplot2)
gii <- mutate(gii, secedu_m_f = (secedu_f/secedu_m))
gii <- mutate(gii, labfor_m_f = (labfor_f/labfor_m))

# Join together the two datasets using the variable Country as the identifier.
library(dplyr)
join_by <- "Country"
new <- inner_join(hd, gii, by = join_by)

# see the new column names and glimpse of the data
colnames(new)
glimpse(new)

# Saving the joined and modified data set to the 'data' folder using write.csv function
write.csv(new, file = "human.csv", row.names=FALSE)

#reading the csv to make sure everything is in order
MyData <- read.csv(file="human.csv", header=TRUE, sep=",")
dim(MyData)

# The dataset has 195 observations of 19 variables.




