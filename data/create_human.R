
# Shweta Goswami
# Dimensional Reductional Techniques
# Data Wrangling of human data
# 29/11/2018


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
colnames(hd)[4] <- "Life.Exp"
colnames(hd)[5] <- "Edu.Exp"
colnames(hd)[6] <- "Edu_Mean"
colnames(hd)[7] <- "GNI"
colnames(hd)[8] <- "GNI_HDI"

# printing out the modified column name
colnames(hd)

# Modifying column names of Human development
colnames(gii)[3] <- "GI Index"
colnames(gii)[4] <- "Mat.Mor"
colnames(gii)[5] <- "Ado.Birth"
colnames(gii)[6] <- "Parli.F"
colnames(gii)[7] <- "Edu2.F"
colnames(gii)[8] <- "Edu2.M"
colnames(gii)[9] <- "Labo.F"
colnames(gii)[10] <- "Labo.M"

# printing out the modified column name
colnames(gii)

# Mutate the "Gender inequality" data and create two new variables. 
# The first one should be the ratio of Female and Male populations with secondary education in each country. (i.e. edu2F / edu2M). The second new variable should be the ratio of labour force participation of females and males in each country (i.e. labF / labM)

library(dplyr)
library(ggplot2)
gii <- mutate(gii, Edu2.FM = (Edu2.F / Edu2.M))
gii <- mutate(gii, Labo.FM = (Labo.F / Labo.M))

# Join together the two datasets using the variable Country as the identifier.
library(dplyr)
join_by <- "Country"
human <- inner_join(hd, gii, by = join_by)

# see the new column names and glimpse of the data
colnames(human)
glimpse(human)


# week 5 data wrangling

# Explore the structure and the dimensions of the data and describe the dataset briefly
dim(human)
str(human)

# The data used in analysis was based on human development report by United Nations Development Programme. The Human Development and Gender Inequality indices will be analysed. The dataset has combined indicators from different parts of the world. The Human development index comprises of health indicators, for instance, Gross National Income per capita, Life expectancy at birth, Maternal mortality ratio,  Adolescent birth rate etc. The Gender Inequality Index comprises of empowerment indicators like Proportion of males and females with at least secondary education, Proportion of females in the labour force etc. 
# Two new variables have been added in the "Gender inequality" data. The first one is the the ratio of Female and Male populations with secondary education in each country. (i.e. edu2F / edu2M). The second one is the ratio of labour force participation of females and males in each country (i.e. labF / labM).
# The dataset has 195 observations of 19 variables.
# More information about the dataset can be found at http://hdr.undp.org/en/content/human-development-index-hdi

# Mutate the data: transform the Gross National Income (GNI) variable to numeric 
str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric

# Excluding unneeded variables: 
library(dplyr)
keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
human <- select(human, one_of(keep))

# printing out a completeness indicator of the 'human' data
complete.cases(human)

# Remove all rows with missing values
human[!complete.cases(human),]
human[complete.cases(human), ] # Keep only the complete rows
human <- human[complete.cases(human), ] # Store the complete cases 
sum(complete.cases(human))

# filter out all rows with NA values
human <- filter(human, complete.cases(human))


# Remove the observations which relate to regions instead of countries. 
last <- nrow(human) - 7
human <- human[1:last, ]

# add countries as rownames
row.names(human) <- human$Country

# remove the country name column from the data.
human <- select(human, -Country)
dim(human)

# The dataset has 155 observations of 8 variables. 


# Saving the joined and modified data set to the 'data' folder using write.csv function
write.csv(new, file = "human.csv")






