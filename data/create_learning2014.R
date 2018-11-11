# Shweta Goswami, 05/11/2018, Week 2 exercise 'Data Wrangling and analysis'
crt_learn <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)
dim(crt_learn)
str(crt_learn)
# 183 observations of 63 variables

# Variables gender, age, attitude, deep, stra, surf and points by combining questions in the learning2014 data

deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# Scale all combination variables to the original scales (by taking the mean).
library(dplyr)
deep_columns <- select(crt_learn, one_of(deep_questions))
crt_learn$deep <- rowMeans(deep_columns)
surface_columns <- select(crt_learn, one_of(surface_questions))
crt_learn$surf <- rowMeans(surface_columns)
strategic_columns <- select(crt_learn, one_of(strategic_questions))
crt_learn$stra <- rowMeans(strategic_columns)

# Exclude observations where the exam points variable is zero.
library(dplyr)
colnames(crt_learn)
keep_columns  <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points")
learning2014 <- select(crt_learn, one_of(keep_columns))
str(learning2014)

library(dplyr)
learning2014 <- filter(learning2014, Points > 0)
str(learning2014)


write.csv(learning2014, file = "learning2014.csv")
read.csv("learning2014.csv",check.names=FALSE)
str(learning2014)
head(learning2014)
