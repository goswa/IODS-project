# Shweta Goswami
# Analysis of longitudinal data
# Data Wrangling of BPRS and RATS
# 08/12/2018

# Reading the BPRS data
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)

# Reading the RATS data
# read the RATS data
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')

# Looking at the (column) names of BPRS and RATS
names(BPRS)
names(RATS)

# Looking at the structure of BPRS and RATS
str(BPRS)
str(RATS)

# print out summaries of the variables
summary(BPRS)
summary(RATS)

#Glimpse of the dataset BPRS and RATS
library(dplyr)
glimpse(BPRS)
glimpse(RATS)

#Saving the original data to the data folder
write.csv(BPRS, file = "BPRS.csv", row.names=FALSE)
write.csv(RATS, file = "RATS.csv", row.names=FALSE)

# Brief summary of BPRS and RATS datasets
# The dataset BPRS has 40 obs. of  11 variables
# The dataset RATS has 16 obs. of  13 variables
# As obvious from the variables structure, the wide form of data presented response variable which is measured on each subject on several 
# different occasions in a single row.
# So, the wide form means presenting data horizontally. 

#Converting the categorical variables of both data sets to factors. 
#BPRS dataset
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

#RATS dataset
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

#Converting the data sets to long form. 

#BPRS data
library(dplyr)
library(tidyr)
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)

#RATS data
RATSL <- RATS %>% gather(key = WD, value = Weight, -ID, -Group) 



#Adding a week variable to BPRS and a Time variable to RATS
#BPRS data
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks,5,5)))

#RATS data
RATSL <-  RATSL %>% mutate(Time = as.integer(substr(WD,3,4)))

# comparing long form with their wide form versions: 

# Looking at the (column) names of BPRSL and RATSL
names(BPRSL)
names(RATSL)

# Looking at the structure of BPRSL and RATSL
str(BPRSL)
str(RATSL)

# print out summaries of the variables
summary(BPRSL)
summary(RATSL)

#Glimpse of the dataset BPRSL and RATSL
library(dplyr)
glimpse(BPRSL)
glimpse(RATSL)


# Interpretation of the wide and long form data:
# If we compare wide and long form data, 
# The wide form combines one of the keys with the value variables and their columns represent groups.
# The wide data has multiple columns for outcomes as we can see in RATS and BPRS dataset
# The long form data presents each subject's response in multiple rows
# And every row shows a response/observation that belongs to a particular variable.
# Simplified, the long form represents columns of outcomes into a single response variable as evident in BPRSL and RATSL dataset.
# Thus, for regression function, long form data is preferred.
# In the current dataset, 
# the wide form data of BPRS has 40 obs. of  11 variables
# the long form of BPRS has 360 obs. of  5 variables
# the wide form data of RATS has 16 obs. of  13 variables
# the long form of RATS has 176 obs. of  5 variables




# Saving the modified data set to the 'data' folder 
save(BPRSL, file = "BPRSL.RData")
save(RATSL, file = "RATSL.RData")

