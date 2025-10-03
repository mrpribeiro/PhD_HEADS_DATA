# 1. OVERALL IMPACT OF AIR POLLUTION
#   - How many deaths/DALYs worldwide were attributable to air pollution (2021)? 
#   - In which areas did it have the greatest impact? (map?) 
#   - In what type of health system?

# Install packages
# install.packages("readxl")
# install.packages("tidyr")
# install.packages("summarytools")
# install.packages("ggplot2")
# install.packages("dplyr")

# Load packages
library(readxl)
library(dplyr)
library(ggplot2)

# Read csv file (Import data)
gbd_data_1 <- read.csv("IHME-GBD_2021_DATA-11d67504-1.csv")
gbd_data_2 <- read.csv("IHME-GBD_2021_DATA-11d67504-2.csv")

# Keep a copy of the  original data
d1 <- gbd_data_1
d2 <- gbd_data_2

# Overview of d1
str(d1)
summary(d1)
head(d1)

# Overview of d2
str(d2)
summary(d2)
head(d2)

# Check unique values
unique(d1$measure)
unique(d1$location)
unique(d1$sex)
unique(d1$age)
unique(d1$cause)
unique(d1$rei)
unique(d1$metric)

unique(d2$measure)
unique(d2$location)
unique(d2$sex)
unique(d2$age)
unique(d2$cause)
unique(d2$rei)
unique(d2$metric)

# Combine both datasets
df_all <- rbind(d1,d2)

# Change chr type of data to factor (keep location as chr until done with joins/maps, and keep year as numeric for time trends) 
df_all$measure <- as.factor(df_all$measure)
df_all$sex <- as.factor(df_all$sex)
df_all$age <- as.factor(df_all$age)
df_all$rei <- as.factor(df_all$rei)
df_all$metric <- as.factor(df_all$metric)

summary(df_all)

# Frequency tables
View(table(df_all$measure))
View(table(df_all$location))
View(table(df_all$sex))
View(table(df_all$age))
View(table(df_all$cause))
View(table(df_all$rei))
