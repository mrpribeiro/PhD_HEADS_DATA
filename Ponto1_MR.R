# 1. OVERALL IMPACT OF AIR POLLUTION
#   - How many deaths/DALYs worldwide were attributable to air pollution (2021)? 
#   - In which areas did it have the greatest impact? (map?) 
#   - In what type of health system?

# Install packages
# install.packages("readxl")
# install.packages("tidyr")
# install.packages("summarytools")
# install.packages("ggplot2")

# Load packages
library(readxl)
# library(dplyr)
# library(ggplot2)

# Read csv file (Import data)
gbd_data_1 <- read.csv("IHME-GBD_2021_DATA-11d67504-1.csv")
gbd_data_2 <- read.csv("IHME-GBD_2021_DATA-11d67504-2.csv")

# Keep a copy of the  original data
d1 <- gbd_data_1
d2 <- gbd_data_2
