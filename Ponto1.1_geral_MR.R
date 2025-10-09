##############################################################################
# How many deaths/DALYs worldwide were attributable to air pollution (2021)? #
##############################################################################

# Load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Read csv files (Import data)
gbd_1990 <- read.csv("IHME-GBD_2021_DATA-4835a3dc-1.csv")
gbd_2000 <- read.csv("IHME-GBD_2021_DATA-c56a3848-1.csv")
gbd_2010 <- read.csv("IHME-GBD_2021_DATA-1923af35-1.csv")
gbd_2020 <- read.csv("IHME-GBD_2021_DATA-d14075a8-1.csv")
gbd_2021 <- read.csv("IHME-GBD_2021_DATA-840155c6-1.csv")

# Combine both datasets
gbd_data <- rbind(gbd_1990,gbd_2000,gbd_2010,gbd_2020,gbd_2021)

# Overview
str(gbd_data)
summary(gbd_data)
head(gbd_data)

# Check unique values
unique(gbd_data$measure)
unique(gbd_data$location)
unique(gbd_data$sex)
unique(gbd_data$age)
unique(gbd_data$cause)
unique(gbd_data$rei)
unique(gbd_data$metric)

# Total deaths worldwide by year:
global_deaths <- gbd_data %>%
  filter(
    measure == "Deaths",
    location == "Global",
    rei == "Air pollution",
    metric == "Number",
    cause == "All causes",
    age == "All ages",
    sex == "Both"
  ) %>%
select(measure, year, val, upper, lower)

##### In 2021, aproximatelly 8 million people died due to air pollution, worldwide.

# Total DAILYs worldwide by year:
global_DAILYs <- gbd_data %>%
  filter(
    measure == "DALYs (Disability-Adjusted Life Years)",
    location == "Global",
    rei == "Air pollution",
    metric == "Number",
    cause == "All causes",
    age == "All ages",
    sex == "Both"
  ) %>%
  select(measure, year, val, upper, lower)

### In 2021, air pollution caused an estimated 236 million DALYs globally.
### That means roughly 236 million years of healthy life were lost across the global population due to disease and early death linked to air pollution — a huge public-health burden.

# PLOT: "Deaths attributable to air pollution worldwide (1990–2021)
ggplot(global_deaths, aes(x = year, y = val)) +
  geom_line(color = "steelblue", size = 1.2) +   # line
  geom_point(color = "darkred", size = 2) +      # points on the line
  labs(title = "Deaths attributable to air pollution worldwide (1990–2021)",
       x = "Year",
       y = "Total no. of deaths") +
  theme_minimal()

## PLOT: "Deaths attributable to air pollution worldwide (1990–2021) WITH UNCERTAINITY VALUES (SHADED AREA)
ggplot(global_deaths, aes(x = year, y = val)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.3) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Deaths attributable to air pollution worldwide (1990–2021)",
       x = "Year",
       y = "Total no. of deaths") +
  theme_minimal()

# PLOT: "DAILYs attributable to air pollution worldwide (1990–2021)
ggplot(global_DAILYs, aes(x = year, y = val)) +
  geom_line(color = "black", size = 1.2) +   # line
  geom_point(color = "orange", size = 2) +      # points on the line
  labs(title = "DAILYs attributable to air pollution worldwide (1990–2021)",
       x = "Year",
       y = "Total no. of DAILYs") +
  theme_minimal()

## PLOT: "DAILYs attributable to air pollution worldwide (1990–2021) WITH UNCERTAINITY VALUES (SHADED AREA)
ggplot(global_DAILYs, aes(x = year, y = val)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgrey", alpha = 0.3) +
  geom_line(color = "black", size = 1.2) +
  geom_point(color = "orange", size = 2) +
  labs(title = "DAILYs attributable to air pollution worldwide (1990–2021)",
       x = "Year",
       y = "Total no. of DAILYs") +
  theme_minimal()


# DEATHS vs DAILYs
global_summary <- rbind (global_DAILYs, global_deaths)

ggplot(global_summary, aes(x = year, y = val)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.3) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  facet_wrap(~ measure, scales = "free_y") +
  labs(title = "Global Burden of Air Pollution (1990–2021)",
       x = "Year", y = "Total number") +
  theme_minimal()


## DALYs
# 
# There’s a clear global decline in DALYs from 1990 to 2020, followed by a slight increase in 2021.
# This means that, globally, the total years of healthy life lost due to air pollution have decreased over the last three decades.
# 
# Possible explanations:
# - Stricter air-quality regulations and cleaner fuels in many regions.
# - Reduced household air pollution (less use of solid fuels).
# - Improvements in healthcare reducing disability and mortality from pollution-related diseases.
# 
## Deaths
# 
# The number of deaths has remained roughly stable, with a small increase from 1990 to 2010, a dip around 2020, and a rebound in 2021.
# Despite population growth, deaths have not increased dramatically, which could indicate improved exposure control and healthcare, but still a persistent mortality burden.
# 
## Summary
# 
# Air pollution remains a major global health problem, but its overall impact on human health has lessened since 1990.
# DALYs fell significantly, meaning people are living healthier, longer lives despite pollution exposure.
# Deaths have stabilised, showing progress but also highlighting that millions still die every year from air-pollution-related causes.
