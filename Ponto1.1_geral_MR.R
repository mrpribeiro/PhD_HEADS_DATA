######################################################################
# What are the global burden temporal trends attributable to air     #
# pollution on chronical respiratory diseases between 1990 and 2021? #
######################################################################
# Load packages
library(dplyr)
library(ggplot2)
library(patchwork)

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
    metric == "Rate",
    cause == "Chronic respiratory diseases",
    age == "All ages",
    sex == "Both"
  ) %>%
select(measure, year, val, upper, lower)

# Total YLLs (Years of Life Lost) worldwide by year:
global_YLLs <- gbd_data %>%
  filter(
    measure == "YLLs (Years of Life Lost)",
    location == "Global",
    rei == "Air pollution",
    metric == "Rate",
    cause == "Chronic respiratory diseases",
    age == "All ages",
    sex == "Both"
  ) %>%
  select(measure, year, val, upper, lower)

# Total YLDs (Years Lived with Disability) worldwide by year:
global_YLDs <- gbd_data %>%
  filter(
    measure == "YLDs (Years Lived with Disability)",
    location == "Global",
    rei == "Air pollution",
    metric == "Rate",
    cause == "Chronic respiratory diseases",
    age == "All ages",
    sex == "Both"
  ) %>%
  select(measure, year, val, upper, lower)

# Total DALYs worldwide by year:
global_DALYs <- gbd_data %>%
  filter(
    measure == "DALYs (Disability-Adjusted Life Years)",
    location == "Global",
    rei == "Air pollution",
    metric == "Rate",
    cause == "Chronic respiratory diseases",
    age == "All ages",
    sex == "Both"
  ) %>%
  select(measure, year, val, upper, lower)

# ----- Individual plots -----

# Deaths
p_deaths <- ggplot(global_deaths, aes(x = year, y = val)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#f4a582", alpha = 0.3) +
  geom_line(color = "#ca0020", size = 1.2) +
  geom_point(color = "#67001f", size = 2) +
  labs(title = "Deaths", x = "Year", y = "Deaths / 100k") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

# DALYs
p_dalys <- ggplot(global_DALYs, aes(x = year, y = val)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#fee08b", alpha = 0.3) +
  geom_line(color = "#fdae61", size = 1.2) +
  geom_point(color = "#e08214", size = 2) +
  labs(title = "DALYs (Disability-Adjusted Life Years)", x = "Year", y = "DALYs / 100k") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

# YLLs
p_ylls <- ggplot(global_YLLs, aes(x = year, y = val)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#91bfdb", alpha = 0.3) +
  geom_line(color = "#4575b4", size = 1.2) +
  geom_point(color = "#313695", size = 2) +
  labs(title = "YLLs (Years of Life Lost)", x = "Year", y = "YLLs / 100k") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

# YLDs
p_ylds <- ggplot(global_YLDs, aes(x = year, y = val)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#c7e9c0", alpha = 0.4) +
  geom_line(color = "#238b45", size = 1.2) +
  geom_point(color = "#00441b", size = 2) +
  labs(title = "YLDs (Years Lived with Disability)", x = "Year", y = "YLDs / 100k") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))


# ----- Combine and add main title + caption -----

combined_plot <- (p_deaths | p_dalys) /
  (p_ylls   | p_ylds) +
  plot_annotation(
    title = "Global Burden of Air Pollution on Chronic Respiratory Diseases (1990–2021)",
    caption = "Data source: Global Burden of Disease 2021",
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 12, face = "italic")
    )
  )

# Show the combined plot
combined_plot
