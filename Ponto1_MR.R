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
# install.packages("rnaturalearth")
# install.packages("sf")
# install.packages("viridis")
# install.packages("countrycode")

# Load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

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
df_all$location <- as.factor(df_all$location)

summary(df_all)

# Frequency tables
View(table(df_all$measure))
View(table(df_all$location))
View(table(df_all$sex))
View(table(df_all$age))
View(table(df_all$cause))
View(table(df_all$rei))

####################################################################################################################################
  # Total deaths worldwide by year:
  # global_deaths <- subset(df_all, location == "Global" & measure == "Deaths" & metric == "Number")

# Total deaths worldwide by risk factor as " Air pollution" by year:
global_deaths <- subset(df_all, location == "Global" & measure == "Deaths" & rei == "Air pollution" & metric == "Number")
# aggregate(val ~ year, global_deaths, sum) # summarize val (estimate, main value), sum by year

# Another way to do it, more practical (put it in a variable)
global_deaths_summary <- global_deaths %>%
  group_by(year) %>%                # group the data by year
  summarise(total_val = sum(val),  # sum the values for each year
            total_upper = sum(upper),
            total_lower = sum(lower)
            )

### This already answers half the 1. question: In 2021, 46772483 died due to air pollution, worldwide.

# PLOT: "Global deaths attributable to air pollution (1990–2021)
ggplot(global_deaths_summary, aes(x = year, y = total_val)) +
  geom_line(color = "steelblue", size = 1.2) +   # line
  geom_point(color = "darkred", size = 2) +      # points on the line
  labs(title = "Global deaths attributable to air pollution (1990–2021)",
       x = "Year",
       y = "Total deaths") +
  theme_minimal()

## PLOT WITH UNCERTAINITY VALUES (SHADED AREA)
ggplot(global_deaths_summary, aes(x = year, y = total_val)) +
  geom_ribbon(aes(ymin = total_lower, ymax = total_upper), fill = "lightblue", alpha = 0.3) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Global deaths attributable to air pollution (1990–2021)",
       x = "Year",
       y = "Total deaths") +
  theme_minimal()

# y with plain numbers instead of scientific ones
# ggplot(global_deaths_summary, aes(x = year, y = total_val)) +
#   geom_line(color = "darkred", size = 1.2) +
#   geom_point(color = "yellow", size = 2) +
#   labs(title = "Global deaths attributable to air pollution (1990–2021)",
#        x = "Year",
#        y = "Total deaths") +
#   scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
#   theme_minimal()


# Filter global deaths for all risk factors (REIs)
global_deaths_all <- subset(df_all, location == "Global" & measure == "Deaths" & metric == "Number")

# Summarize deaths by year and REI
global_deaths_summary_all <- global_deaths_all %>%
  group_by(year, rei) %>%
  summarise(total_val = sum(val), .groups = "drop")

# Plot: deaths by risk factor over time
ggplot(global_deaths_summary_all, aes(x = year, y = total_val, color = rei)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma) +  # show plain numbers with commas
  labs(title = "Global deaths worldwide by risk factor (1990–2021)",
       x = "Year",
       y = "Total deaths",
       color = "Risk factor") +
  theme_minimal()


# Function to plot global data for selected REIs or all if NULL
plot_global_by_rei <- function(df, selected_reis = NULL, measure_type = "Deaths", plot_title = NULL) {
  
  # Filter for Global, selected measure, metric Number
  data_filtered <- df %>%
    filter(location == "Global",
           measure == measure_type,
           metric == "Number")
  
  # If specific REIs provided, filter them
  if (!is.null(selected_reis)) {
    data_filtered <- data_filtered %>%
      filter(rei %in% selected_reis)
  }
  
  # Summarize by year and REI
  data_summary <- data_filtered %>%
    group_by(year, rei) %>%
    summarise(total_val = sum(val), .groups = "drop")
  
  # Default title if none provided
  if (is.null(plot_title)) {
    plot_title <- ifelse(is.null(selected_reis),
                         paste("Global", measure_type, "by all risk factors (1990–2021)"),
                         paste("Global", measure_type, "by selected risk factors (1990–2021)"))
  }
  
  # Create plot
  p <- ggplot(data_summary, aes(x = year, y = total_val, color = rei)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(title = plot_title,
         x = "Year",
         y = measure_type,
         color = "Risk factor") +
    theme_minimal()
  
  return(p)
}

# Usage for all risk factors deaths
plot_global_by_rei(df_all, selected_reis = NULL, measure_type = "Deaths")

# Usage for all risk factors DALYs
plot_global_by_rei(df_all, selected_reis = NULL, measure_type = "DALYs (Disability-Adjusted Life Years)")

# DALYS by specific risk factors:
plot_global_by_rei(df_all,
                   selected_reis = c("Household air pollution from solid fuels", "Ambient particulate matter pollution"),
                   plot_title = "Comparison of DALYs from Houselhold and Ambient pollution risk factors (1990–2021)",
                   measure_type = "DALYs (Disability-Adjusted Life Years)")



# Summarise global data (Deaths & DALYs only)
global_summary <- df_all %>%
  filter(location == "Global",
         year >= 1990,
         measure %in% c("Deaths", "DALYs (Disability-Adjusted Life Years)"),
         metric == "Number",
         grepl("air pollution", rei, ignore.case = TRUE)) %>%
  group_by(year, measure) %>%
  summarise(total_val = sum(val), .groups = "drop")

ggplot(global_summary, aes(x = year, y = total_val)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  facet_wrap(~ measure, scales = "free_y") +
  labs(title = "Global Burden of Air Pollution (1990–2021)",
       x = "Year", y = "Total number") +
  theme_minimal()

##################################################################################################################################

# Define the health system categories
health_systems <- c("Advanced Health System",
                    "Basic Health System",
                    "Limited Health System",
                    "Minimal Health System")

# Filter and summarise
df_healthsystems <- df_all %>%
  filter(year == 2021,
         rei == "Air pollution",
         measure %in% c("Deaths", "DALYs (Disability-Adjusted Life Years)"),
         location %in% health_systems,
         metric == "Number") %>%
  group_by(measure, location) %>%
  summarise(total_val = sum(val, na.rm = TRUE)) %>%
  arrange(measure, desc(total_val))

# Make sure your health_systems are ordered in the plot
df_healthsystems <- df_healthsystems %>%
  mutate(location = factor(location, levels = c("Minimal Health System",
                                                "Limited Health System",
                                                "Basic Health System",
                                                "Advanced Health System")))
# Create a faceted bar plot
ggplot(df_healthsystems, aes(x = location, y = total_val, fill = measure)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Impact of Air Pollution by Health System Type in 2021",
    x = "Health System",
    y = "Total Count",
    fill = "Measure"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# Faceted bar plot
ggplot(df_healthsystems, aes(x = location, y = total_val, fill = location)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ measure, scales = "free_y") +
  labs(
    title = "Impact of Air Pollution by Health System Type in 2021",
    x = "Health System",
    y = "Total Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Separate data
df_deaths <- df_healthsystems %>% filter(measure == "Deaths")
df_dalys  <- df_healthsystems %>% filter(measure == "DALYs (Disability-Adjusted Life Years)")

# Vertical bar plot for Deaths
ggplot(df_deaths, aes(x = location, y = total_val, fill = location)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(total_val)), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Deaths due to Air Pollution by Health System Type (2021)",
    x = "Health System",
    y = "Total Deaths"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Vertical bar plot for DALYs
ggplot(df_dalys, aes(x = location, y = total_val, fill = location)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(total_val)), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "DALYs due to Air Pollution by Health System Type (2021)",
    x = "Health System",
    y = "Total DALYs"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##########################################################################################
unique(df_all$location)
