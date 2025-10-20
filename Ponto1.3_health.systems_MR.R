###########################################################################################################
# How do deaths and DALYs attributable to air pollution differ across health systems of varying strength? #
###########################################################################################################
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(forcats)

# Read csv files (Import data)
gbd_data <- read.csv("IHME-GBD_2021_DATA-840155c6-1.csv")

# Finding unique locations
unique(gbd_data$location)

# Define the health system categories
health_systems <- c("Advanced Health System",
                    "Basic Health System",
                    "Limited Health System",
                    "Minimal Health System")

# Filter and summarise for Rate:
df_healthsystems <- gbd_data %>%
  filter(
    location %in% health_systems,
    rei == "Air pollution",
    metric == "Rate",       # <- ensure we are using Rate, not Number
    cause == "All causes",
    age == "Age-standardized",
    sex == "Both"
  ) %>%
  select(measure, location, val, upper, lower)

# Make sure your health_systems are ordered in the plot
df_healthsystems <- df_healthsystems %>%
  mutate(location = factor(location, levels = c(
    "Minimal Health System",
    "Limited Health System",
    "Basic Health System",
    "Advanced Health System"
  )))

# Ordered measure
df_healthsystems$measure <- factor(df_healthsystems$measure,
                                     levels = c("Deaths",
                                                "DALYs (Disability-Adjusted Life Years)",
                                                "YLLs (Years of Life Lost)",
                                                "YLDs (Years Lived with Disability)")
)

# Custom palette: worse -> better
pal <- c(
  "Minimal Health System"   = "#b2182b",
  "Limited Health System"   = "#fdae61",
  "Basic Health System"     = "#fee08b",
  "Advanced Health System"  = "#1a9850"
)

# Plot
A <- ggplot(df_healthsystems, aes(x = location, y = val, fill = location)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.18, size = 0.6, color = "black") +
  scale_fill_manual(values = pal) +
  scale_y_continuous() +  # remove comma formatting for rates
  facet_wrap(~ measure, scales = "free_x") +
  coord_flip() +
  labs(
    title = "Impact of Air Pollution by Health System Type (2021)",
    subtitle = "Deaths, DALYs, YLLs & YLDS (per 100,000 population) with 95% uncertainty intervals",
    x = "",
    y = "Rate per 100,000",
    caption = "Data Source: Global Burden of Disease 2021"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 20),
    plot.title = element_text(face = "bold", size = 24)
    
  )

save_plot <- function(plot, filename){
  ggsave(filename, plot = plot, width = 12, height = 6, dpi = 300)
}
save_plot(A, "healthsystems4.png")


# Separate data
df_deaths <- df_healthsystems %>% filter(measure == "Deaths")
df_dalys  <- df_healthsystems %>% filter(measure == "DALYs (Disability-Adjusted Life Years)")

# DEATHS
ggplot(df_deaths, aes(x = location, y = val, fill = location)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.18, size = 0.6, color = "black") +
  geom_text(aes(label = round(val, 0)), 
            hjust = -0.1, vjust = -0.4,size = 4) +
  scale_fill_manual(values = pal) +
  scale_y_continuous() +
  coord_flip() +
  labs(
    title = "Deaths rate attributable to air pollution by Health System type (2021)",
    x = "",
    y = "Death/100k",
    caption = "Data Source: Global Burden of Disease 2021"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size=18)
  )

# DALYs
ggplot(df_dalys, aes(x = location, y = val, fill = location)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.18, size = 0.6, color = "black") +
  geom_text(aes(label = round(val, 0)), 
            hjust = -0.2, vjust = -1.5,size = 4) +
  scale_fill_manual(values = pal) +
  scale_y_continuous() +
  coord_flip() +
  labs(
    title = "DALYs rate attributable to air pollution by Health System type (2021)",
    x = "",
    y = "DALYs/100k",
    caption = "Data Source: Global Burden of Disease 2021"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size=18)
  )

## General trend
# 
# DALYs rates are highest in Minimal and Limited Health Systems and lowest in Advanced Health Systems.
# This suggests a strong association between health system strength and the burden of air pollution. Stronger health systems seem to mitigate the impact of air pollution on population health.
# 
##  Deaths vs. DALYs
# 
# The difference between health system types is more pronounced in DALYs, showing that weaker systems not only see more deaths but also more years of life lost due to disability.
# 
## Uncertainty intervals
# 
# The error bars (upper and lower 95% uncertainty intervals) indicate how confident the estimates are.
# 
# Minimal and Limited Health Systems have wider intervals, reflecting more uncertainty, possibly due to less data availability or reporting quality.
# Advanced systems have narrower intervals, reflecting more reliable data.
# 
## Relative differences
# 
# You can quantify relative risk:
# Minimal vs. Advanced Health System: DALYs rate is roughly 3–4 times higher.
# Death rate is roughly 2–3 times higher.
# This highlights that improving health system strength could substantially reduce the burden of air pollution.
# 
## Key insights
# 
# Stronger health systems (Advanced, Basic) have lower air pollution-related death and disability rates.
# Weaker health systems (Limited, Minimal) bear the highest relative burden.
# DALYs provide a more comprehensive measure of health impact than deaths alone.
