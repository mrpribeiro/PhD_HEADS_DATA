############################################################################################################################
# How does the burden of air pollution differs on chronical respiratory diseases across health systems of varying strength #
############################################################################################################################
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

# Read csv files (Import data)
gbd_data <- read.csv("IHME-GBD_2021_DATA-840155c6-1.csv")

# Finding unique locations
unique(gbd_data$location)

# --- Prepare data ---
df_healthsystems <- gbd_data %>%
  filter(
    location %in% c("Advanced Health System",
                    "Basic Health System",
                    "Limited Health System",
                    "Minimal Health System"),
    rei == "Air pollution",
    metric == "Rate",
    cause == "Chronic respiratory diseases",
    age == "Age-standardized",
    sex == "Both"
  ) %>%
  select(measure, location, val, upper, lower) %>%
  mutate(location = factor(location, levels = c(
    "Minimal Health System",
    "Limited Health System",
    "Basic Health System",
    "Advanced Health System"
  )))

# Color palette (worse → better)
pal <- c(
  "Minimal Health System"   = "#b2182b",
  "Limited Health System"   = "#fdae61",
  "Basic Health System"     = "#fee08b",
  "Advanced Health System"  = "#1a9850"
)

# --- Flexible plotting function ---
make_plot <- function(data, measure_label, x_label) {
  data %>%
    filter(measure == measure_label) %>%
    ggplot(aes(y = location, x = val, fill = location)) +
    geom_col(color = "black", width = 0.7) +
    geom_errorbar(aes(xmin = lower, xmax = upper),
                  width = 0.3, size = 0.6, color = "black") +
    scale_fill_manual(values = pal) +
    labs(
      title = measure_label,
      y = NULL,
      x = x_label
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "none",
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 11)
    )
}

# --- Individual plots
p_deaths <- make_plot(df_healthsystems,
                      "Deaths",
                      "Deaths/100k")

p_dalys  <- make_plot(df_healthsystems,
                      "DALYs (Disability-Adjusted Life Years)",
                      "DALYs/100k")

p_ylls   <- make_plot(df_healthsystems,
                      "YLLs (Years of Life Lost)",
                      "YLLs/100k")

p_ylds   <- make_plot(df_healthsystems,
                      "YLDs (Years Lived with Disability)",
                      "YLDs/100k")

# --- Combine into a 2×2 grid ---
combined_health <- (p_deaths | p_dalys) /
  (p_ylls | p_ylds) +
  plot_annotation(
    title = "Impact of Air Pollution on Chronic Respiratory Diseases by Health System Type (2021)",
    caption = "Data Source: Global Burden of Disease 2021",
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 12, face = "italic", hjust = 1)
    )
  )

# Show final figure
combined_health


