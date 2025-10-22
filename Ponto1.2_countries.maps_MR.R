#####################################################################################
# Which regions of the world have experienced the greatest global burden of chronic #
# respiratory diseases attributable to air pollution between 1990 and 2021?         #
#####################################################################################

# Load packages
# Core tidyverse tools
library(dplyr)        # for filter(), select(), mutate(), arrange(), bind_rows(), etc.
library(ggplot2)      # for all plotting (geom_sf, geom_point, geom_segment, etc.)

# Spatial and geographic data handling
library(rnaturalearth)  # for ne_countries()
library(rnaturalearthdata)
library(sf)             # for working with spatial features (used by rnaturalearth + ggplot2)

# Data merging and standardizing country names
library(countrycode)    # for converting country names to ISO3 codes

# Color scales for maps
library(viridis)        # for scale_fill_viridis_c()

# Combining plots neatly
library(patchwork)      # for combining ggplots side by side with +, /, plot_layout(), plot_annotation()

# Load GBD data
gbd_data_2021 <- read.csv("IHME-GBD_2021_DATA-ce582a59-1.csv")
gbd_data_1990 <- read.csv("IHME-GBD_2021_DATA-7baf5a43-1.csv")

##############
### DEATHS ###
##############

#Filter and select relevant columns 
df_deaths_clean_2021 <- gbd_data_2021 %>%
  filter(
    measure == "Deaths",
    rei == "Air pollution",
    metric == "Rate",
    cause == "Chronic respiratory diseases",
    age == "Age-standardized"
  ) %>%
  select(location, val, upper, lower)

df_deaths_clean_1990 <- gbd_data_1990 %>%
  filter(
    measure == "Deaths",
    rei == "Air pollution",
    metric == "Rate",
    cause == "Chronic respiratory diseases",
    age == "Age-standardized"
  ) %>%
  select(location, val, upper, lower)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")


# Standardize country names using countrycode
#   First, create ISO3 codes for GBD data
df_deaths_clean_2021$iso3 <- countrycode(df_deaths_clean_2021$location,
                                    origin = "country.name",
                                    destination = "iso3c")

df_deaths_clean_1990$iso3 <- countrycode(df_deaths_clean_1990$location,
                                         origin = "country.name",
                                         destination = "iso3c")

#   Create ISO3 codes for world map
world$iso3 <- countrycode(world$name,
                          origin = "country.name",
                          destination = "iso3c")


# Check unmatched countries
unmatched <- df_deaths_clean_2021 %>%
  filter(!iso3 %in% world$iso3) %>% # Keep all countries whose ISO3 code is not in the world dataset.
  select(location, iso3)
unmatched$location

# The non correlation with the iso3 code is from the world dataset (bc df does not have Null values associated)

# Fix known mismatches manually
world$name[world$name == "Micronesia"] <- "Micronesia (Federated States of)"
world$name[world$name == "St. Vin. and Gren."] <- "Saint Vincent and the Grenadines"
world$name[world$name == "U.S. Virgin Is."] <- "United States Virgin Islands "
world$name[world$name == "S. Sudan"] <- "South Sudan "
# Tokelau cannot be matched


# Recalculate ISO3 codes after fixing names
world$iso3 <- countrycode(world$name,
                          origin = "country.name",
                          destination = "iso3c")

# Check unmatched countries AGAIN
unmatched <- df_deaths_clean_2021 %>%
  filter(!iso3 %in% world$iso3) %>%
  select(location)
unmatched$location

# Merge GBD data with world map
world_data_2021 <- world %>%
  left_join(df_deaths_clean_2021, by = "iso3")

world_data_1990 <- world %>%
  left_join(df_deaths_clean_1990, by = "iso3")

# Determine the common range across both datasets
fill_limits <- range(c(world_data_1990$val, world_data_2021$val), na.rm = TRUE)


### PLOTS DEATHS

# Common theme to make things consistent
base_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# --- Plot for 1990 (no legend, simple title) ---
p1990 <- ggplot(data = world_data_1990) +
  geom_sf(aes(fill = val), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(
    option = "inferno", na.value = "lightgrey",
    direction = -1, name = "Deaths/100k", limits = fill_limits
  ) +
  labs(title = "1990") +
  base_theme +
  theme(legend.position = "none")  # remove legend

# --- Plot for 2021 (keep legend) ---
p2021 <- ggplot(data = world_data_2021) +
  geom_sf(aes(fill = val), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(
    option = "inferno", na.value = "lightgrey",
    direction = -1, name = "Deaths/100k", limits = fill_limits
  ) +
  labs(title = "2021") +
  base_theme

# --- Combine side by side ---
combined <- p1990 + p2021 +
  plot_annotation(
    title = "Global Deaths from Chronic Respiratory Diseases Attributable to Air Pollution (1990–2021)",
    subtitle = "Death Rates per 100,000 Population",
    caption = "Data Source: Global Burden of Disease 2021"
  ) &
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  )

# Show maps
combined

#############
### DALYS ###
#############

#Filter and select relevant columns 
df_DALYS_clean_2021 <- gbd_data_2021 %>%
  filter(
    measure == "DALYs (Disability-Adjusted Life Years)",
    rei == "Air pollution",
    metric == "Rate",
    cause == "Chronic respiratory diseases",
    age == "Age-standardized"
  ) %>%
  select(location, val, upper, lower)

df_DALYS_clean_1990 <- gbd_data_1990 %>%
  filter(
    measure == "DALYs (Disability-Adjusted Life Years)",
    rei == "Air pollution",
    metric == "Rate",
    cause == "Chronic respiratory diseases",
    age == "Age-standardized"
  ) %>%
  select(location, val, upper, lower)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Standardize country names using countrycode
# Create ISO3 codes for GBD data
df_DALYS_clean_2021$iso3 <- countrycode(df_DALYS_clean_2021$location,
                                    origin = "country.name",
                                    destination = "iso3c")

df_DALYS_clean_1990$iso3 <- countrycode(df_DALYS_clean_1990$location,
                                        origin = "country.name",
                                        destination = "iso3c")

#   Create ISO3 codes for world map
world$iso3 <- countrycode(world$name,
                          origin = "country.name",
                          destination = "iso3c")

# Check unmatched countries
unmatched_DALYS <- df_DALYS_clean_2021 %>%
  filter(!iso3 %in% world$iso3) %>% # Keep all countries whose ISO3 code is not in the world dataset.
  select(location, iso3)
unmatched_DALYS$location

# Merge GBD data with world map
world_data_DALYs_2021 <- world %>%
  left_join(df_DALYS_clean_2021, by = "iso3")

world_data_DALYs_1990 <- world %>%
  left_join(df_DALYS_clean_1990, by = "iso3")

# Determine the common range across both datasets
fill_limits_dalys <- range(c(world_data_DALYs_1990$val, world_data_DALYs_2021$val), na.rm = TRUE)

### PLOTS DALYs

# Common theme to make things consistent
base_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# --- Plot for 1990 (no legend, simple title) ---
p1990_dalys <- ggplot(data = world_data_DALYs_1990) +
  geom_sf(aes(fill = val), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(
    option = "rocket", na.value = "lightgrey",
    direction = -1, name = "DALYs/100k", limits = fill_limits_dalys
  ) +
  labs(title = "1990") +
  base_theme +
  theme(legend.position = "none")  # remove legend

# --- Plot for 2021 (keep legend) ---
p2021_dalys <- ggplot(data = world_data_DALYs_2021) +
  geom_sf(aes(fill = val), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(
    option = "rocket", na.value = "lightgrey",
    direction = -1, name = "DALYs/100k", limits = fill_limits_dalys
  ) +
  labs(title = "2021") +
  base_theme

# --- Combine side by side ---
combined_dalys <- p1990_dalys + p2021_dalys +
  plot_annotation(
    title = "Global DALYs from Chronic Respiratory Diseases Attributable to Air Pollution (1990–2021)",
    subtitle = "DALYs (Disability-Adjusted Life Years) Rates per 100,000 Population",
    caption = "Data Source: Global Burden of Disease 2021"
  ) &
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  )

# Show maps
combined_dalys

###  PLOTs top 10 countries ###

##########
# DEATHS #
##########

# Top 10 highest
top10_high_2021 <- df_deaths_clean_2021 %>%
  arrange(desc(val)) %>%
  slice_head(n = 10)

top10_high_1990 <- df_deaths_clean_1990 %>%
  arrange(desc(val)) %>%
  slice_head(n = 10)


# Top 10 lowest
top10_low_2021 <- df_deaths_clean_2021 %>%
  arrange(val) %>%
  slice_head(n = 10)

top10_low_1990 <- df_deaths_clean_1990 %>%
  arrange(val) %>%
  slice_head(n = 10)

# Combine both into one table
top10_high_table <- bind_rows(
  top10_high_2021 %>% mutate(year = "2021"),
  top10_high_1990 %>% mutate(year = "1990")
) %>%
  select(year, location, val, upper, lower)

top10_low_table <- bind_rows(
  top10_low_2021 %>% mutate(year = "2021"),
  top10_low_1990 %>% mutate(year = "1990")
) %>%
  select(year, location, val, upper, lower)

# Make sure year is a factor so colors are consistent
top10_high_table$year <- factor(top10_high_table$year, levels = c("1990", "2021"))
top10_low_table$year <- factor(top10_low_table$year, levels = c("1990", "2021"))


## LOLIPOP PLOTS
# Define color mapping
year_colors <- c("1990" = "#003f5c", "2021" = "#ffa600")

# --- Base theme ---
base_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 11),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

# --- Lollipop HIGH ---
p_high <- ggplot(top10_high_table, aes(x = reorder(location, val), y = val, color = year)) +
  geom_segment(aes(xend = location, y = 0, yend = val), linewidth = 1) +
  geom_point(size = 4) +
  coord_flip() +
  scale_color_manual(values = year_colors, name = "Year") +
  labs(title = "Top 10 Countries (Highest Death Rates)", y = "Deaths/100k") +
  base_theme +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# --- Lollipop LOW ---
p_low <- ggplot(top10_low_table, aes(x = reorder(location, -val), y = val, color = year)) +
  geom_segment(aes(xend = location, y = 0, yend = val), linewidth = 1) +
  geom_point(size = 4) +
  coord_flip() +
  scale_color_manual(values = year_colors, name = "Year") +
  labs(title = "Bottom 10 Countries (Lowest Death Rates)", y = "Deaths/100k") +
  base_theme +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# --- Combine side by side ---
combined <- p_high + p_low +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Global Extremes in Deaths from Air Pollution–Related Chronic Respiratory Diseases (1990–2021)",
    caption = "Data source: Global Burden of Disease 2021"
  ) &
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(size = 10, hjust = 1, face = "italic")
  )

combined

