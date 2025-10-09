###################################################################################################
# Which countries experienced the highest health (deaths/DALYs) impact from air pollution? (2021) #
###################################################################################################

#install.packages(c("rnaturalearth", "rnaturalearthdata", "sf", "ggplot2", "dplyr", "countrycode", "viridis"))
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(dplyr)
library(countrycode)
library(viridis)


# Load GBD data
gbd_data_2021 <- read.csv("IHME-GBD_2021_DATA-ce582a59-1.csv")
gbd_data_1990 <- read.csv("IHME-GBD_2021_DATA-7baf5a43-1.csv")

### DEATHS ###
#Filter and select relevant columns 
df_deaths_clean_2021 <- gbd_data_2021 %>%
  filter(
    measure == "Deaths",
    rei == "Air pollution",
    metric == "Rate",
    cause == "All causes",
    age == "Age-standardized"
  ) %>%
  select(location, val, upper, lower)

df_deaths_clean_1990 <- gbd_data_1990 %>%
  filter(
    measure == "Deaths",
    rei == "Air pollution",
    metric == "Rate",
    cause == "All causes",
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


# Plot DEATHS 2021
ggplot(data = world_data_2021) +
  geom_sf(aes(fill = val), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(option = "inferno", na.value = "lightgrey", direction = -1,
                       name = "Deaths/100k") +
  labs(title = "Global health impact of air pollution in 2021",
       subtitle = "Death rates per 100,000 population",
       caption = "Data source: Global Burden of Disease") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Plot DEATHS 1990
ggplot(data = world_data_1990) +
  geom_sf(aes(fill = val), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(option = "inferno", na.value = "lightgrey", direction = -1,
                       name = "Deaths/100k") +
  labs(title = "Global health impact of air pollution in 1990",
       subtitle = "Death rates per 100,000 population",
       caption = "Data source: Global Burden of Disease") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

###  ###

### DALYS ###
#Filter and select relevant columns 
df_DALYS_clean_2021 <- gbd_data_2021 %>%
  filter(
    measure == "DALYs (Disability-Adjusted Life Years)",
    rei == "Air pollution",
    metric == "Rate",
    cause == "All causes",
    age == "Age-standardized"
  ) %>%
  select(location, val, upper, lower)

df_DALYS_clean_1990 <- gbd_data_1990 %>%
  filter(
    measure == "DALYs (Disability-Adjusted Life Years)",
    rei == "Air pollution",
    metric == "Rate",
    cause == "All causes",
    age == "Age-standardized"
  ) %>%
  select(location, val, upper, lower)

# Standardize country names using countrycode
# Create ISO3 codes for GBD data
df_DALYS_clean_2021$iso3 <- countrycode(df_DALYS_clean_2021$location,
                                    origin = "country.name",
                                    destination = "iso3c")

df_DALYS_clean_1990$iso3 <- countrycode(df_DALYS_clean_1990$location,
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

# Plot DALYS 2021
ggplot(data = world_data_2021) +
  geom_sf(aes(fill = val), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(option = "rocket", na.value = "lightgrey", direction = -1,
                       name = "DALYs/100k") +
  labs(title = "Global Health Impact of Air Pollution in 2021",
       subtitle = "Disability-adjusted life years per 100,000 population",
       caption = "Data source: Global Burden of Disease") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Plot DALYS 1990
ggplot(data = world_data_1990) +
  geom_sf(aes(fill = val), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(option = "rocket", na.value = "lightgrey", direction = -1,
                       name = "DALYs/100k") +
  labs(title = "Global health impact of air pollution in 1990",
       subtitle = "Disability-adjusted life years per 100,000 population",
       caption = "Data source: Global Burden of Disease") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

###  ###
########################################################################################################
########################################################################################################


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

#Loliplot HIGH
ggplot(top10_high_table, aes(x = reorder(location, val), y = val, color = year)) +
  geom_point(size = 4) +  # The "head" of the lollipop
  geom_segment(aes(x = location, xend = location, y = 0, yend = val), linewidth = 1) +  # The stick
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  scale_color_viridis(discrete = TRUE, option = "viridis") +
  labs(x = "", y = "Deaths/100k",
       title = "Top 10 countries with highest air pollution death rates (1990 vs 2021)") +
  theme_minimal()


#Loliplot LOW
ggplot(top10_low_table, aes(x = reorder(location, val), y = val, color = year)) +
  geom_point(size = 4) +
  geom_segment(aes(x = location, xend = location, y = 0, yend = val), linewidth = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  scale_color_manual(values = c("1990" = "#003f5c", "2021" = "#ffa600")) +  # darker colors
  labs(x = "", y = "Deaths/100k",
       title = "Top 10 countries with lowest air pollution death rates (1990 vs 2021)") +
  theme_minimal()


