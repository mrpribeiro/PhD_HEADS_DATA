# Which countries experienced the highest health impact from air pollution? (2021)

#install.packages(c("rnaturalearth", "rnaturalearthdata", "sf", "ggplot2", "dplyr", "countrycode"))
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(dplyr)
library(countrycode)

# Load GBD data
gbd_data_2021 <- read.csv("IHME-GBD_2021_DATA-ce582a59-1.csv")

### DEATHS ###
#Filter and select relevant columns 
df_deaths_clean <- gbd_data_2021 %>%
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
df_deaths_clean$iso3 <- countrycode(df_deaths_clean$location,
                                    origin = "country.name",
                                    destination = "iso3c")

#   Create ISO3 codes for world map
world$iso3 <- countrycode(world$name,
                          origin = "country.name",
                          destination = "iso3c")


# Check unmatched countries
unmatched <- df_deaths_clean %>%
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
unmatched <- df_deaths_clean %>%
  filter(!iso3 %in% world$iso3) %>%
  select(location)
unmatched$location

# Merge GBD data with world map
world_data <- world %>%
  left_join(df_deaths_clean, by = "iso3")


# Plot
ggplot(data = world_data) +
  geom_sf(aes(fill = val), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(option = "inferno", na.value = "lightgrey", direction = -1,
                       name = "Deaths per 100,000") +
  labs(title = "Global Health Impact of Air Pollution",
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
df_DALYS_clean <- gbd_data_2021 %>%
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
df_DALYS_clean$iso3 <- countrycode(df_DALYS_clean$location,
                                    origin = "country.name",
                                    destination = "iso3c")

# Check unmatched countries
unmatched_DALYS <- df_DALYS_clean %>%
  filter(!iso3 %in% world$iso3) %>% # Keep all countries whose ISO3 code is not in the world dataset.
  select(location, iso3)
unmatched_DALYS$location

# Merge GBD data with world map
world_data_DALYs <- world %>%
  left_join(df_DALYS_clean, by = "iso3")

# Plot
ggplot(data = world_data) +
  geom_sf(aes(fill = val), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(option = "rocket", na.value = "lightgrey", direction = -1,
                       name = "DALYs per 100,000") +
  labs(title = "Global Health Impact of Air Pollution",
       subtitle = "DALYs rates per 100,000 population",
       caption = "Data source: Global Burden of Disease") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

###  ###


# Which countries experienced the highest health impact from air pollution? (2021)

#install.packages(c("rnaturalearth", "rnaturalearthdata", "sf", "ggplot2", "dplyr", "countrycode"))
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(dplyr)
library(countrycode)

# Load GBD data
gbd_data_2021 <- read.csv("IHME-GBD_2021_DATA-ce582a59-1.csv")

### DEATHS ###
#Filter and select relevant columns 
df_deaths_clean <- gbd_data_2021 %>%
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
df_deaths_clean$iso3 <- countrycode(df_deaths_clean$location,
                                    origin = "country.name",
                                    destination = "iso3c")

#   Create ISO3 codes for world map
world$iso3 <- countrycode(world$name,
                          origin = "country.name",
                          destination = "iso3c")


# Check unmatched countries
unmatched <- df_deaths_clean %>%
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
unmatched <- df_deaths_clean %>%
  filter(!iso3 %in% world$iso3) %>%
  select(location)
unmatched$location

# Merge GBD data with world map
world_data <- world %>%
  left_join(df_deaths_clean, by = "iso3")


# Plot
ggplot(data = world_data) +
  geom_sf(aes(fill = val), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(option = "inferno", na.value = "lightgrey", direction = -1,
                       name = "Deaths per 100,000") +
  labs(title = "Global Health Impact of Air Pollution",
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
df_DALYS_clean <- gbd_data_2021 %>%
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
df_DALYS_clean$iso3 <- countrycode(df_DALYS_clean$location,
                                    origin = "country.name",
                                    destination = "iso3c")

# Check unmatched countries
unmatched_DALYS <- df_DALYS_clean %>%
  filter(!iso3 %in% world$iso3) %>% # Keep all countries whose ISO3 code is not in the world dataset.
  select(location, iso3)
unmatched_DALYS$location

# Merge GBD data with world map
world_data_DALYs <- world %>%
  left_join(df_DALYS_clean, by = "iso3")

# Plot
ggplot(data = world_data) +
  geom_sf(aes(fill = val), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(option = "rocket", na.value = "lightgrey", direction = -1,
                       name = "DALYs per 100,000") +
  labs(title = "Global Health Impact of Air Pollution",
       subtitle = "DALYs rates per 100,000 population",
       caption = "Data source: Global Burden of Disease") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

###  ###