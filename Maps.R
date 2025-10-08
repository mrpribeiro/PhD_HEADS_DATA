# Which countries experienced the highest health impact from air pollution?

#install.packages(c("rnaturalearth", "rnaturalearthdata", "sf", "ggplot2", "dplyr", "countrycode"))
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(dplyr)
library(countrycode)

# Load GBD data
gbd_data <- read.csv("IHME-GBD_2021_DATA-ce582a59-1.csv")

### DEATHS ###
#Filter and select relevant columns 
df_deaths_clean <- gbd_data %>%
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
  filter(!iso3 %in% world$iso3) %>%
  select(location)
unmatched$location

# Checking manually in world dataframe the macthes for these locations
sort(world$name)

# Fix known mismatches manually
df_deaths_clean$location[df_deaths_clean$location == "Micronesia (Federated States of)"] <- "Micronesia"
df_deaths_clean$location[df_deaths_clean$location == "Saint Vincent and the Grenadines"] <- "St. Vin. and Gren."
df_deaths_clean$location[df_deaths_clean$location == "United States Virgin Islands"] <- "U.S. Virgin Is."
df_deaths_clean$location[df_deaths_clean$location == "South Sudan"] <- "S. Sudan"
# Tokelau cannot be matched


# Recalculate ISO3 codes after fixing names
df_deaths_clean$iso3 <- countrycode(df_deaths_clean$location,
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


