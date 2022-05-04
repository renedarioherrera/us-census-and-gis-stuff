# setup ####
# rene dario herrera 
# 2 May 2022 
# renedario at duck dot com 

# packages ####
library(here)
library(tidyverse)
library(tidycensus) #Sys.getenv("CENSUS_API_KEY")
library(janitor)
library(stringr)
library(tigris)

# options
options(tigris_use_cache = TRUE)

# set values for environment
my_states <- c(
  "AZ",
  "NM",
  "VA",
  "NC"
)

# get census tracts
tracts_az <- tracts(
  state = "az"
)

tracts_nm <- tracts(
  state = "nm"
)

tracts_va <- tracts(
  state = "va"
)

tracts_nc <- tracts(
  state = "nc"
)

# get list of census variables 
v19 <- load_variables(2019, "acs5", cache = TRUE)
v19_profile <- load_variables(2019, "acs5/profile", cache = TRUE)
v19_subject <- load_variables(2019, "acs5/subject", cache = TRUE)

# view 
# View(v19)

# read data
# percent under poverty, under age 18 
poverty_tidy <- get_acs(
  geography = "tract",
  variables = c(
    "population" = "S1701_C01_001",
    "poverty" = "S1701_C03_002",
    "median_income" = "S1901_C01_012",
    "median_rent" = "DP04_0134",
    "median_age" = "B01002_001",
    "bachelor_degree_percent" = "DP02_0065P",
    "broadband_percent" = "DP02_0153P",
    "hispanic_percent" = "DP05_0071P",
    "sex_percent" = "S0101_C06_008",
    "womens_earnings_percent" = "S2411_C04_001",
    "gini_index" = "B19083_001",
    "white_not_hispanic" = "B01001H_001"
    ),
  year = 2019,
  output = "tidy",
  geometry = FALSE,
  survey = "acs5",
  state = my_states,
  cache_table = TRUE
) %>%
  clean_names()

# clean geography name
poverty_tidy[c("tract", "county", "state")] <- str_split_fixed(poverty_tidy$name, pattern = ", ", n = 3)

poverty_wide <- get_acs(
  geography = "tract",
  variables = c(
    "population" = "S1701_C01_001",
    "poverty" = "S1701_C03_002",
    "median_income" = "S1901_C01_012",
    "median_rent" = "DP04_0134",
    "median_age" = "B01002_001",
    "bachelor_degree_percent" = "DP02_0065P",
    "broadband_percent" = "DP02_0153P",
    "hispanic_percent" = "DP05_0071P",
    "sex_percent" = "S0101_C06_008",
    "womens_earnings_percent" = "S2411_C04_001",
    "gini_index" = "B19083_001",
    "white_not_hispanic" = "B01001H_001"
  ),
  year = 2019,
  output = "wide",
  geometry = FALSE,
  survey = "acs5",
  state = my_states,
  cache_table = TRUE
) %>%
  clean_names()

# clean geography name
poverty_wide[c("tract", "county", "state")] <- str_split_fixed(poverty_wide$name, pattern = ", ", n = 3)

# 
poverty_tidy %>%
  drop_na(estimate) %>%
  select(!(moe)) %>%
  filter(variable == "sex_percent" | variable == "hispanic_percent") %>%
  pivot_wider(
    names_from = variable, values_from = c(estimate)
  ) %>%
  ggplot() +
  geom_point(mapping= aes(x = hispanic_percent, y = sex_percent)) +
  facet_wrap(~state)

# 
poverty_tidy %>%
  drop_na(estimate) %>%
  select(!(moe)) %>%
  filter(variable == "median_rent" | variable == "median_income") %>%
  pivot_wider(
    names_from = variable, values_from = c(estimate)
  ) %>%
  ggplot() +
  geom_point(mapping= aes(x = median_rent, y = median_income)) +
  facet_wrap(~state)

# 
poverty_tidy %>%
  drop_na(estimate) %>%
  select(!(moe)) %>%
  filter(variable == "gini_index" | variable == "median_income") %>%
  pivot_wider(
    names_from = variable, values_from = c(estimate)
  ) %>%
  ggplot() +
  geom_point(mapping= aes(x = gini_index, y = median_income)) +
  facet_wrap(~state)

#
poverty_tidy %>%
  drop_na(estimate) %>%
  select(!(moe)) %>%
  filter(variable == "population" | variable == "poverty") %>%
  pivot_wider(
    names_from = variable, values_from = c(estimate)
  ) %>%
  ggplot() +
  geom_point(mapping= aes(x = population, y = poverty)) +
  facet_wrap(~state)

#
poverty_tidy %>%
  drop_na(estimate) %>%
  select(!(moe)) %>%
  filter(variable == "womens_earnings_percent" | variable == "sex_percent") %>%
  pivot_wider(
    names_from = variable, values_from = c(estimate)
  ) %>%
  ggplot() +
  geom_point(mapping= aes(x = womens_earnings_percent, y = sex_percent)) +
  facet_wrap(~state)

# population density ####
# spatial join
poverty_tracts_az <- geo_join(
  spatial_data = tracts_az,
  data_frame = poverty_wide,
  by_sp = "GEOID",
  by_df = "geoid",
  how = "left"
)

poverty_tracts_az <- poverty_tracts_az %>%
  mutate(pop_dens = population_e / (ALAND/1000))

# arizona
ggplot() +
  geom_sf(
    data = poverty_tracts_az,
    mapping = aes(fill = pop_dens),
    color = NA
  ) +
  scale_fill_viridis_c() 

# spatial join
poverty_tracts_nm <- geo_join(
  spatial_data = tracts_nm,
  data_frame = poverty_wide,
  by_sp = "GEOID",
  by_df = "geoid",
  how = "left"
)

poverty_tracts_nm <- poverty_tracts_nm %>%
  mutate(pop_dens = population_e / (ALAND/1000))

# new mexico
ggplot() +
  geom_sf(
    data = poverty_tracts_nm,
    mapping = aes(fill = pop_dens),
    color = NA
  ) +
  scale_fill_viridis_c() 

# spatial join
poverty_tracts_va <- geo_join(
  spatial_data = tracts_va,
  data_frame = poverty_wide,
  by_sp = "GEOID",
  by_df = "geoid",
  how = "left"
)

poverty_tracts_va <- poverty_tracts_va %>%
  mutate(pop_dens = population_e / (ALAND/1000))

# virginia
ggplot() +
  geom_sf(
    data = poverty_tracts_va,
    mapping = aes(fill = pop_dens),
    color = NA
  ) +
  scale_fill_viridis_c() 

# spatial join
poverty_tracts_nc <- geo_join(
  spatial_data = tracts_nc,
  data_frame = poverty_wide,
  by_sp = "GEOID",
  by_df = "geoid",
  how = "left"
)

poverty_tracts_nc <- poverty_tracts_nc %>%
  mutate(pop_dens = population_e / (ALAND/1000))

# north carolina
ggplot() +
  geom_sf(
    data = poverty_tracts_nc,
    mapping = aes(fill = pop_dens),
    color = NA
  ) +
  scale_fill_viridis_c() 

# gini index ####
# arizona
ggplot() +
  geom_sf(
    data = poverty_tracts_az,
    mapping = aes(fill = gini_index_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# new mexico
ggplot() +
  geom_sf(
    data = poverty_tracts_nm,
    mapping = aes(fill = gini_index_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# virginia
ggplot() +
  geom_sf(
    data = poverty_tracts_va,
    mapping = aes(fill = gini_index_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# north carolina
ggplot() +
  geom_sf(
    data = poverty_tracts_nc,
    mapping = aes(fill = gini_index_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# median age ####
# arizona
ggplot() +
  geom_sf(
    data = poverty_tracts_az,
    mapping = aes(fill = median_age_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# new mexico
ggplot() +
  geom_sf(
    data = poverty_tracts_nm,
    mapping = aes(fill = median_age_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# virginia
ggplot() +
  geom_sf(
    data = poverty_tracts_va,
    mapping = aes(fill = median_age_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# north carolina
ggplot() +
  geom_sf(
    data = poverty_tracts_nc,
    mapping = aes(fill = median_age_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# poverty ####
# arizona
ggplot() +
  geom_sf(
    data = poverty_tracts_az,
    mapping = aes(fill = poverty_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# new mexico
ggplot() +
  geom_sf(
    data = poverty_tracts_nm,
    mapping = aes(fill = poverty_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# virginia
ggplot() +
  geom_sf(
    data = poverty_tracts_va,
    mapping = aes(fill = poverty_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# north carolina
ggplot() +
  geom_sf(
    data = poverty_tracts_nc,
    mapping = aes(fill = poverty_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# median income ####
# arizona
ggplot() +
  geom_sf(
    data = poverty_tracts_az,
    mapping = aes(fill = median_income_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# new mexico
ggplot() +
  geom_sf(
    data = poverty_tracts_nm,
    mapping = aes(fill = median_income_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# virginia
ggplot() +
  geom_sf(
    data = poverty_tracts_va,
    mapping = aes(fill = median_income_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# north carolina
ggplot() +
  geom_sf(
    data = poverty_tracts_nc,
    mapping = aes(fill = median_income_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# percent sex ####
# arizona
ggplot() +
  geom_sf(
    data = poverty_tracts_az,
    mapping = aes(fill = sex_percent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# new mexico
ggplot() +
  geom_sf(
    data = poverty_tracts_nm,
    mapping = aes(fill = sex_percent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# virginia
ggplot() +
  geom_sf(
    data = poverty_tracts_va,
    mapping = aes(fill = sex_percent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# north carolina
ggplot() +
  geom_sf(
    data = poverty_tracts_nc,
    mapping = aes(fill = sex_percent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# women's earnings ####
# arizona
ggplot() +
  geom_sf(
    data = poverty_tracts_az,
    mapping = aes(fill = womens_earnings_percent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# new mexico
ggplot() +
  geom_sf(
    data = poverty_tracts_nm,
    mapping = aes(fill = womens_earnings_percent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# virginia
ggplot() +
  geom_sf(
    data = poverty_tracts_va,
    mapping = aes(fill = womens_earnings_percent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# north carolina
ggplot() +
  geom_sf(
    data = poverty_tracts_nc,
    mapping = aes(fill = womens_earnings_percent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# median rent ####
# arizona
ggplot() +
  geom_sf(
    data = poverty_tracts_az,
    mapping = aes(fill = median_rent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# new mexico
ggplot() +
  geom_sf(
    data = poverty_tracts_nm,
    mapping = aes(fill = median_rent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# virginia
ggplot() +
  geom_sf(
    data = poverty_tracts_va,
    mapping = aes(fill = median_rent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# north carolina
ggplot() +
  geom_sf(
    data = poverty_tracts_nc,
    mapping = aes(fill = median_rent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# hispanic ####
# arizona
ggplot() +
  geom_sf(
    data = poverty_tracts_az,
    mapping = aes(fill = hispanic_percent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# new mexico
ggplot() +
  geom_sf(
    data = poverty_tracts_nm,
    mapping = aes(fill = hispanic_percent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# virginia
ggplot() +
  geom_sf(
    data = poverty_tracts_va,
    mapping = aes(fill = hispanic_percent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 

# north carolina
ggplot() +
  geom_sf(
    data = poverty_tracts_nc,
    mapping = aes(fill = hispanic_percent_e),
    color = NA
  ) +
  scale_fill_viridis_c() 
