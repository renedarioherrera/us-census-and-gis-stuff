# setup
# rene dario herrera 
# 2 May 2022 
# renedario at duck dot com 

# packages 
library(here)
library(tidyverse)
library(tidycensus) #Sys.getenv("CENSUS_API_KEY")
library(janitor)
library(stringr)

# set values for environment
my_states <- c(
  "AZ",
  "NM",
  "VA",
  "NC"
)

# get list of variables 
v19 <- load_variables(2019, "acs5", cache = TRUE)
v19_profile <- load_variables(2019, "acs5/profile", cache = TRUE)
v19_subject <- load_variables(2019, "acs5/subject", cache = TRUE)

# view 
View(v19)

# read data
# percent under poverty, under age 18 
poverty <- get_acs(
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
poverty[c("tract", "county", "state")] <- str_split_fixed(poverty$name, pattern = ", ", n = 3)

# 
poverty %>%
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
poverty %>%
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
poverty %>%
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
poverty %>%
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
poverty %>%
  drop_na(estimate) %>%
  select(!(moe)) %>%
  filter(variable == "womens_earnings_percent" | variable == "sex_percent") %>%
  pivot_wider(
    names_from = variable, values_from = c(estimate)
  ) %>%
  ggplot() +
  geom_point(mapping= aes(x = womens_earnings_percent, y = sex_percent)) +
  facet_wrap(~state)
