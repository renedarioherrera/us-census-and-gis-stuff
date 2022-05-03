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
    "median_rent" = "DP04_0134"),
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
  filter(estimate != 0) %>%
  group_by(state) %>%
  slice_min(n = 5, order_by = estimate)
