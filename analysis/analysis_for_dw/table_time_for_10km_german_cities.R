#######
# Script for creating the table on how long it takes to travel 10 km on average
#######

library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)
devtools::load_all()


# params ------------------------------------------------------------------
area_selected = "urban"
value_column = "yearTravelTimePerKm"
output_path = here(makePath("output/data/dw/year_avg_travel_time_10km.csv"))

# data on german cities --------------------------------------------------
data = tomtomR::data_tomtom


# get the data for each year ----------------------------------------------
data_wide = data %>%
  filter(area == {{area_selected}}) %>%
  select(year, area, city, country, all_of(value_column)) %>%
  pivot_wider(names_from = "year",
              values_from = all_of(value_column))


# reorder for datawrapper -------------------------------------------------
# and rename as they will not be seen anyways (just for the little bar chart in the table)

data_reordered = data_wide %>%
  select(city, as.character(2019:2023)) %>%
  rename_with(~glue("tl_{.x}"), where(is.numeric))


# add another 2023 column -------------------------------------------------
data_2023 = data_reordered %>%
  mutate(
    "2023" = tl_2023,
    .before = tl_2019
  )


# from 1 to 10km ----------------------------------------------------------
data_10km = data_2023 %>%
  mutate(
    across(where(is.numeric), ~.x * 10)
  )

# convert seconds to minutes ----------------------------------------------
data_minutes = data_10km %>%
  mutate(
    across(where(is.numeric), ~round(.x / 60, 1))
  )


write_csv(data_minutes, output_path)


