#######
# Script for creating the data for the graphic on the stats for one year of travelling to work by car
# Assumptions:
#  - Travel every day Mon - Fri
#  - Each way 10 km
#  - 230 days per Year
#  - Always travel at rushhour time
#######

library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)
devtools::load_all()


# output path -------------------------------------------------------------
output_path = here(makePath("output/data/dw/one_year_travelling.csv"))

# params ------------------------------------------------------------------
area_selected = "urban"
year_selected=2023
city_selected="Hamburg"

TRAVEL_DAYS_PER_YEAR = 230
TRAVEL_DISTANCE_ONE_WAY_KM = 10


# data on german cities --------------------------------------------------------------------
data = tomtomR::data_tomtom

# select necessary columns ------------------------------------------------
data_selected = data %>%
  select(
    city,
    area,
    year,
    amPeakTravelTimePerKm,
    pmPeakTravelTimePerKm,
    amPeakTimeLostPerKm,
    pmPeakTimeLostPerKm,
    yearTravelTimePerKm,
    yearTravelTimeOptimumPerKm
  )


# filter to urban, hamburg and 2023 ---------------------------------------------------------
data_urban_2023_hamburg = data_selected %>% filter(area == {{area_selected}}, year=={{year_selected}}, city=={{city_selected}})


# create columns ----------------------------------------------------------
data_urban_2023_hamburg %>%
  mutate(
    travel_km_one_year = TRAVEL_DAYS_PER_YEAR * (2 * TRAVEL_DISTANCE_ONE_WAY_KM),
    travel_time_overall_sec = ((travel_km_one_year/2) * amPeakTravelTimePerKm) + ((travel_km_one_year / 2) * pmPeakTravelTimePerKm),
    traffic_time_overall_sec = ((travel_km_one_year/2) * amPeakTimeLostPerKm) + ((travel_km_one_year / 2) * pmPeakTimeLostPerKm),
  ) %>%
  mutate(
    travel_time_overall_h = round(travel_time_overall_sec / (60 * 60)),
    traffic_time_overall_h = round(traffic_time_overall_sec / (60 * 60)),
    mean_travel_time_10km_min = (yearTravelTimePerKm * 10) / 60,
    optimal_travel_time_10km_min = (yearTravelTimeOptimumPerKm * 10) / 60
  ) %>%
  select(
    city, travel_time_overall_h:optimal_travel_time_10km_min
  ) -> data_one_year


# write out ---------------------------------------------------------------
write_csv(data_one_year, output_path)


# make it long for a bar chart in dw --------------------------------------
data_one_year_long  = data_one_year %>%
  pivot_longer(
    -city,
    names_to = "variable",
    values_to = "value"
  )

# overwrite output ---------------------------------------------------------------
write_csv(data_one_year_long, output_path)












