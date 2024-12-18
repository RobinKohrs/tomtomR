#######
# Script for creating the data for the heatmap with all the durations for 10 km of driving for many hours!
#######
library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)
library(readxl)


# paths data ---------------------------------------------------------------
data_weeks = list(
  list(
    year = 2023,
    path = here(
      "data_raw/wetransfer_headmap-fur-ndr_2023-12-28_1251/Headmap für NDR/2023/Hamburg_urban.csv"
    )
  ),
  list(
    year = 2022,
    path = here(
      "data_raw/wetransfer_headmap-fur-ndr_2023-12-28_1251/Headmap für NDR/2022/Hamburg_urban.csv"
    )
  ),
  list(
    year = 2021,
    path = here(
      "data_raw/wetransfer_headmap-fur-ndr_2023-12-28_1251/Headmap für NDR/2021/Hamburg_urban.csv"
    )
  ),
  list(
    year = 2020,
    path = here(
      "data_raw/wetransfer_headmap-fur-ndr_2023-12-28_1251/Headmap für NDR/2020/Hamburg_urban.csv"
    )
  ),
  list(
    year = 2019,
    path = here(
      "data_raw/wetransfer_headmap-fur-ndr_2023-12-28_1251/Headmap für NDR/2019/Hamburg_urban.csv"
    )
  )
)

# function to rename english weekdays -------------------------------------
renameWeekdays = function(wdays){
  lookup =  c(
    "Mon" = "MONDAY",
    "Die" = "TUESDAY",
    "Mit" = "WEDNESDAY",
    "Do" = "THURSDAY",
    "Fr" = "FRIDAY",
    "Sa" = "SATURDAY",
    "So" = "SUNDAY"
  )

 names(lookup)[match(wdays, lookup)]
}



# get the data ------------------------------------------------------------
data = map(data_weeks, function(e) {

  path = e$path
  year = e$year

  # the output path
  op = makePath(here(glue("output/data/dw/typical_week_{year}.csv")))

  # read data
  d = read_csv(path)

  # rename weekday
  d_ger = d %>% mutate(day = renameWeekdays(day))

  # make it wide
  d_ger %>%
    select(day, hour, timePerKmS) %>%
    mutate(timePerKmS = as.numeric(timePerKmS)) %>%
    mutate(
      timePerKmS = round((timePerKmS * 10) / 60,1) # to 10km and minutes
    ) %>%
    pivot_wider(names_from = day, values_from = timePerKmS) -> d_clean

  write_csv(d_clean, op)

  d_clean
})

