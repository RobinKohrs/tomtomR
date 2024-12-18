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


walk(data_weeks, function(e){

  path = e$path
  year = e$year

  # the output path
  op = makePath(here(glue("output/data/dw/congestion_level/daily_congestion_level{year}.csv")))

  # read data
  d = read_csv(path)

  # rename weekday
  d_ger = d %>% mutate(day = renameWeekdays(day))

  # average over the hours
  d_ger %>%
    group_by(hour, day) %>%
    summarise(mean_congestionLevel = mean(congestionLevel)) %>%
    pivot_wider(
      names_from = day,
      values_from = mean_congestionLevel
    ) %>%
    select(
      hour, Mon, Die, Mit, Do, Fr, Sa, So
    ) -> d_clean

  write_csv(d_clean, op)


})









