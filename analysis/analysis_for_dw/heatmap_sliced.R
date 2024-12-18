library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)

# paths data ---------------------------------------------------------------
path_data = here(
  "data_raw/wetransfer_headmap-fur-ndr_2023-12-28_1251/Headmap für NDR/2023/Hamburg_urban.csv"
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


# read data ---------------------------------------------------------------
data = read_csv(path_data)


# translate to german -----------------------------------------------------
data_ger = data %>% mutate(day = renameWeekdays(day))


#######
# Zeit pro Km -------------------------------------------------------------
#######

# make it wide for datawrapper - weekdays as cols -------------------------
data_clean_10k = data_ger %>%
  select(day, hour, timePerKmS) %>%
  mutate(timePerKmS = as.numeric(timePerKmS)) %>%
  mutate(timePerKmS = round((timePerKmS * 10) / 60, 1)) %>%  # to 10km and minutes not seconds
         pivot_wider(names_from = day, values_from = timePerKmS)


# a typical day
# one value for each hour for weekday and weekend

op_typical_day = makePath(here(
  "output/data/dw/a_typical_day/time_10km_weekday_weekend.csv"
))

data_clean_10k %>%
  rowwise() %>%
  mutate(wday = mean(c_across(Mon:Fr)),
         weekend = mean(c_across(Sa:So))) %>%
  select(hour, wday, weekend) -> data_typical_day

write_csv(data_typical_day, op_typical_day)

# comapre 16 clock over all days
# one value for each weekday

op_16_hour = makePath(here("output/data/dw/a_typical_day/time_10km_16_hour_all_days.csv"))
data_clean_10k %>%
  filter(hour == 16) %>%
  pivot_longer(cols = Mon:So,
               names_to = "day",
               values_to = "time") %>% select(-hour) -> data_16_hour

write_csv(data_16_hour, op_16_hour)





#######
# Congestion Level  -------------------------------------------------------------
#######

# make it wide for datawrapper - weekdays as cols -------------------------
data_clean_congestionLevel = data_ger %>%
  select(day, hour, congestionLevel) %>%
         pivot_wider(names_from = day, values_from = congestionLevel)

###
# a typical day
###

op_typical_day_congestionLevel = makePath(here(
  "output/data/dw/a_typical_day/congestionLevel_weekday_weekend.csv"
))

data_clean_congestionLevel %>%
  rowwise() %>%
  mutate(wday = mean(c_across(Mon:Fr)),
         weekend = mean(c_across(Sa:So))) %>%
  select(hour, wday, weekend) -> data_congestionLevel_typical_day

write_csv(data_congestionLevel_typical_day, op_typical_day_congestionLevel)

###
# comapre 17 clock over all days
###

op_17_hour_congestionLevel = makePath(here("output/data/dw/a_typical_day/congestionLevel_17_hour_all_days.csv"))

data_clean_congestionLevel %>%
  filter(hour == 17) %>%
  pivot_longer(cols = Mon:So,
               names_to = "day",
               values_to = "time") %>% select(-hour) -> data_17_hour_congestionLevel

write_csv(data_17_hour_congestionLevel, op_17_hour_congestionLevel)




