#######
# Script for creating the data for the full area analysis of Hamburg in 2023
# Idea is to compare the average all year speed for all stretes with the average speed during morning rush hour
# in order to spot the areas with lots of traffic in the mornings
#######

library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)
library(rmapshaper)
devtools::load_all()


# paths to data -----------------------------------------------------------
path_avg = here("data_raw/hamburg_2023/avg_all_times.geojson")
path_mor = here("data_raw/hamburg_2023/avg_mornings.geojson")

# read the data (subset only for now) -----------------------------------------------------------
# avg = st_read(path_avg, query = "SELECT * FROM foo LIMIT 200")
# mor = st_read(path_mor, query = "SELECT * FROM foo LIMIT 200")

avg = st_read(path_avg)
morning = st_read(path_mor)


# clean data --------------------------------------------------------------
geo_data = morning %>% select(segmentId)


data_clean = list(avg = avg, morning = morning) %>% imap(function(d, n) {
  d %>%
    st_drop_geometry() %>%
    filter(!is.na(segmentTimeResults)) %>%
    mutate(d = map(segmentTimeResults, function(c) {
      fromJSON(c) %>% as.data.frame()
    })) %>% unnest(d) %>% mutate(time = {{n}}) %>%
    select(segmentId,
           streetName,
           speedLimit,
           frc,
           distance,
           harmonicAverageSpeed:time) -> d_clean
  d_clean
})


# bind them togehter ------------------------------------------------------
data_all = bind_rows(data_clean)


# group by segment id -----------------------------------------------------
diffs = data_all %>%
  group_by(segmentId) %>%
  summarise(
    diff_kmh = medianSpeed[time == "morning"] - medianSpeed[time == "avg"],
    sample_size_mor = sampleSize[time == "morning"],
    sample_size_avg = sampleSize[time == "avg"],
    # should be the same?!
    distance_mor = distance[time == "morning"],
    streetName = streetName[time == "morning"],
    frc = frc[time == "morning"]
  )
