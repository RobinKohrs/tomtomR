library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)


# data -----------------------------------------------------------------------
p_geo = here("data_raw/geo_test/jobs_3220858_results_Hamburg_-_2017_.geojson")
p_non_geo = here("data_raw/geo_test/jobs_3220858_results_Hamburg_-_2017_.json")

d_geo = read_sf(p_geo)

# get the data out --------------------------------------------------------
d_clean = d_geo %>%
  filter(!is.na(segmentTimeResults)) %>%
  mutate(d = map(segmentTimeResults, function(c) {
    fromJSON(c) %>% as.data.frame()
  })) %>% unnest(d) %>%
  mutate(speedPercentiles = map_chr(speedPercentiles, ~ paste0(.x, collapse = ","))) %>%
  select(-mapsVersions)


# difference between speedlimit and median speed --------------------------
d_clean_diff=d_clean %>%
  mutate(
    diff_median = speedLimit - medianSpeed,
    diff_avg = speedLimit - averageSpeed
  )

# write it out ------------------------------------------------------------
d_final  = d_clean_diff
op = makePath(here("data_raw/geo_test/d_clean.fgb"))
unlink(op)
write_sf(d_final, op)


# try averaging the values per street -------------------------------------
d_per_street = d_clean_diff %>%
  group_by(streetName) %>%
  summarise(
    mean_speed = weighted.mean(averageSpeed, sampleSize),
    mean_diff = weighted.mean(diff_avg, sampleSize)
  )

op_group = makePath(here("data_raw/geo_test/unioned_per_street.fgb"))
unlink(op_group)
write_sf(d_per_street, op_group)

