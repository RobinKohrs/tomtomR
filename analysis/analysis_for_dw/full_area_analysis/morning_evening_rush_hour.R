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
path_rush = here("data_raw/geo_full_area/rush_hour_combined_mor_eve/rushour.geojson")
path_avg = here("data_raw/geo_full_area/average/avg_all_times.geojson")


# read the data --------------------------------------------------------------------
# data_rush_raw = st_read(path_rushour, query = "SELECT * FROM foo LIMIT 200")
# data_avg_raw = st_read(path_avg, query = "SELECT * FROM foo LIMIT 200")

data_rush_raw = st_read(path_rush)
data_avg_raw = st_read(path_avg)

# save geo data -----------------------------------------------------------
data_geo = data_rush_raw %>% select(segmentId)

# clean the data ----------------------------------------------------------
# unnest the json column, add a time variable, and remove the geometry for now
data_rush = clean_full_area_data(data_rush_raw) %>% mutate(time="rush")
data_avg = clean_full_area_data(data_avg_raw) %>% mutate(time="avg")

# write out clean data
op_rush_clean = makePath(here("data_raw/geo_full_area/rush_hour_combined_mor_eve/clean_rushhour.gpkg"))
op_avg_clean = makePath(here("data_raw/geo_full_area/average//clean_avg.gpkg"))
write_sf(data_rush %>% select(where(~!is.list(.x))), op_rush_clean)
write_sf(data_avg %>% select(where(~!is.list(.x))), op_avg_clean)

data_rush = data_rush %>% st_drop_geometry()
data_avg = data_avg %>% st_drop_geometry()

# combine rush hour and normal flow ---------------------------------------
data_all = bind_rows(data_rush, data_avg)


# for each segment get the difference in the median speed ----------------------

per_segment_diff = data_all %>%
  filter(medianSpeed != 0) %>%
  group_by(segmentId, streetName, speedLimit, frc, distance) %>%
  summarise(
    diff_median_speed = medianSpeed[time=="rush"] - medianSpeed[time=="avg"],
    ratio_median_speed = medianSpeed[time=="rush"] / medianSpeed[time=="avg"],
    diff_avg_speed = harmonicAverageSpeed[time=="rush"] - harmonicAverageSpeed[time=="avg"],
    ratio_avg_speed = harmonicAverageSpeed[time=="rush"] / harmonicAverageSpeed[time=="avg"],
    sampleSize_rush = sampleSize[time=="rush"],
    sampleSize_avg = sampleSize[time=="avg"]
  )



# handle duplicate segments -----------------------------------------------

# if there are segment values which have the same id just differ by the sign, take the mean value of
# the difference in speed

per_segment_diff %>%
  mutate(direction = if_else(segmentId < 0,-1, 1),
         id_abs = abs(segmentId)) %>%
  group_by(id_abs) %>%
  summarise(
    mean_diff_median = mean(diff_median_speed),
    mean_ratio_median = mean(ratio_median_speed),
    mean_diff_avg = mean(diff_avg_speed),
    mean_ratio_avg = mean(ratio_avg_speed),
    segmentId = first(segmentId),
    n_times_seg = n(),
    segmentId = first(segmentId),
    streetName = first(streetName),
    speedLimit = first(speedLimit),
    frc = first(frc),
    distance = first(distance),
    distance_gr = paste0(distance, sep = ","),
    sampleSize_rush = mean(sampleSize_rush),
    sampleSize_avg = mean(sampleSize_avg),
  ) -> data_diff

# bind back the geodata
data_diff_geo = left_join(data_diff, data_geo, join_by(segmentId)) %>%
  st_as_sf()

op_geo_data = here("output/data/dw/full_area_analysis/per_segment_difference.geojson")
unlink(op_geo_data)
write_sf(data_diff_geo, op_geo_data)

# prepare for dw locator map ----------------------------------------------

# n classes
n_stops = 9
# colors
colors = rcartocolor::carto_pal(n=n_stops+2, "ArmyRose") %>% rev()

####### binning of the data

#  equally spaced bins
data_extent = c(min(data_diff_geo$mean_diff), max(data_diff_geo$mean_diff))
data_extent_min = -max(abs(round(data_extent)))
data_extent_max = max(abs(round(data_extent)))


breaks = seq(from=-5, to=5, length.out=n_stops + 1)

# labels
labels = imap_chr(breaks, function(., idx) {
  paste0(round(breaks[idx]), " - ", round(breaks[idx + 1]))
}) %>% .[1:length(.) - 1]

first_label = " < -8"
last_label = "> 8"

labels = c(first_label, labels, last_label)

# assign data to classes
data_diff_geo %>%
  mutate(
    class = findInterval(mean_diff, breaks),
    classname = labels[class+1],
    color = colors[class+1]
  )  -> data_singlepart

data_singlepart %>%
  group_by(class, classname, color) %>%
  summarise() -> data_dw


library(rmapshaper)
data_dw_simp = data_dw %>% ms_simplify()

# this as an option?
# https://academy.datawrapper.de/article/216-why-its-not-possible-to-zoom-in-published-locator-maps

op = makePath(here("output/data/dw/full_area_analysis/mean_diff_per_class.geojson"))
unlink(op)
write_sf(data_dw_simp, op)

