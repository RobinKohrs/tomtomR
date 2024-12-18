library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)


# path to segment data ----------------------------------------------------
path_data_segments = here("output/data/dw/full_area_analysis/per_segment_mean_diff_ratio.geojson")

# data per segment ---------------------------------------------------------
data_per_segment = read_sf(path_data_segments)

# -------------------------------------------------------------------------
# per street level
# -------------------------------------------------------------------------
data_per_segment %>%
  group_by(streetName) %>%
  summarise(
    mean_diff = weighted.mean(mean_diff, distance, na.rm = T),
    mean_ratio = weighted.mean(mean_ratio, distance, na.rm =
                                            T),
    mean_sample_size = mean(sampleSize_avg)
  ) -> mean_per_street


# per street or per segment! ----------------------------------------------
data = data_per_segment

# prepare for dw locator map ----------------------------------------------

# n classes
n_stops = 9
# colors
colors = scico::scico(n_stops+2, palette="vik") %>% rev()
colors[6] = "lightgrey"
# variable for binning the data
var = "mean_ratio"


#  equally spaced bins
data_extent = c(min(data[[var]], na.rm=T), max(data[[var]], na.rm=T))
data_extent_min = -max(abs(round(data_extent)))
data_extent_max = max(abs(round(data_extent)))


breaks = seq(from=0.5, to=1.5, length.out=n_stops + 1)

# labels
labels = imap_chr(breaks, function(., idx) {
  paste0(round(breaks[idx],1), " - ", round(breaks[idx + 1],1))
}) %>% .[1:length(.) - 1]

first_label = " < -0.5"
last_label = "> 1.5"

labels = c(first_label, labels, last_label)

# assign data to classes
data %>%
  filter(if_all({{var}}, ~!is.na(.x))) %>%
  mutate(
    class = findInterval(!!sym(var), breaks),
    classname = labels[class+1],
    color = colors[class+1]
  ) %>%
  group_by(class, classname, color) %>%
  summarise() %>%
  mutate("stroke-width" = 2.5) -> data_colored


library(rmapshaper)
data_dw_simp = data_colored %>% ms_simplify()


op = makePath(here("output/data/dw/full_area_analysis/overview_mean_ratio_street_per_class.geojson"))
unlink(op)
write_sf(data_dw_simp, op)

# for the detail maps make the lines thicker ------------------------------
data_detail = data_dw_simp %>%
  mutate(
    "stroke-width" = 6
  )

op_detail = makePath(here("output/data/dw/full_area_analysis/detail_mean_ratio_street_per_class.geojson"))
unlink(op_detail)
write_sf(data_detail, op_detail)

