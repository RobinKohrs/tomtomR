library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)
library(mapview)


# path to segment data ----------------------------------------------------
path_data_segments = here("output/data/dw/full_area_analysis/per_segment_mean_diff_ratio.geojson")

# data per segment ---------------------------------------------------------
data_per_segment = read_sf(path_data_segments)

# create grid -------------------------------------------------------------
d = data_per_segment %>% st_transform(3035)
grid = st_make_grid(d, 1000, square=F) %>% st_as_sf()

# for each grid cell get the mean ratio ---------------------------------
grid_ratios = map(seq_along(1:nrow(grid)), function(r){
  print(r)

  # the grid cell
  row = grid[r, ] %>% st_transform(4326)

  if(!lengths(st_intersects(row, data_per_segment)) > 0){
    print("no intersection")
    return(NA)
  }

  # get all the segments within
  segments = data_per_segment[row, ]
  mean_ratio = mean(segments$mean_ratio)
  row[["mean_ratio"]] = mean_ratio
  row


})

grid_ratios_no_na = grid_ratios[!is.na(grid_ratios)]


# create the mean grid ----------------------------------------------------
mean_grid = bind_rows(grid_ratios_no_na)


# remove the NAs ----------------------------------------------------------
mean_grid_no_na = mean_grid %>%
  filter(!is.na(mean_ratio)) %>%
  mutate(id = row_number())


# geo ---------------------------------------------------------------------
op_geo = makePath(here("output/data/dw/grid/grid_geo.geojson"))
unlink(op_geo)
write_sf(mean_grid_no_na %>% select(id), op_geo)


# data --------------------------------------------------------------------
op_data = makePath(here("output/data/dw/grid/grid_data.csv"))
mean_grid_no_na %>%
  st_drop_geometry() %>%
  write_csv(op_data)


# together ----------------------------------------------------------------

data = mean_grid_no_na

# n classes
n_stops = 9
# colors
colors = scico::scico(n_stops+2, palette="vik") %>% rev()
colors[6] = "lightgrey"
# variable for binning the data
var = "mean_ratio"


#  equally spaced bins
data_extent = c(min(data[[var]], na.rm=T), max(data[[var]], na.rm=T))
breaks = seq(from=0.85, to=1.15, length.out=n_stops + 1)

# labels
labels = imap_chr(breaks, function(., idx) {
  paste0(round(breaks[idx],1), " - ", round(breaks[idx + 1],1))
}) %>% .[1:length(.) - 1]
first_label = " < -0.85"
last_label = "> 1.15"
labels = c(first_label, labels, last_label)

# assign data to classes
data %>%
  filter(if_all({{var}}, ~!is.na(.x))) %>%
  mutate(
    class = findInterval(!!sym(var), breaks),
    classname = labels[class+1],
    color = colors[class+1],
    opacity = 90
  ) -> data_colored
  # group_by(class, classname, color) %>%
  # summarise() -> data_colored

op_locator = makePath(here("output/data/dw/grid/locator_grid.geojson"))
unlink(op_locator)
write_sf(data_colored, op_locator)
















