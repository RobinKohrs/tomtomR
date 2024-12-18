library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)
library(tidygeocoder)


# path to segment data ----------------------------------------------------
path_data_segments = here("output/data/dw/full_area_analysis/per_segment_mean_diff_ratio.geojson")

# data per segment ---------------------------------------------------------
data_per_segment = read_sf(path_data_segments)

# variable to use ---------------------------------------------------------
var = "mean_ratio"

# top and flop segment centroids ------------------------------------------------------------
top_flop_centroids = data_per_segment %>%
  filter(!is.na(!!sym(var))) %>%
  arrange(mean_ratio) %>%
  slice(c(1, nrow(.))) %>%
  st_centroid() %>%
  st_as_sf() %>%
  select(name=streetName)


# adresses ----------------------------------------------------------------
adresses = list(list(
  name = "Flughafen",
  coords = c(53.62591092417195, 10.00400790583759)
),
list(
  name = "Elbbrücken",
  coords = c(53.538633176280754, 10.03127604254123)
))

pois = map(adresses, function(a) {
  list(name = a$name,
       x = a$coords[2],
       y = a$coords[1])
}) %>% bind_rows() %>% st_as_sf(coords = c("x", "y"), crs = 4326)



# bind all together -------------------------------------------------------
all_highlights = bind_rows(top_flop_centroids, pois) %>%
  mutate(
    icon="circle-out-2",
    "stroke-width" = 2,
    "fill-opacity" = 0,
    scale = .3,
    "text-bold" = 1,
    anchor = case_when(
      name == "Flughafen" ~ "top-left",
      name == "Überseering" ~ "top-right",
      name == "Reeperbahn" ~ "bottom-left",
      name == "Elbbrücken" ~ "bottom-right",
    )
  )

# write out ---------------------------------------------------------------
op_highlight_overviews = makePath(here("output/data/dw/full_area_analysis/highlight_overview_maps.geojson"))
unlink(op_highlight_overviews)
write_sf(all_highlights, op_highlight_overviews)














