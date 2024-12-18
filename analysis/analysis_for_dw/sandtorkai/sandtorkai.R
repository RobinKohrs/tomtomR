library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)


# paths -------------------------------------------------------------------
paths_geojsons = dir(here("data_raw/sandtorkai/"), ".*\\.geojson$", recursive = T, full.names = T)

map(paths_geojsons, function(p){

  clean_name = basename(p) %>% janitor::make_clean_names()
  parentdir = dirname(p) %>% basename()

  kartenversion = "2018"

  if(str_detect(parentdir, "2020")){
    kartenversion = "2020"
  }

  if(str_detect(parentdir, "2022")){
    kartenversion = "2022"
  }


  # read data
  d = read_sf(p)
  d_clean = clean_full_area_data(d)


})

