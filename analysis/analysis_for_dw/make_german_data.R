library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)
library(readxl)

path_data_csv = here("data_raw/AW Daten des TomTom Traffic Index 2024/231222 TomTom Traffic Stats_All Cities.csv")
data = read_csv(path_data_csv)
data_tomtom = data %>% filter(country=="Germany") %>% translateGermanCities()

usethis::use_data(data_tomtom)
