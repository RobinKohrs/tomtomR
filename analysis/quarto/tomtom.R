library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)
library(readxl)

# data loading ------------------------------------------------------------
path_data_csv = here("data_raw/AW Daten des TomTom Traffic Index 2024/231222 TomTom Traffic Stats_All Cities.csv")
path_data_excel = here("data_raw/AW Daten des TomTom Traffic Index 2024/231222 TomTom Traffic Index Stats_All Cities.xlsx")

data_csv = read_csv(path_data_csv)
data_xls = read_xlsx(path_data_excel)

# use csv for now ---------------------------------------------------------
data = data_csv




