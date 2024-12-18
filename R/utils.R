# Create directories until Path
#'
#' @param path Path to file that is going to be created
#'
#' @return The path
#' @export
#'
makePath = function(path) {
  is_file = str_detect(basename(path), "\\.")
  if (is_file) {
    dirFromPath = dirname(path)
    if (!dir.exists(dirFromPath)) {
      dir.create(dirFromPath, recursive = T)
    }
    return(path)
  }
  else {
    dir.create(path, recursive = T)
    return(path)
  }
}

# function to rename german cities and translate to German
#'
#' @param df
#'
#' @return The tom-tom data with german cities translated
#' @export
#'
translateGermanCities = function(df){
  df %>%
    mutate(
      city = case_when(
        city == "Frankfurt-am-main" ~ "Frankfurt am Main",
        city == "Cologne" ~ "Köln",
        city == "Dusseldorf" ~ "Düsseldorf",
        city == "Monchengladbach" ~ "Mönchengladbach",
        city == "Munich" ~ "München",
        city == "Nuremberg" ~ "Nürnberg",
        city == "Munster" ~ "Münster",
        city == "Ruhr-region-east" ~ "Ruhrregion Ost",
        city == "Ruhr-region-west" ~ "Ruhrregion West",
        .default = city
      )
    )  -> df
}

#' Get the data in the json column
#'
#' @param a full area report from tomtom as sf-dataframe
#'
#' @return a dataframe with a unnested segmentTimeResults column
#' @export
#'
clean_full_area_data = function(df){
  df %>%
    filter(!is.na(segmentTimeResults)) %>%
    mutate(d = map(segmentTimeResults, function(c) {
      fromJSON(c) %>% as.data.frame()
    })) %>% unnest(d) %>%
    select(
      segmentId,
      streetName,
      speedLimit,
      frc,
      distance,
      harmonicAverageSpeed:speedPercentiles
    ) -> d_clean
  d_clean
}
