time_to_local <- function(x, tz){
  lubridate::with_tz(x, tz = tz) %>%
    as.character()
}
