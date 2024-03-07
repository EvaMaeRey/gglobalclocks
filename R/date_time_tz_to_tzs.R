date_time_tz_to_tzs <- function(from_date_time = "2024-03-27 12:00:00", 
                                from_tz = "US/Mountain", 
                                to_tz = c("Europe/Paris", 
                                          "Europe/Amsterdam", 
                                          "Australia/Adelaide",
                                          "Australia/Melbourne", 
                                          "Europe/Stockholm", 
                                          "US/Eastern",
                                          "Europe/London", 
                                          "US/Pacific",
                                          "US/Mountain", 
                                          "US/Central", 
                                          "Australia/Sydney",
                                          "Europe/Vienna",
                                          "America/Sao_Paulo",
                                          "America/Santiago",
                                          "America/Buenos_Aires")){

   meeting <- ymd_hms(from_date_time, 
                      tz = from_tz)

OlsonNames() %>%
  data.frame(tz = .) %>%
  dplyr::filter(tz != "US/Pacific-New") %>%
  dplyr::filter(tz %in% to_tz) %>%
  dplyr::mutate(local_date_time_chr = purrr::map2(meeting, tz, time_to_local))  %>%
  tidyr::unnest(local_date_time_chr) %>%
  dplyr::mutate(local_time_date_utc = 
                  lubridate::ymd_hms(local_date_time_chr, tz = "UTC"))  %>% 
  dplyr::mutate(local_time = lubridate::ymd_hms(local_date_time_chr))  %>% 
  dplyr::mutate(local_time = hms::as_hms(local_time)) %>%
  dplyr::mutate(local_date = as.Date(local_date_time_chr)) %>%
  dplyr::mutate(local_wday = lubridate::wday(local_date, label = T)) %>%
  dplyr::arrange(local_date, local_time) %>%
  dplyr::select(-local_date_time_chr)

}
