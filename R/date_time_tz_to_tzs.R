date_time_tz_to_tzs <- function(from_date_time = "2024-03-27 12:00:00", 
                                from_tz = "US/Eastern", 
                                to_tz = c("Europe/Amsterdam", 
                                          "Australia/Adelaide",
                                          "Europe/Stockholm", 
                                          "US/Mountain", 
                                          "America/Santiago",
                                          "Asia/Seoul")){

   meeting <- ymd_hms(from_date_time, 
                      tz = from_tz)

OlsonNames() %>%
  data.frame(tz = .) %>%
  dplyr::filter(tz != "US/Pacific-New") %>%
  dplyr::filter(tz %in% to_tz) %>%
  dplyr::mutate(local_date_time_chr = purrr::map2(meeting, tz, time_to_local))  %>%
  tidyr::unnest(local_date_time_chr) %>%
  dplyr::mutate(local_date_time_utc = 
                  lubridate::ymd_hms(local_date_time_chr, tz = "UTC"))  %>% 
  # dplyr::mutate(local_time_hms = lubridate::ymd_hms(local_date_time_chr))  %>% 
  dplyr::mutate(local_time_hms = hms::as_hms(local_date_time_utc)) %>% 
  dplyr::mutate(local_time_hm = local_time_hms %>% str_remove("...$")) %>%
  dplyr::mutate(local_date = as.Date(local_date_time_chr)) %>%
  dplyr::mutate(local_wday = lubridate::wday(local_date, label = T)) %>%
  dplyr::arrange(local_date, local_time_hm) %>%
  dplyr::select(-local_date_time_chr) %>%
  dplyr::mutate(local_wday_date = paste0(local_wday, ", ", month(local_date, label = T), " ", day(local_date)))

}
