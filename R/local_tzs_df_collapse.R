local_tzs_df_collapse <- function(local_tzs_df, collapse = "; "){
  
  local_tzs_df |>
    group_by(local_date, local_date_time_utc, local_time_hm, local_wday_date) |>
    summarise(locations = paste(tz, collapse = collapse)) |>
    ungroup() |>
    select(locations, everything()) |>
    mutate(locations = str_remove_all(locations, "Europe/|America/|Australia/")) |>
    arrange(local_date_time_utc) |>
    mutate(locations = fct_inorder(locations)) |>
    select(-local_date) |>
    select(everything(), local_date_time_utc)
  
}
