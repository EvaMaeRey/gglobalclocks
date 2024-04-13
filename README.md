
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# gglobalclocks

What if you could draw a beautiful ‘wall’ of clocks with local times
from around the world? Well, gglobalclocks lets you do that\!

I worked on the wall of clocks thinking it might help me schedule
virtual meetings with participant in lots of different time zones. Does
a beautiful wall of clocks help schedule global meetings? As it turns
out, in my experience, not really\!

But gglobalclocks also has utilities for building dataframes of
locations and local times which I have found helpful for multi-timezone
scheduling.

``` r
library(tidyverse)
gglobalclocks:::date_time_tz_to_tzs() |> 
  gglobalclocks:::local_tzs_df_collapse() |>
  gglobalclocks:::gglobalclocks() + 
    aes(local_time_hm = local_time_hm) + 
    gglobalclocks:::stamp_clockface() + 
    gglobalclocks:::geom_clock_hands() + 
    facet_wrap(~str_wrap(locations, 20))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
  
tz_targets <- c("US/Mountain","US/Eastern","US/Pacific",  "Europe/Amsterdam", "America/Sao_Paulo", "America/Santiago", "Australia/Adelaide","Australia/Melbourne", "Europe/Vienna", "Europe/London", "Asia/Seoul")

gglobalclocks:::date_time_tz_to_tzs(
  from_date_time = "2024-05-11 10:00:00",
  from_tz = "US/Eastern",
  to_tz = tz_targets)
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `local_date_time_utc = lubridate::ymd_hms(local_date_time_chr,
#>   tz = "UTC")`.
#> Caused by warning:
#> !  1 failed to parse.
#> # A tibble: 11 × 7
#>    tz     local_date_time_utc local_time_hms local_time_hm local_date local_wday
#>    <chr>  <dttm>              <time>         <chr>         <date>     <ord>     
#>  1 US/Pa… 2024-05-11 07:00:00 07:00          07:00         2024-05-11 Sat       
#>  2 US/Mo… 2024-05-11 08:00:00 08:00          08:00         2024-05-11 Sat       
#>  3 Ameri… 2024-05-11 10:00:00 10:00          10:00         2024-05-11 Sat       
#>  4 US/Ea… 2024-05-11 10:00:00 10:00          10:00         2024-05-11 Sat       
#>  5 Ameri… 2024-05-11 11:00:00 11:00          11:00         2024-05-11 Sat       
#>  6 Europ… 2024-05-11 15:00:00 15:00          15:00         2024-05-11 Sat       
#>  7 Europ… 2024-05-11 16:00:00 16:00          16:00         2024-05-11 Sat       
#>  8 Europ… 2024-05-11 16:00:00 16:00          16:00         2024-05-11 Sat       
#>  9 Asia/… 2024-05-11 23:00:00 23:00          23:00         2024-05-11 Sat       
#> 10 Austr… 2024-05-11 23:30:00 23:30          23:30         2024-05-11 Sat       
#> 11 Austr… NA                     NA          <NA>          2024-05-12 Sun       
#> # ℹ 1 more variable: local_wday_date <chr>

df <- gglobalclocks:::date_time_tz_to_tzs(
  from_date_time = "2024-05-11 10:00:01",
  from_tz = "US/Eastern",
  to_tz = tz_targets) |> 
  gglobalclocks:::local_tzs_df_collapse()

df |> select(-local_date_time_utc)
#> # A tibble: 9 × 3
#>   locations            local_time_hm local_wday_date
#>   <fct>                <chr>         <chr>          
#> 1 US/Pacific           07:00         Sat, May 11    
#> 2 US/Mountain          08:00         Sat, May 11    
#> 3 Santiago; US/Eastern 10:00         Sat, May 11    
#> 4 Sao_Paulo            11:00         Sat, May 11    
#> 5 London               15:00         Sat, May 11    
#> 6 Amsterdam; Vienna    16:00         Sat, May 11    
#> 7 Asia/Seoul           23:00         Sat, May 11    
#> 8 Adelaide             23:30         Sat, May 11    
#> 9 Melbourne            00:00         Sun, May 12

df |>  
  ggplot() + 
  gglobalclocks:::stamp_workday() +
  aes(x = local_time_hm %>% paste0(":00") %>% hms::as_hms(), 
      y = fct_inorder(locations) %>% fct_rev()) + 
  geom_point(shape = "|", size = 8) +
  # geom_rect(aes(xmin =  local_time_hm %>% paste0(":00") %>% hms::as_hms(),
  #           xmax = local_time_hm %>% paste0(":00") %>% hms::as_hms() + minutes(50),
  #           ymin = fct_inorder(locations) %>% fct_rev() - .25,
  #           ymax = fct_inorder(locations) %>% fct_rev() + .25))
  geom_label(aes(label = paste(local_time_hm, ""), 
                 hjust = hms::as_hms(local_date_time_utc) > hms::as_hms("17:00:00")),  
             label.size = NA, vjust = .9, alpha = 0) +
  geom_label(aes(label = fct_inorder(locations) %>% fct_rev(),
                 hjust = hms::as_hms(local_date_time_utc) > hms::as_hms("17:00:00")),
             vjust = .1, label.size = NA, alpha = 0) + 
  aes(color = local_wday_date) +
  theme(axis.text.y = element_text(hjust = 1, size = 8)) + 
  scale_fill_manual(values = monochromeR::generate_palette("navy", "go_lighter", 3)) + 
  labs(x = NULL, y = NULL, fill = NULL, color = NULL) + 
  theme_classic() + 
  scale_color_manual(values = c("navy", "goldenrod2")) + 
  theme_void() + 
  theme(legend.position = "top", legend.justification = "left")  +
  geom_label(data = gglobalclocks:::create_day_schedule_df(), 
            aes(x = time_start, y = 10, 
                label = time_start %>% str_remove("...$")), 
            angle = 0, color = "gray",
            alpha = .95,
            hjust = -.05, 
            size = 3,
            vjust = 1) + 
  geom_vline(xintercept = "12:00:00" %>% hms::as_hms(), color = "grey", 
             linetype = "dotted") + 
  labs(title= "Local start times for ggplot2 extenders Saturday, May 11, 2024 meeting") + 
  theme(plot.title.position = "plot") + 
  guides(label = "none", color = "none") + 
  geom_label(data = . %>% distinct(local_wday_date) %>%
               mutate(y = c(1,-1)), aes(label =  local_wday_date, y = y/3.5 + 1.5),
             x = hms::as_hms("05:00:00"), alpha = 0, label.size = 0) + 
  geom_hline(yintercept = 1.5, color = "goldenrod2", linetype = "dotted")
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" />

``` r

df |> distinct(local_wday_date)
#> # A tibble: 2 × 1
#>   local_wday_date
#>   <chr>          
#> 1 Sat, May 11    
#> 2 Sun, May 12
  
  
gglobalclocks:::create_day_schedule_df()  
#>   time_start time_end   stance
#> 1   00:00:00 07:00:00    avoid
#> 2   07:00:00 09:00:00    awake
#> 3   09:00:00 17:00:00 business
#> 4   17:00:00 21:00:00    awake
#> 5   21:00:00 24:00:00    avoid

# Sys.Date() |> paste0 ("00:00:00") + minutes(50)

# hms::as_hms("00:00:00")
```

``` r
library(tidyverse)
library(lubridate)
```

-----

## Part II. Functions discussion and definitions

### `time_to_local()`

One helper function to do translation from one time zone to multiple
time zones is time\_to\_local. From working on global clocks, it seems
like you can’t keep date-times with different time zones in one vector
(or variable), so you need to do a conversion one by one and save a
character version of the complete time zone information.

``` r
time_to_local <- function(x, tz){
  lubridate::with_tz(x, tz = tz) %>%
    as.character()
}
```

### `date_time_tz_to_tzs()`

We can use the converter above to translate from a ‘from’ location and
time to a bunch of locations’ local times. We do this and then add a few
more helpful columns like local\_time, local\_date etc.

``` r
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
```

``` r
date_time_tz_to_tzs() |>
  head()
#> # A tibble: 6 × 7
#>   tz      local_date_time_utc local_time_hms local_time_hm local_date local_wday
#>   <chr>   <dttm>              <time>         <chr>         <date>     <ord>     
#> 1 US/Mou… 2024-03-27 10:00:00 10:00          10:00         2024-03-27 Wed       
#> 2 Americ… 2024-03-27 13:00:00 13:00          13:00         2024-03-27 Wed       
#> 3 Europe… 2024-03-27 17:00:00 17:00          17:00         2024-03-27 Wed       
#> 4 Europe… 2024-03-27 17:00:00 17:00          17:00         2024-03-27 Wed       
#> 5 Asia/S… 2024-03-28 01:00:00 01:00          01:00         2024-03-28 Thu       
#> 6 Austra… 2024-03-28 02:30:00 02:30          02:30         2024-03-28 Thu       
#> # ℹ 1 more variable: local_wday_date <chr>
```

### `local_tzs_df_collapse()`

If it turns out that you have multiple tz with the same local meeting
time, you can collapse these locations by local time.

``` r
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
```

This is the type of information you might send to an attendees list so
they can know, at a glance, their likelihood of making attending work.

``` r
date_time_tz_to_tzs() |> 
  local_tzs_df_collapse() |> 
  head()
#> `summarise()` has grouped output by 'local_date', 'local_date_time_utc',
#> 'local_time_hm'. You can override using the `.groups` argument.
#> # A tibble: 5 × 4
#>   locations            local_date_time_utc local_time_hm local_wday_date
#>   <fct>                <dttm>              <chr>         <chr>          
#> 1 US/Mountain          2024-03-27 10:00:00 10:00         Wed, Mar 27    
#> 2 Santiago             2024-03-27 13:00:00 13:00         Wed, Mar 27    
#> 3 Amsterdam; Stockholm 2024-03-27 17:00:00 17:00         Wed, Mar 27    
#> 4 Asia/Seoul           2024-03-28 01:00:00 01:00         Thu, Mar 28    
#> 5 Adelaide             2024-03-28 02:30:00 02:30         Thu, Mar 28
```

# More charming display…

You can of course display in a nicer way, passing to html table maker
for example.

``` r
date_time_tz_to_tzs() |> 
  local_tzs_df_collapse() |> 
  select(-local_date_time_utc) |>
  knitr::kable()
#> `summarise()` has grouped output by 'local_date', 'local_date_time_utc',
#> 'local_time_hm'. You can override using the `.groups` argument.
```

| locations            | local\_time\_hm | local\_wday\_date |
| :------------------- | :-------------- | :---------------- |
| US/Mountain          | 10:00           | Wed, Mar 27       |
| Santiago             | 13:00           | Wed, Mar 27       |
| Amsterdam; Stockholm | 17:00           | Wed, Mar 27       |
| Asia/Seoul           | 01:00           | Thu, Mar 28       |
| Adelaide             | 02:30           | Thu, Mar 28       |

### Let’s build a wall of global clocks with base ggplot2

``` r
# local_time_hm_to_hms <- 

date_time_tz_to_tzs() |> 
  local_tzs_df_collapse() |>
  mutate(local_time_hms = local_time_hm %>% paste0(":00") %>% hms::as_hms() ) |>
  mutate(minute_turn = local_time_hms %>% 
           lubridate::minute() %% 60 / 60) %>% 
  mutate(hour_turn = local_time_hms %>% 
           lubridate::hour() %% 12/12 + minute_turn/12) %>% 
  mutate(am_pm = ifelse(local_time_hms %>% lubridate::hour() >= 12, "pm", "am")) %>% 
  ggplot() + 
  facet_wrap(~ str_wrap(locations, 15)) + 
  aes(x =  hour_turn, 
      xend =  hour_turn,
      y = 0,
      yend = .6,
      color = am_pm) + 
  labs(color = NULL) +
  geom_segment() + 
  geom_segment(aes(x =  minute_turn, 
                  xend =  minute_turn ,
               yend = 1) ) + 
  coord_polar() + 
  scale_x_continuous(limits = c(0,1)) + 
  geom_text(data = tibble(x = 1:12, y = 1), 
            aes(label = x, x = x/12, y = y, xend = NULL, 
                yend = NULL, color = NULL),
            show.legend = F) + 
  theme_void() + 
  annotate(geom = "segment", x = 0, xend = 1, y = 1.2, yend = 1.2)
#> `summarise()` has grouped output by 'local_date', 'local_date_time_utc',
#> 'local_time_hm'. You can override using the `.groups` argument.
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

## Speed things up by putting some of this into functions

### `gglobalclocks()`

``` r
gglobalclocks <- function(data = NULL){
  
  ggplot2::ggplot(data) + 
  list(ggplot2::theme_void(),
       ggplot2::coord_polar(),
       ggplot2::scale_x_continuous(limits = c(0,1)),
       ggplot2::scale_y_continuous(limits = c(0,1.3)))
  
}
```

### `stamp_clockface()`

``` r
stamp_clockface <- function(){
  
    list(ggplot2::geom_text(data = data.frame(x = 1:12, y = 1), 
            ggplot2::aes(label = x, x = x/12, y = y, xend = NULL, 
                yend = NULL, color = NULL, local_time_hm = NULL),
            show.legend = F),
          ggplot2::annotate(geom = "segment", x = 0, xend = 1, y = 1.2, yend = 1.2)
         
    )
  
}
```

``` r
gglobalclocks() + stamp_clockface()
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

### `statClockhourhand`

``` r
# there's probably a lot of fun to be had refactoring this!
# seems quite verbose
compute_clock_hands <- function(data){
  
  data |> 
  dplyr::mutate(local_time_hms = local_time_hm %>% paste0(":00") %>% hms::as_hms() ) |>
  dplyr::mutate(minute_turn = local_time_hms %>% lubridate::minute() %% 60 / 60) |> 
  dplyr::mutate(hour_turn = local_time_hms %>% lubridate::hour() %% 12/12 + minute_turn/12) |>
  dplyr::mutate(am_pm = ifelse(local_time_hms %>% lubridate::hour() > 12, "pm", "am")) 
  
}

compute_hour_hand <- function(data, scales){
  
  data |>
    compute_clock_hands() |>
    dplyr::mutate(x =  hour_turn, 
      xend =  hour_turn,
      y = 0,
      yend = .6)
  
}

compute_minute_hand <- function(data, scales){
  
  data |>
    compute_clock_hands() |>
    dplyr::mutate(x =  minute_turn, 
      xend =  minute_turn,
      # color = am_pm, 
      y = 0,
      yend = 1)
  
}


statClockminhand <- ggplot2::ggproto(`_class` = "statClockminhand",
                          `_inherit` = ggplot2::Stat,
                          # required_aes = c("local_time"),
                          compute_group = compute_minute_hand,
                          default_aes = ggplot2::aes(color =
                                                            ggplot2::after_stat(am_pm))
                           )

geom_minute_hand <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = statClockminhand,  # proto object from step 2
    geom = ggplot2::GeomSegment,  # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

statClockhourhand <- ggplot2::ggproto(`_class` = "statClockhourhand",
                          `_inherit` = ggplot2::Stat,
                          # required_aes = c("local_time"),
                          compute_group = compute_hour_hand,
                          default_aes = ggplot2::aes(color =
                                                            ggplot2::after_stat(am_pm))
                           )
```

``` r
date_time_tz_to_tzs() |> compute_clock_hands()
#> # A tibble: 6 × 10
#>   tz      local_date_time_utc local_time_hms local_time_hm local_date local_wday
#>   <chr>   <dttm>              <time>         <chr>         <date>     <ord>     
#> 1 US/Mou… 2024-03-27 10:00:00 10:00          10:00         2024-03-27 Wed       
#> 2 Americ… 2024-03-27 13:00:00 13:00          13:00         2024-03-27 Wed       
#> 3 Europe… 2024-03-27 17:00:00 17:00          17:00         2024-03-27 Wed       
#> 4 Europe… 2024-03-27 17:00:00 17:00          17:00         2024-03-27 Wed       
#> 5 Asia/S… 2024-03-28 01:00:00 01:00          01:00         2024-03-28 Thu       
#> 6 Austra… 2024-03-28 02:30:00 02:30          02:30         2024-03-28 Thu       
#> # ℹ 4 more variables: local_wday_date <chr>, minute_turn <dbl>,
#> #   hour_turn <dbl>, am_pm <chr>
date_time_tz_to_tzs() |> compute_minute_hand()
#> # A tibble: 6 × 14
#>   tz      local_date_time_utc local_time_hms local_time_hm local_date local_wday
#>   <chr>   <dttm>              <time>         <chr>         <date>     <ord>     
#> 1 US/Mou… 2024-03-27 10:00:00 10:00          10:00         2024-03-27 Wed       
#> 2 Americ… 2024-03-27 13:00:00 13:00          13:00         2024-03-27 Wed       
#> 3 Europe… 2024-03-27 17:00:00 17:00          17:00         2024-03-27 Wed       
#> 4 Europe… 2024-03-27 17:00:00 17:00          17:00         2024-03-27 Wed       
#> 5 Asia/S… 2024-03-28 01:00:00 01:00          01:00         2024-03-28 Thu       
#> 6 Austra… 2024-03-28 02:30:00 02:30          02:30         2024-03-28 Thu       
#> # ℹ 8 more variables: local_wday_date <chr>, minute_turn <dbl>,
#> #   hour_turn <dbl>, am_pm <chr>, x <dbl>, xend <dbl>, y <dbl>, yend <dbl>
date_time_tz_to_tzs() |> compute_hour_hand()
#> # A tibble: 6 × 14
#>   tz      local_date_time_utc local_time_hms local_time_hm local_date local_wday
#>   <chr>   <dttm>              <time>         <chr>         <date>     <ord>     
#> 1 US/Mou… 2024-03-27 10:00:00 10:00          10:00         2024-03-27 Wed       
#> 2 Americ… 2024-03-27 13:00:00 13:00          13:00         2024-03-27 Wed       
#> 3 Europe… 2024-03-27 17:00:00 17:00          17:00         2024-03-27 Wed       
#> 4 Europe… 2024-03-27 17:00:00 17:00          17:00         2024-03-27 Wed       
#> 5 Asia/S… 2024-03-28 01:00:00 01:00          01:00         2024-03-28 Thu       
#> 6 Austra… 2024-03-28 02:30:00 02:30          02:30         2024-03-28 Thu       
#> # ℹ 8 more variables: local_wday_date <chr>, minute_turn <dbl>,
#> #   hour_turn <dbl>, am_pm <chr>, x <dbl>, xend <dbl>, y <dbl>, yend <dbl>
```

### `geom_clock_hands()`

``` r
geom_hour_hand <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = statClockhourhand,  # proto object from step 2
    geom = ggplot2::GeomSegment,  # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


geom_clock_hands <- function(...){
  
  list(geom_hour_hand(...),
       geom_minute_hand(...))
  
}
```

### Try it out

``` r
date_time_tz_to_tzs() |>
  local_tzs_df_collapse() |>
gglobalclocks() +
  aes(local_time_hm = local_time_hm) +
  stamp_clockface() +
  geom_minute_hand() +
  geom_hour_hand() +
  facet_wrap(~locations)
#> `summarise()` has grouped output by 'local_date', 'local_date_time_utc',
#> 'local_time_hm'. You can override using the `.groups` argument.
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

``` r

date_time_tz_to_tzs() |> 
  local_tzs_df_collapse() |>
  gglobalclocks() + 
  aes(local_time_hm = local_time_hm) + 
  stamp_clockface() + 
  geom_clock_hands() + 
  facet_wrap(~locations)
#> `summarise()` has grouped output by 'local_date', 'local_date_time_utc',
#> 'local_time_hm'. You can override using the `.groups` argument.
```

<img src="man/figures/README-unnamed-chunk-10-2.png" width="100%" />

``` r
readme2pkg::chunk_to_r("gglobalclocks")
readme2pkg::chunk_to_r("stamp_clockface")

readme2pkg::chunk_to_r("statClockhourhand")
readme2pkg::chunk_to_r("geom_clock_hands")
```

## If a wall of clocks is unhelpful in scheduling, can we viz in a different way? Towards LocationXLocaTime plot

### `stamp_workday()`

We’ll use an ‘stamp’ (annotation) layer that helps us get oriented to
the 12 hours in a work day.

``` r
create_day_schedule_df <- function(){
  
  data.frame(time_start = 
               hms::as_hms(c("00:00:00","07:00:00", "09:00:00", "17:00:00","21:00:00")), 
             time_end = 
               hms::as_hms(c("07:00:00","09:00:00", "17:00:00", "21:00:00","24:00:00")),
             stance = c("avoid","awake", "business","awake","avoid"))
}


stamp_workday <- function(show.legend = T){  
  
  ggplot2::geom_rect(data = create_day_schedule_df(),
            ggplot2::aes(xmin = time_start,
                xmax = time_end, 
                x = NULL,
                fill = stance,
                ymin = -Inf,
                ymax = Inf,
                y = NULL,
                color = NULL),
           alpha = .5, show.legend = show.legend) 
}
```

``` r
readme2pkg::chunk_to_r(chunk_name = "stamp_workday")
```

## LocationXLocalTime

``` r
date_time_tz_to_tzs() |> 
  local_tzs_df_collapse() |>  
  ggplot() + 
  aes(local_time_hm %>% paste0(":00") %>%  hms::as_hms(), fct_rev(fct_inorder(str_wrap(locations,25)))) + 
  labs(x = "Local meet time", y = NULL) + 
  stamp_workday() +
  geom_point() + 
  geom_text(aes(label = local_time_hm),
            hjust = -.1, show.legend = F) + 
  geom_vline(xintercept = hms::as_hms("12:00:00"),
              linetype = "dashed", color = "grey25",
              alpha = .2) +
  aes(color = local_wday_date) +
  labs(color = "Local meet date") +
  theme(legend.position = "top", 
        legend.justification = "left") + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_line(color = ""))
#> `summarise()` has grouped output by 'local_date', 'local_date_time_utc',
#> 'local_time_hm'. You can override using the `.groups` argument.
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

# Part II. Packaging and documentation 🚧

## Phase 1. Minimal working package

### Bit A. Created package archetecture, running `devtools::create(".")` in interactive session. ✅

``` r
devtools::create(".")
```

### Bit B. Added roxygen skeleton? 🚧

Use a roxygen skeleton for auto documentation and making sure proposed
functions are *exported*. Generally, early on, I don’t do much
(anything) in terms of filling in the skeleton for documentation,
because things may change.

### Bit C. Managed dependencies ? 🚧

Package dependencies managed, i.e. `depend::function()` in proposed
functions and declared in the DESCRIPTION

``` r
usethis::use_package("dplyr")
usethis::use_package( "hms")
usethis::use_package( "lubridate")
usethis::use_package( "purrr")
usethis::use_package( "tidyr")
```

### Bit D. Moved functions R folder? ✅

Use new {readme2pkg} function to do this from readme…

``` r
readme2pkg::chunk_to_r("date_time_tz_to_tzs")
readme2pkg::chunk_to_r("local_tzs_df_collapse")
readme2pkg::chunk_to_r("time_to_local")
```

### Bit E. Run `devtools::check()` and addressed errors. 🚧

``` r
devtools::check(pkg = ".")
```

### Bit F. Build package 🚧 ✅

``` r
devtools::install(upgrade = F)
```

### Bit G. Write traditional README that uses built package (also serves as a test of build. ✅

The goal of the {ggchalkboard} package is to make it easy to theme
ggplots like chalkboards

Install package with:

    remotes::install_github("EvaMaeRey/ggchalkboard")

Once functions are exported you can remove go to two colons, and when
things are are really finalized, then go without colons (and rearrange
your readme…)

``` r
library(gglobalclocks)  

gglobalclocks:::date_time_tz_to_tzs("2024-03-06 11:00:00", 
                                    from_tz = "US/Mountain",
                                    to_tz = c("US/Eastern",
                                              "US/Mountain",
                                              "US/Pacific",
                                              "Europe/Paris"))
```

### Bit H. Chosen a license? ✅

``` r
usethis::use_mit_license()
```

### Bit I. Add lifecycle badge (experimental) ✅

``` r
usethis::use_lifecycle_badge("experimental")
```

## Phase 2: Listen & iterate 🚧

Try to get feedback from experts on API, implementation, default
decisions. Is there already work that solves this problem?

## Phase 3: Let things settle

### Bit A. Settle on examples. Put them in the roxygen skeleton and readme. 🚧

### Bit B. Written formal tests of functions and save to test that folders 🚧

That would look like this…

``` r
library(testthat)

test_that("calc times 2 works", {
  expect_equal(times_two(4), 8)
  expect_equal(times_two(5), 10)
  
})
```

``` r
readme2pkg::chunk_to_tests_testthat("test_calc_times_two_works")
```

### Bit C. Added a description and author information in the DESCRIPTION file 🚧

### Bit D. Addressed *all* notes, warnings and errors. 🚧

## Phase 4. Promote to wider audience…

### Bit A. Package website built? ✅

``` r
usethis::use_pkgdown()
pkgdown::build_site()
```

### Bit B. Package website deployed? 🚧 ✅

## Phase 5: Harden/commit

### Submit to CRAN/RUniverse? 🚧

# Appendix: Reports, Environment

## Edit Description file

``` r
readLines("DESCRIPTION")
#>  [1] "Package: gglobalclocks"                                                            
#>  [2] "Title: Create a Wall of Clocks with the Local Times for Locations Around the World"
#>  [3] "Version: 0.0.0.9000"                                                               
#>  [4] "Authors@R: "                                                                       
#>  [5] "    person(given = \"Gina\","                                                      
#>  [6] "           family = \"Reynolds\","                                                 
#>  [7] "           role = c(\"aut\", \"cre\"),"                                            
#>  [8] "           email = \"first.last@example.com\","                                    
#>  [9] "           comment = c(ORCID = \"YOUR-ORCID-ID\"))"                                
#> [10] "Description: What the package does (one paragraph)."                               
#> [11] "License: MIT + file LICENSE"                                                       
#> [12] "Encoding: UTF-8"                                                                   
#> [13] "LazyData: true"                                                                    
#> [14] "Roxygen: list(markdown = TRUE)"                                                    
#> [15] "RoxygenNote: 7.1.1"                                                                
#> [16] "Imports: "                                                                         
#> [17] "    dplyr,"                                                                        
#> [18] "    hms,"                                                                          
#> [19] "    lubridate,"                                                                    
#> [20] "    purrr,"                                                                        
#> [21] "    tidyr"
```

## Environment

Here I just want to print the packages and the versions

``` r
all <- sessionInfo() |> print() |> capture.output()
all[11:20]
#>  [1] ""                                                                         
#>  [2] "attached base packages:"                                                  
#>  [3] "[1] stats     graphics  grDevices utils     datasets  methods   base     "
#>  [4] ""                                                                         
#>  [5] "other attached packages:"                                                 
#>  [6] " [1] lubridate_1.9.2      forcats_1.0.0        stringr_1.5.0       "      
#>  [7] " [4] dplyr_1.1.0          purrr_1.0.1          readr_2.1.4         "      
#>  [8] " [7] tidyr_1.3.0          tibble_3.2.1         ggplot2_3.4.4.9000  "      
#>  [9] "[10] tidyverse_2.0.0.9000"                                                
#> [10] ""
```

## `devtools::check()` report

``` r
devtools::check(pkg = ".")
```

What would gglobalclocks syntax look like using above approach as
jumping off point?
