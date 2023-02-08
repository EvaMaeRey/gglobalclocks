
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gglobalclocks

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
#> ✓ tibble  3.1.6     ✓ dplyr   1.0.8
#> ✓ tidyr   1.0.2     ✓ stringr 1.4.0
#> ✓ readr   1.3.1     ✓ forcats 0.5.0
#> Warning: package 'ggplot2' was built under R version 3.6.2
#> Warning: package 'tibble' was built under R version 3.6.2
#> Warning: package 'purrr' was built under R version 3.6.2
#> Warning: package 'dplyr' was built under R version 3.6.2
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(lubridate)
#> Warning: package 'lubridate' was built under R version 3.6.2
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union

ymd_hms("2009-08-07 11:00:00", 
        tz = "America/New_York") ->
  meeting

time_to_local <- function(x, tz){
  lubridate::with_tz(x, tz = tz) %>%
    as.character()
}


OlsonNames() %>%
  tibble(tz = .) %>%
  filter(tz != "US/Pacific-New") %>%
  filter(tz %>% str_detect("Amsterdam|Stockholm|US/Eastern|British|Europe/London|US/Pacific|Mountain|US/Central")) %>%
  mutate(meeting_ny = meeting) %>%
  mutate(local_time = purrr::map2(meeting_ny, tz, time_to_local))  %>%
  unnest(local_time) %>%
  group_by(local_time) %>%
  summarise(locations = paste(tz, collapse = "\n")) ->
df

# df %>% 
#   ggplot() + 
#   facet_wrap(~locations) +
#   aes(angle = local_time %>% lubridate::hour() %% 12, 
#       radius = 1, x = 0, y = 0) +
#   geom_spoke(radius = .8) + 
#   geom_spoke(aes(angle = local_time %>% lubridate::minute() %% 60)) 
  

  
df %>% 
  mutate(minute_turn = local_time %>% lubridate::minute() %% 60 / 60) %>% 
  mutate(hour_turn = local_time %>% lubridate::hour() %% 12/12 + minute_turn/12) %>% 
  mutate(pm = local_time %>% lubridate::hour() > 12) %>% 
  ggplot() + 
  facet_wrap(~locations) + 
  aes(x =  hour_turn, 
      xend =  hour_turn,
      y = 0,
      yend = .6,
      color = pm) + 
  geom_segment() + 
  geom_segment(aes(x =  minute_turn, 
                  xend =  minute_turn ,
               yend = 1) ) + 
  coord_polar() + 
  scale_x_continuous(limits = c(0,1)) + 
  geom_text(data = tibble(x = 1:12, y = 1), 
            aes(label = x, x = x/12, y = y, xend = NULL, yend = NULL, color = NULL),
            show.legend = F) + 
  theme_void() + 
  annotate(geom = "segment", x = 0, xend = 1, y = 1.2, yend = 1.2)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

<!-- badges: start -->

<!-- badges: end -->

The goal of gglobalclocks is to …

## Installation

You can install the released version of gglobalclocks from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("gglobalclocks")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EvaMaeRey/gglobalclocks")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(gglobalclocks)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
