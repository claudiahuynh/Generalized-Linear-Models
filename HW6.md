HW6
================
My An Huynh
2024-11-19

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(p8105.datasets)
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme
    ## 
    ## Attaching package: 'nlme'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## This is mgcv 1.9-1. For overview type 'help("mgcv-package")'.

``` r
library(SemiPar)
```

## Problem 1

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## using cached file: /Users/claudiahuynh/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 14:18:13.607687 (8.651)

    ## file min/max dates: 1869-01-01 / 2024-09-30

## Problem 2

``` r
homicide_df =
  read_csv("data/homicide-data.csv") |> 
  mutate(
    city_state = paste(city, state, sep = ", "),
    solved_bin = ifelse(disposition == "Closed by arrest", 1, 0),
    victim_age = as.numeric(victim_age)
  ) |>
  filter(
    !(city_state %in% c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL")),
    victim_race %in% c("White", "Black")
  ) |> 
  select(-city, -state)
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `victim_age = as.numeric(victim_age)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

Glm for Baltimore, MD

``` r
baltimore = 
  homicide_df |> 
  filter(
    city_state == "Baltimore, MD"
  ) 

baltimore_glm = 
  glm(solved_bin ~ victim_age + victim_race + victim_sex, data = baltimore, family = binomial()) |> 
  broom::tidy() |> 
  filter(
    term == "victim_sexMale"
  ) |> 
  mutate(
    odds_ratio = exp(estimate),
    conf_upper_95 = exp(estimate + 1.96 * std.error),
    conf_lower_95 = exp(estimate - 1.96 * std.error)
  )
```

Do this for every city

``` r
glm_function = function(df) {
  df = 
    glm(solved_bin ~ victim_age + victim_race + victim_sex, data = df, family = binomial()) |> 
    broom::tidy() |> 
    filter(
      term == "victim_sexMale"
    ) |> 
    mutate(
      odds_ratio = exp(estimate),
      conf_upper_95 = exp(estimate + 1.96 * std.error),
      conf_lower_95 = exp(estimate - 1.96 * std.error)
    )
}

homicide_glm = 
  homicide_df |> 
  select(city_state, solved_bin, victim_sex, victim_age, victim_race) |>
  nest(data = solved_bin:victim_race) |> 
  mutate(
    glm_results = map(data, glm_function)
  ) |> 
  unnest(glm_results) |> 
  select(city_state, odds_ratio, conf_lower_95, conf_upper_95)

homicide_plot = 
  homicide_glm |> 
  ggplot(aes(x = reorder(city_state, odds_ratio), y = odds_ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf_lower_95, ymax = conf_upper_95)) 

print(homicide_plot)
```

![](HW6_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Problem 3

``` r
birthweight_df = 
  read_csv("data/birthweight.csv") |> 
  mutate(
    babysex = as.factor(babysex),
    frace = as.factor(frace),
    malform = as.factor(malform),
    mrace = as.factor(mrace)
  )
```

    ## Rows: 4342 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (20): babysex, bhead, blength, bwt, delwt, fincome, frace, gaweeks, malf...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
str(birthweight_df)
```

    ## tibble [4,342 × 20] (S3: tbl_df/tbl/data.frame)
    ##  $ babysex : Factor w/ 2 levels "1","2": 2 1 2 1 2 1 2 2 1 1 ...
    ##  $ bhead   : num [1:4342] 34 34 36 34 34 33 33 33 36 33 ...
    ##  $ blength : num [1:4342] 51 48 50 52 52 52 46 49 52 50 ...
    ##  $ bwt     : num [1:4342] 3629 3062 3345 3062 3374 ...
    ##  $ delwt   : num [1:4342] 177 156 148 157 156 129 126 140 146 169 ...
    ##  $ fincome : num [1:4342] 35 65 85 55 5 55 96 5 85 75 ...
    ##  $ frace   : Factor w/ 5 levels "1","2","3","4",..: 1 2 1 1 1 1 2 1 1 2 ...
    ##  $ gaweeks : num [1:4342] 39.9 25.9 39.9 40 41.6 ...
    ##  $ malform : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ menarche: num [1:4342] 13 14 12 14 13 12 14 12 11 12 ...
    ##  $ mheight : num [1:4342] 63 65 64 64 66 66 72 62 61 64 ...
    ##  $ momage  : num [1:4342] 36 25 29 18 20 23 29 19 13 19 ...
    ##  $ mrace   : Factor w/ 4 levels "1","2","3","4": 1 2 1 1 1 1 2 1 1 2 ...
    ##  $ parity  : num [1:4342] 3 0 0 0 0 0 0 0 0 0 ...
    ##  $ pnumlbw : num [1:4342] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ pnumsga : num [1:4342] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ppbmi   : num [1:4342] 26.3 21.3 23.6 21.8 21 ...
    ##  $ ppwt    : num [1:4342] 148 128 137 127 130 115 105 119 105 145 ...
    ##  $ smoken  : num [1:4342] 0 0 1 10 1 0 0 0 0 4 ...
    ##  $ wtgain  : num [1:4342] 29 28 11 30 26 14 21 21 41 24 ...

``` r
linear_model = lm(bwt ~ bhead + blength + gaweeks + momage + ppbmi + ppwt + smoken + wtgain, data = birthweight_df)
```

``` r
bwt_plot = 
  birthweight_df |> 
  add_predictions(linear_model) |> 
  ggplot(aes(x = bhead, y = bwt)) +
  geom_point() +
  geom_line(aes(y = pred))

print(bwt_plot)
```

![](HW6_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
