hw5_prob3
================

``` r
library(tidyverse)
```

## Problem 3

``` r
homicide_raw = read_csv("data_hw5/homicide-data.csv")
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
summary(homicide_raw)
```

    ##      uid            reported_date       victim_last        victim_first      
    ##  Length:52179       Min.   : 20070101   Length:52179       Length:52179      
    ##  Class :character   1st Qu.: 20100318   Class :character   Class :character  
    ##  Mode  :character   Median : 20121216   Mode  :character   Mode  :character  
    ##                     Mean   : 20130899                                        
    ##                     3rd Qu.: 20150911                                        
    ##                     Max.   :201511105                                        
    ##                                                                              
    ##  victim_race         victim_age         victim_sex            city          
    ##  Length:52179       Length:52179       Length:52179       Length:52179      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##     state                lat             lon          disposition       
    ##  Length:52179       Min.   :25.73   Min.   :-122.51   Length:52179      
    ##  Class :character   1st Qu.:33.77   1st Qu.: -96.00   Class :character  
    ##  Mode  :character   Median :38.52   Median : -87.71   Mode  :character  
    ##                     Mean   :37.03   Mean   : -91.47                     
    ##                     3rd Qu.:40.03   3rd Qu.: -81.76                     
    ##                     Max.   :45.05   Max.   : -71.01                     
    ##                     NA's   :60      NA's   :60

Comments on the raw data:  
The raw data contains 12 columns and 52,179 entries. Column variables
are uid, reported_data, victim_last (all caped), victim_first (all
caped), victim_race, victim_age, victim_sex, city, state, lat, lon, and
disposition.

Create a `city_state` variable.

``` r
homicide_df = 
  homicide_raw |>
  mutate(
    city_state = paste(city, state, sep = ", ")
  )
```

Summarize within cities to obtain the total number of homicides and the
number of unsolved homicides (those for which the disposition is “Closed
without arrest” or “Open/No arrest”).

``` r
homicide_summary  = 
  homicide_df |> 
  group_by(city_state) |> 
  summarise(
    total = n(),
    unsolved = sum(disposition %in% c("Closed without arrest","Open/No arrest"))
  )
```

For the city of Baltimore, MD, use the `prop.test` function to estimate
the proportion of homicides that are unsolved; save the output of
prop.test as an R object, apply the broom::tidy to this object and pull
the estimated proportion and confidence intervals from the resulting
tidy dataframe.

``` r
baltimore_summary = 
  homicide_df |> 
  filter(city_state == "Baltimore, MD") |> 
  summarise(
    total = n(),
    unsolved = sum(disposition %in% c("Closed without arrest","Open/No arrest"))
  )
```

``` r
baltimore_prop = 
  prop.test(
    x = baltimore_summary$unsolved,
    n = baltimore_summary$total
  )
baltimore_prop_tidy  = 
  broom::tidy(baltimore_prop) |> 
  select(estimate, conf.low, conf.high)
baltimore_prop_tidy
```

    ## # A tibble: 1 × 3
    ##   estimate conf.low conf.high
    ##      <dbl>    <dbl>     <dbl>
    ## 1    0.646    0.628     0.663
