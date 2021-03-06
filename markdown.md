Homework 5
================
Vincent Tam
November 3, 2018

    ## # A tibble: 20 x 9
    ##    armid week1  week2 week3 week4 week5 week6 week7 week8
    ##    <chr> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1 con01  0.2  -1.31   0.66  1.96  0.23  1.09  0.05  1.94
    ##  2 con02  1.13 -0.88   1.07  0.17 -0.83 -0.31  1.58  0.44
    ##  3 con03  1.77  3.11   2.22  3.26  3.31  0.89  1.88  1.01
    ##  4 con04  1.04  3.66   1.22  2.33  1.47  2.7   1.87  1.66
    ##  5 con05  0.47 -0.580 -0.09 -1.37 -0.32 -2.17  0.45  0.48
    ##  6 con06  2.37  2.5    1.59 -0.16  2.08  3.07  0.78  2.35
    ##  7 con07  0.03  1.21   1.13  0.64  0.49 -0.12 -0.07  0.46
    ##  8 con08 -0.08  1.42   0.09  0.36  1.18 -1.16  0.33 -0.44
    ##  9 con09  0.08  1.24   1.44  0.41  0.95  2.75  0.3   0.03
    ## 10 con10  2.14  1.15   2.52  3.44  4.26  0.97  2.73 -0.53
    ## 11 exp01  3.05  3.67   4.84  5.8   6.33  5.46  6.38  5.91
    ## 12 exp02 -0.84  2.63   1.64  2.58  1.24  2.32  3.11  3.78
    ## 13 exp03  2.15  2.08   1.82  2.84  3.36  3.61  3.37  3.74
    ## 14 exp04 -0.62  2.54   3.78  2.73  4.49  5.82  6     6.49
    ## 15 exp05  0.7   3.33   5.34  5.57  6.9   6.66  6.24  6.95
    ## 16 exp06  3.73  4.08   5.4   6.41  4.87  6.09  7.66  5.83
    ## 17 exp07  1.18  2.35   1.23  1.17  2.02  1.61  3.13  4.88
    ## 18 exp08  1.37  1.43   1.84  3.6   3.8   4.72  4.68  5.7 
    ## 19 exp09 -0.4   1.08   2.66  2.7   2.8   2.64  3.51  3.27
    ## 20 exp10  1.09  2.8    2.8   4.3   2.25  6.57  6.09  4.64

![](markdown_files/figure-markdown_github/Problem%201,%20setup-1.png) It appears that there is a difference between the two Arms in the study. The Experimental Arm has higher levels of observations than the Experimental Arm.

``` r
library(dplyr)
library(janitor)
library(purrr)
library(broom)
library(reshape)
```

    ## 
    ## Attaching package: 'reshape'

    ## The following object is masked from 'package:data.table':
    ## 
    ##     melt

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     rename

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, smiths

``` r
library(knitr)
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following objects are masked from 'package:reshape':
    ## 
    ##     colsplit, melt, recast

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     dcast, melt

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(tidyverse)
library(ggplot2)
library(stringr)
hom_data <- read.csv(url("https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv"))
hom_data = 
  hom_data %>%
  mutate(city_state = str_c(city, ",", state))
dispo = 
  hom_data %>%
  select(city_state, disposition) %>%
  group_by(city_state) %>%
  summarize(total = n())
solved =
  hom_data %>%
  filter(disposition == "Closed by arrest") %>%
  group_by(city_state) %>%
  summarize(total_solved = n())
unsolved =
  hom_data %>%
  filter(disposition == "Closed without arrest" | disposition == "Open/No arrest") %>%
  group_by(city_state) %>%
  summarize(total_unsolved = n())
## prop test for Baltimore, MD
baltimore_total =
  dispo %>%
  filter(city_state == "Baltimore,MD") 
baltimore_solved =
  solved %>%
  filter(city_state == "Baltimore,MD")
baltimore_unsolved = 
  unsolved %>%
  filter(city_state == "Baltimore,MD") 
baltimore_merged = 
  merge(baltimore_unsolved, baltimore_total)
baltimore_proptest = 
  prop.test(baltimore_merged$total_unsolved, baltimore_merged$total) %>%
  tidy() %>%
  select(conf.low, conf.high, estimate) %>%
  kable(digits = 3)
baltimore_proptest
```

|  conf.low|  conf.high|  estimate|
|---------:|----------:|---------:|
|     0.628|      0.663|     0.646|

``` r
## prop.test = unsolved/total
partial_table = 
  merge(dispo, unsolved, by = "city_state", na.rm = TRUE, all = T) %>%
  filter(city_state != "Tulsa,AL") %>%
  unnest()
partial_table = as.data.frame(partial_table)
allcities_proptest = 
  partial_table %>%
  mutate(homicide_CI = map2(.x = total_unsolved, .y = total, ~prop.test(x = .x, n = .y)$conf.int)) %>%
  mutate(homicide_estimate = map2(.x = total_unsolved, .y = total, ~prop.test(x = .x, n = .y)$estimate)) 
## plot_cities = 
##  allcities_proptest %>%
##  ggplot(allcities_proptest, aes(x = city_state, y = homicide_estimate)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
##  labs(
##    title = "Estimates and CIs for All Cities",
##    x = "City",
##    y = "Estimates"
##  )  +
##  theme(axis.text.x = element_text(angle = 90)) 
## plot_cities
```

The raw data shows 15 columns and over 52,000 rows of entries. The raw data contains public data of homicides including names, city of death, age, sex, etc.
