Untitled
================
Vincent Tam
November 3, 2018

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts -------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(janitor)
library(dplyr)
library(purrr)
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
library(ggplot2)
conexp = data_frame(ID = list.files(path = "data")) %>%
  mutate(ID = as.character(ID)) 
participants <- function(filename) {read.csv(paste0("data", filename))} %>%
  fulldata = map_df(conexp$ID, participants) %>%
  tidydata = cbind(conexp, fulldata) %>%
  clean_names() %>%
  mutate(ID_arm = str_replace(ID_arm, ".csv", "")) %>%
  separate(ID_arm, into = c("arm", "ID"), sep = "") %>%
  gather(key = week, value = obs, week_1:week_8) %>%
  mutate(week = str_replace(week, "week_", "")) %>%
  mutate(week = as.numeric(week)) %>%
  ggplot2(data = tidydata, aes(x = week, y = obs, color = ID_arm)) + geom_point() + geom_line() + labs(title = "Control vs Experimental, Week 1-8", x = "Week", y = "Observations")
```
