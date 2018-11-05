Untitled
================
Vincent Tam
November 3, 2018

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts -------------------------------- tidyverse_conflicts() --
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
LF = list.files(pattern="csv$")
rbindlist(lapply(setNames(LF, LF), fread), idcol = "data")
```

    ## Null data.table (0 rows and 0 cols)

``` r
conexp = map(list.files(path = "data", full.names = TRUE), read.csv) %>%
   reduce(rbind)
conexp
```

    ##    week_1 week_2 week_3 week_4 week_5 week_6 week_7 week_8
    ## 1    0.20  -1.31   0.66   1.96   0.23   1.09   0.05   1.94
    ## 2    1.13  -0.88   1.07   0.17  -0.83  -0.31   1.58   0.44
    ## 3    1.77   3.11   2.22   3.26   3.31   0.89   1.88   1.01
    ## 4    1.04   3.66   1.22   2.33   1.47   2.70   1.87   1.66
    ## 5    0.47  -0.58  -0.09  -1.37  -0.32  -2.17   0.45   0.48
    ## 6    2.37   2.50   1.59  -0.16   2.08   3.07   0.78   2.35
    ## 7    0.03   1.21   1.13   0.64   0.49  -0.12  -0.07   0.46
    ## 8   -0.08   1.42   0.09   0.36   1.18  -1.16   0.33  -0.44
    ## 9    0.08   1.24   1.44   0.41   0.95   2.75   0.30   0.03
    ## 10   2.14   1.15   2.52   3.44   4.26   0.97   2.73  -0.53
    ## 11   3.05   3.67   4.84   5.80   6.33   5.46   6.38   5.91
    ## 12  -0.84   2.63   1.64   2.58   1.24   2.32   3.11   3.78
    ## 13   2.15   2.08   1.82   2.84   3.36   3.61   3.37   3.74
    ## 14  -0.62   2.54   3.78   2.73   4.49   5.82   6.00   6.49
    ## 15   0.70   3.33   5.34   5.57   6.90   6.66   6.24   6.95
    ## 16   3.73   4.08   5.40   6.41   4.87   6.09   7.66   5.83
    ## 17   1.18   2.35   1.23   1.17   2.02   1.61   3.13   4.88
    ## 18   1.37   1.43   1.84   3.60   3.80   4.72   4.68   5.70
    ## 19  -0.40   1.08   2.66   2.70   2.80   2.64   3.51   3.27
    ## 20   1.09   2.80   2.80   4.30   2.25   6.57   6.09   4.64
