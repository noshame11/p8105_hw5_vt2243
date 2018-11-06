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

![](markdown_files/figure-markdown_github/Problem%201,%20setup-1.png)

``` r
library(dplyr)
library(janitor)
library(purrr)
X <- read.csv(url("https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv"))
X = 
  X %>%
  unite(city_state, city, state)
## summarize within cities to obtain total number of homicides and unsolved homicides
## purrr to subset using nest
## using MAP to loop in total homicides and unsolved homicides within each unique city/state
dispo = 
  X %>%
  select(city_state, disposition) 
homicides =
  dispo %>%
  filter(disposition == "Closed by arrest") %>%
  group_by(city_state) %>%
  summarize(total_solved = n())
homicides
```

    ## # A tibble: 51 x 2
    ##    city_state     total_solved
    ##    <chr>                 <int>
    ##  1 Albuquerque_NM          232
    ##  2 Atlanta_GA              600
    ##  3 Baltimore_MD           1002
    ##  4 Baton Rouge_LA          228
    ##  5 Birmingham_AL           453
    ##  6 Boston_MA               304
    ##  7 Buffalo_NY              202
    ##  8 Charlotte_NC            481
    ##  9 Chicago_IL             1462
    ## 10 Cincinnati_OH           385
    ## # ... with 41 more rows

``` r
unsolved =
  dispo %>%
  filter(disposition == "Closed without arrest" | disposition == "Open/No arrest") %>%
  group_by(city_state) %>%
  summarize(total_unsolved = n())
unsolved
```

    ## # A tibble: 50 x 2
    ##    city_state     total_unsolved
    ##    <chr>                   <int>
    ##  1 Albuquerque_NM            146
    ##  2 Atlanta_GA                373
    ##  3 Baltimore_MD             1825
    ##  4 Baton Rouge_LA            196
    ##  5 Birmingham_AL             347
    ##  6 Boston_MA                 310
    ##  7 Buffalo_NY                319
    ##  8 Charlotte_NC              206
    ##  9 Chicago_IL               4073
    ## 10 Cincinnati_OH             309
    ## # ... with 40 more rows

``` r
## prop test for MD
baltimore_total =
  dispo %>%
  filter(city_state == "Baltimore_MD") 
total = count(baltimore_total)
total
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1  2827

``` r
baltimore_solved =
  homicides %>%
  filter(city_state == "Baltimore_MD")
baltimore_solved
```

    ## # A tibble: 1 x 2
    ##   city_state   total_solved
    ##   <chr>               <int>
    ## 1 Baltimore_MD         1002

``` r
baltimore_unsolved = 
  unsolved %>%
  filter(city_state == "Baltimore_MD") 
baltimore_unsolved
```

    ## # A tibble: 1 x 2
    ##   city_state   total_unsolved
    ##   <chr>                 <int>
    ## 1 Baltimore_MD           1825

``` r
baltimore_homicides = 
  merge(baltimore_unsolved, total)
baltimore_homicides
```

    ##     city_state total_unsolved    n
    ## 1 Baltimore_MD           1825 2827

``` r
length(total)
```

    ## [1] 1

``` r
length(baltimore_unsolved)
```

    ## [1] 2

``` r
baltimore_unsolved
```

    ## # A tibble: 1 x 2
    ##   city_state   total_unsolved
    ##   <chr>                 <int>
    ## 1 Baltimore_MD           1825

``` r
total
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1  2827

``` r
prop.test(baltimore_homicides$total_unsolved, baltimore_homicides$n)
```

    ## 
    ##  1-sample proportions test with continuity correction
    ## 
    ## data:  baltimore_homicides$total_unsolved out of baltimore_homicides$n, null probability 0.5
    ## X-squared = 239.01, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: true p is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.6275625 0.6631599
    ## sample estimates:
    ##         p 
    ## 0.6455607

``` r
baltimore_total
```

    ##        city_state           disposition
    ## 1    Baltimore_MD        Open/No arrest
    ## 2    Baltimore_MD        Open/No arrest
    ## 3    Baltimore_MD        Open/No arrest
    ## 4    Baltimore_MD      Closed by arrest
    ## 5    Baltimore_MD      Closed by arrest
    ## 6    Baltimore_MD      Closed by arrest
    ## 7    Baltimore_MD      Closed by arrest
    ## 8    Baltimore_MD      Closed by arrest
    ## 9    Baltimore_MD      Closed by arrest
    ## 10   Baltimore_MD      Closed by arrest
    ## 11   Baltimore_MD      Closed by arrest
    ## 12   Baltimore_MD      Closed by arrest
    ## 13   Baltimore_MD      Closed by arrest
    ## 14   Baltimore_MD        Open/No arrest
    ## 15   Baltimore_MD        Open/No arrest
    ## 16   Baltimore_MD        Open/No arrest
    ## 17   Baltimore_MD      Closed by arrest
    ## 18   Baltimore_MD Closed without arrest
    ## 19   Baltimore_MD      Closed by arrest
    ## 20   Baltimore_MD        Open/No arrest
    ## 21   Baltimore_MD        Open/No arrest
    ## 22   Baltimore_MD Closed without arrest
    ## 23   Baltimore_MD        Open/No arrest
    ## 24   Baltimore_MD        Open/No arrest
    ## 25   Baltimore_MD      Closed by arrest
    ## 26   Baltimore_MD        Open/No arrest
    ## 27   Baltimore_MD      Closed by arrest
    ## 28   Baltimore_MD      Closed by arrest
    ## 29   Baltimore_MD        Open/No arrest
    ## 30   Baltimore_MD        Open/No arrest
    ## 31   Baltimore_MD        Open/No arrest
    ## 32   Baltimore_MD        Open/No arrest
    ## 33   Baltimore_MD      Closed by arrest
    ## 34   Baltimore_MD      Closed by arrest
    ## 35   Baltimore_MD        Open/No arrest
    ## 36   Baltimore_MD Closed without arrest
    ## 37   Baltimore_MD        Open/No arrest
    ## 38   Baltimore_MD        Open/No arrest
    ## 39   Baltimore_MD        Open/No arrest
    ## 40   Baltimore_MD      Closed by arrest
    ## 41   Baltimore_MD      Closed by arrest
    ## 42   Baltimore_MD        Open/No arrest
    ## 43   Baltimore_MD        Open/No arrest
    ## 44   Baltimore_MD      Closed by arrest
    ## 45   Baltimore_MD        Open/No arrest
    ## 46   Baltimore_MD        Open/No arrest
    ## 47   Baltimore_MD      Closed by arrest
    ## 48   Baltimore_MD        Open/No arrest
    ## 49   Baltimore_MD        Open/No arrest
    ## 50   Baltimore_MD      Closed by arrest
    ## 51   Baltimore_MD      Closed by arrest
    ## 52   Baltimore_MD        Open/No arrest
    ## 53   Baltimore_MD      Closed by arrest
    ## 54   Baltimore_MD        Open/No arrest
    ## 55   Baltimore_MD      Closed by arrest
    ## 56   Baltimore_MD      Closed by arrest
    ## 57   Baltimore_MD        Open/No arrest
    ## 58   Baltimore_MD      Closed by arrest
    ## 59   Baltimore_MD      Closed by arrest
    ## 60   Baltimore_MD      Closed by arrest
    ## 61   Baltimore_MD        Open/No arrest
    ## 62   Baltimore_MD      Closed by arrest
    ## 63   Baltimore_MD      Closed by arrest
    ## 64   Baltimore_MD        Open/No arrest
    ## 65   Baltimore_MD      Closed by arrest
    ## 66   Baltimore_MD      Closed by arrest
    ## 67   Baltimore_MD      Closed by arrest
    ## 68   Baltimore_MD        Open/No arrest
    ## 69   Baltimore_MD      Closed by arrest
    ## 70   Baltimore_MD      Closed by arrest
    ## 71   Baltimore_MD      Closed by arrest
    ## 72   Baltimore_MD      Closed by arrest
    ## 73   Baltimore_MD        Open/No arrest
    ## 74   Baltimore_MD        Open/No arrest
    ## 75   Baltimore_MD Closed without arrest
    ## 76   Baltimore_MD      Closed by arrest
    ## 77   Baltimore_MD      Closed by arrest
    ## 78   Baltimore_MD        Open/No arrest
    ## 79   Baltimore_MD      Closed by arrest
    ## 80   Baltimore_MD        Open/No arrest
    ## 81   Baltimore_MD        Open/No arrest
    ## 82   Baltimore_MD      Closed by arrest
    ## 83   Baltimore_MD      Closed by arrest
    ## 84   Baltimore_MD        Open/No arrest
    ## 85   Baltimore_MD        Open/No arrest
    ## 86   Baltimore_MD      Closed by arrest
    ## 87   Baltimore_MD        Open/No arrest
    ## 88   Baltimore_MD        Open/No arrest
    ## 89   Baltimore_MD        Open/No arrest
    ## 90   Baltimore_MD      Closed by arrest
    ## 91   Baltimore_MD Closed without arrest
    ## 92   Baltimore_MD      Closed by arrest
    ## 93   Baltimore_MD        Open/No arrest
    ## 94   Baltimore_MD      Closed by arrest
    ## 95   Baltimore_MD      Closed by arrest
    ## 96   Baltimore_MD      Closed by arrest
    ## 97   Baltimore_MD        Open/No arrest
    ## 98   Baltimore_MD        Open/No arrest
    ## 99   Baltimore_MD        Open/No arrest
    ## 100  Baltimore_MD      Closed by arrest
    ## 101  Baltimore_MD Closed without arrest
    ## 102  Baltimore_MD        Open/No arrest
    ## 103  Baltimore_MD        Open/No arrest
    ## 104  Baltimore_MD      Closed by arrest
    ## 105  Baltimore_MD      Closed by arrest
    ## 106  Baltimore_MD      Closed by arrest
    ## 107  Baltimore_MD      Closed by arrest
    ## 108  Baltimore_MD      Closed by arrest
    ## 109  Baltimore_MD      Closed by arrest
    ## 110  Baltimore_MD        Open/No arrest
    ## 111  Baltimore_MD        Open/No arrest
    ## 112  Baltimore_MD        Open/No arrest
    ## 113  Baltimore_MD      Closed by arrest
    ## 114  Baltimore_MD        Open/No arrest
    ## 115  Baltimore_MD        Open/No arrest
    ## 116  Baltimore_MD        Open/No arrest
    ## 117  Baltimore_MD      Closed by arrest
    ## 118  Baltimore_MD        Open/No arrest
    ## 119  Baltimore_MD        Open/No arrest
    ## 120  Baltimore_MD      Closed by arrest
    ## 121  Baltimore_MD      Closed by arrest
    ## 122  Baltimore_MD        Open/No arrest
    ## 123  Baltimore_MD        Open/No arrest
    ## 124  Baltimore_MD        Open/No arrest
    ## 125  Baltimore_MD Closed without arrest
    ## 126  Baltimore_MD      Closed by arrest
    ## 127  Baltimore_MD      Closed by arrest
    ## 128  Baltimore_MD        Open/No arrest
    ## 129  Baltimore_MD      Closed by arrest
    ## 130  Baltimore_MD        Open/No arrest
    ## 131  Baltimore_MD      Closed by arrest
    ## 132  Baltimore_MD      Closed by arrest
    ## 133  Baltimore_MD        Open/No arrest
    ## 134  Baltimore_MD        Open/No arrest
    ## 135  Baltimore_MD        Open/No arrest
    ## 136  Baltimore_MD      Closed by arrest
    ## 137  Baltimore_MD      Closed by arrest
    ## 138  Baltimore_MD        Open/No arrest
    ## 139  Baltimore_MD        Open/No arrest
    ## 140  Baltimore_MD        Open/No arrest
    ## 141  Baltimore_MD        Open/No arrest
    ## 142  Baltimore_MD        Open/No arrest
    ## 143  Baltimore_MD Closed without arrest
    ## 144  Baltimore_MD      Closed by arrest
    ## 145  Baltimore_MD      Closed by arrest
    ## 146  Baltimore_MD        Open/No arrest
    ## 147  Baltimore_MD        Open/No arrest
    ## 148  Baltimore_MD      Closed by arrest
    ## 149  Baltimore_MD Closed without arrest
    ## 150  Baltimore_MD      Closed by arrest
    ## 151  Baltimore_MD        Open/No arrest
    ## 152  Baltimore_MD      Closed by arrest
    ## 153  Baltimore_MD      Closed by arrest
    ## 154  Baltimore_MD      Closed by arrest
    ## 155  Baltimore_MD        Open/No arrest
    ## 156  Baltimore_MD        Open/No arrest
    ## 157  Baltimore_MD        Open/No arrest
    ## 158  Baltimore_MD        Open/No arrest
    ## 159  Baltimore_MD      Closed by arrest
    ## 160  Baltimore_MD        Open/No arrest
    ## 161  Baltimore_MD      Closed by arrest
    ## 162  Baltimore_MD        Open/No arrest
    ## 163  Baltimore_MD      Closed by arrest
    ## 164  Baltimore_MD        Open/No arrest
    ## 165  Baltimore_MD        Open/No arrest
    ## 166  Baltimore_MD        Open/No arrest
    ## 167  Baltimore_MD      Closed by arrest
    ## 168  Baltimore_MD        Open/No arrest
    ## 169  Baltimore_MD        Open/No arrest
    ## 170  Baltimore_MD      Closed by arrest
    ## 171  Baltimore_MD      Closed by arrest
    ## 172  Baltimore_MD      Closed by arrest
    ## 173  Baltimore_MD        Open/No arrest
    ## 174  Baltimore_MD Closed without arrest
    ## 175  Baltimore_MD Closed without arrest
    ## 176  Baltimore_MD        Open/No arrest
    ## 177  Baltimore_MD        Open/No arrest
    ## 178  Baltimore_MD        Open/No arrest
    ## 179  Baltimore_MD        Open/No arrest
    ## 180  Baltimore_MD Closed without arrest
    ## 181  Baltimore_MD        Open/No arrest
    ## 182  Baltimore_MD        Open/No arrest
    ## 183  Baltimore_MD      Closed by arrest
    ## 184  Baltimore_MD        Open/No arrest
    ## 185  Baltimore_MD      Closed by arrest
    ## 186  Baltimore_MD        Open/No arrest
    ## 187  Baltimore_MD      Closed by arrest
    ## 188  Baltimore_MD        Open/No arrest
    ## 189  Baltimore_MD        Open/No arrest
    ## 190  Baltimore_MD Closed without arrest
    ## 191  Baltimore_MD Closed without arrest
    ## 192  Baltimore_MD      Closed by arrest
    ## 193  Baltimore_MD      Closed by arrest
    ## 194  Baltimore_MD      Closed by arrest
    ## 195  Baltimore_MD      Closed by arrest
    ## 196  Baltimore_MD        Open/No arrest
    ## 197  Baltimore_MD        Open/No arrest
    ## 198  Baltimore_MD        Open/No arrest
    ## 199  Baltimore_MD      Closed by arrest
    ## 200  Baltimore_MD        Open/No arrest
    ## 201  Baltimore_MD        Open/No arrest
    ## 202  Baltimore_MD        Open/No arrest
    ## 203  Baltimore_MD        Open/No arrest
    ## 204  Baltimore_MD        Open/No arrest
    ## 205  Baltimore_MD      Closed by arrest
    ## 206  Baltimore_MD      Closed by arrest
    ## 207  Baltimore_MD Closed without arrest
    ## 208  Baltimore_MD        Open/No arrest
    ## 209  Baltimore_MD      Closed by arrest
    ## 210  Baltimore_MD        Open/No arrest
    ## 211  Baltimore_MD        Open/No arrest
    ## 212  Baltimore_MD      Closed by arrest
    ## 213  Baltimore_MD      Closed by arrest
    ## 214  Baltimore_MD Closed without arrest
    ## 215  Baltimore_MD      Closed by arrest
    ## 216  Baltimore_MD        Open/No arrest
    ## 217  Baltimore_MD        Open/No arrest
    ## 218  Baltimore_MD        Open/No arrest
    ## 219  Baltimore_MD      Closed by arrest
    ## 220  Baltimore_MD      Closed by arrest
    ## 221  Baltimore_MD        Open/No arrest
    ## 222  Baltimore_MD        Open/No arrest
    ## 223  Baltimore_MD        Open/No arrest
    ## 224  Baltimore_MD        Open/No arrest
    ## 225  Baltimore_MD        Open/No arrest
    ## 226  Baltimore_MD      Closed by arrest
    ## 227  Baltimore_MD Closed without arrest
    ## 228  Baltimore_MD      Closed by arrest
    ## 229  Baltimore_MD      Closed by arrest
    ## 230  Baltimore_MD        Open/No arrest
    ## 231  Baltimore_MD        Open/No arrest
    ## 232  Baltimore_MD        Open/No arrest
    ## 233  Baltimore_MD        Open/No arrest
    ## 234  Baltimore_MD      Closed by arrest
    ## 235  Baltimore_MD      Closed by arrest
    ## 236  Baltimore_MD        Open/No arrest
    ## 237  Baltimore_MD      Closed by arrest
    ## 238  Baltimore_MD        Open/No arrest
    ## 239  Baltimore_MD        Open/No arrest
    ## 240  Baltimore_MD        Open/No arrest
    ## 241  Baltimore_MD      Closed by arrest
    ## 242  Baltimore_MD        Open/No arrest
    ## 243  Baltimore_MD        Open/No arrest
    ## 244  Baltimore_MD        Open/No arrest
    ## 245  Baltimore_MD Closed without arrest
    ## 246  Baltimore_MD      Closed by arrest
    ## 247  Baltimore_MD        Open/No arrest
    ## 248  Baltimore_MD Closed without arrest
    ## 249  Baltimore_MD        Open/No arrest
    ## 250  Baltimore_MD      Closed by arrest
    ## 251  Baltimore_MD        Open/No arrest
    ## 252  Baltimore_MD      Closed by arrest
    ## 253  Baltimore_MD      Closed by arrest
    ## 254  Baltimore_MD      Closed by arrest
    ## 255  Baltimore_MD Closed without arrest
    ## 256  Baltimore_MD        Open/No arrest
    ## 257  Baltimore_MD        Open/No arrest
    ## 258  Baltimore_MD Closed without arrest
    ## 259  Baltimore_MD      Closed by arrest
    ## 260  Baltimore_MD        Open/No arrest
    ## 261  Baltimore_MD      Closed by arrest
    ## 262  Baltimore_MD Closed without arrest
    ## 263  Baltimore_MD        Open/No arrest
    ## 264  Baltimore_MD        Open/No arrest
    ## 265  Baltimore_MD      Closed by arrest
    ## 266  Baltimore_MD Closed without arrest
    ## 267  Baltimore_MD      Closed by arrest
    ## 268  Baltimore_MD Closed without arrest
    ## 269  Baltimore_MD        Open/No arrest
    ## 270  Baltimore_MD      Closed by arrest
    ## 271  Baltimore_MD        Open/No arrest
    ## 272  Baltimore_MD Closed without arrest
    ## 273  Baltimore_MD      Closed by arrest
    ## 274  Baltimore_MD      Closed by arrest
    ## 275  Baltimore_MD        Open/No arrest
    ## 276  Baltimore_MD        Open/No arrest
    ## 277  Baltimore_MD        Open/No arrest
    ## 278  Baltimore_MD        Open/No arrest
    ## 279  Baltimore_MD        Open/No arrest
    ## 280  Baltimore_MD      Closed by arrest
    ## 281  Baltimore_MD      Closed by arrest
    ## 282  Baltimore_MD        Open/No arrest
    ## 283  Baltimore_MD        Open/No arrest
    ## 284  Baltimore_MD        Open/No arrest
    ## 285  Baltimore_MD      Closed by arrest
    ## 286  Baltimore_MD        Open/No arrest
    ## 287  Baltimore_MD        Open/No arrest
    ## 288  Baltimore_MD      Closed by arrest
    ## 289  Baltimore_MD      Closed by arrest
    ## 290  Baltimore_MD      Closed by arrest
    ## 291  Baltimore_MD      Closed by arrest
    ## 292  Baltimore_MD        Open/No arrest
    ## 293  Baltimore_MD      Closed by arrest
    ## 294  Baltimore_MD        Open/No arrest
    ## 295  Baltimore_MD        Open/No arrest
    ## 296  Baltimore_MD      Closed by arrest
    ## 297  Baltimore_MD      Closed by arrest
    ## 298  Baltimore_MD      Closed by arrest
    ## 299  Baltimore_MD Closed without arrest
    ## 300  Baltimore_MD        Open/No arrest
    ## 301  Baltimore_MD        Open/No arrest
    ## 302  Baltimore_MD        Open/No arrest
    ## 303  Baltimore_MD        Open/No arrest
    ## 304  Baltimore_MD        Open/No arrest
    ## 305  Baltimore_MD      Closed by arrest
    ## 306  Baltimore_MD      Closed by arrest
    ## 307  Baltimore_MD        Open/No arrest
    ## 308  Baltimore_MD      Closed by arrest
    ## 309  Baltimore_MD        Open/No arrest
    ## 310  Baltimore_MD      Closed by arrest
    ## 311  Baltimore_MD Closed without arrest
    ## 312  Baltimore_MD        Open/No arrest
    ## 313  Baltimore_MD        Open/No arrest
    ## 314  Baltimore_MD      Closed by arrest
    ## 315  Baltimore_MD        Open/No arrest
    ## 316  Baltimore_MD        Open/No arrest
    ## 317  Baltimore_MD      Closed by arrest
    ## 318  Baltimore_MD Closed without arrest
    ## 319  Baltimore_MD        Open/No arrest
    ## 320  Baltimore_MD        Open/No arrest
    ## 321  Baltimore_MD      Closed by arrest
    ## 322  Baltimore_MD Closed without arrest
    ## 323  Baltimore_MD      Closed by arrest
    ## 324  Baltimore_MD      Closed by arrest
    ## 325  Baltimore_MD        Open/No arrest
    ## 326  Baltimore_MD        Open/No arrest
    ## 327  Baltimore_MD      Closed by arrest
    ## 328  Baltimore_MD      Closed by arrest
    ## 329  Baltimore_MD      Closed by arrest
    ## 330  Baltimore_MD        Open/No arrest
    ## 331  Baltimore_MD      Closed by arrest
    ## 332  Baltimore_MD        Open/No arrest
    ## 333  Baltimore_MD        Open/No arrest
    ## 334  Baltimore_MD        Open/No arrest
    ## 335  Baltimore_MD      Closed by arrest
    ## 336  Baltimore_MD        Open/No arrest
    ## 337  Baltimore_MD      Closed by arrest
    ## 338  Baltimore_MD      Closed by arrest
    ## 339  Baltimore_MD        Open/No arrest
    ## 340  Baltimore_MD        Open/No arrest
    ## 341  Baltimore_MD Closed without arrest
    ## 342  Baltimore_MD        Open/No arrest
    ## 343  Baltimore_MD      Closed by arrest
    ## 344  Baltimore_MD      Closed by arrest
    ## 345  Baltimore_MD      Closed by arrest
    ## 346  Baltimore_MD      Closed by arrest
    ## 347  Baltimore_MD        Open/No arrest
    ## 348  Baltimore_MD      Closed by arrest
    ## 349  Baltimore_MD        Open/No arrest
    ## 350  Baltimore_MD      Closed by arrest
    ## 351  Baltimore_MD      Closed by arrest
    ## 352  Baltimore_MD Closed without arrest
    ## 353  Baltimore_MD Closed without arrest
    ## 354  Baltimore_MD        Open/No arrest
    ## 355  Baltimore_MD      Closed by arrest
    ## 356  Baltimore_MD      Closed by arrest
    ## 357  Baltimore_MD        Open/No arrest
    ## 358  Baltimore_MD        Open/No arrest
    ## 359  Baltimore_MD        Open/No arrest
    ## 360  Baltimore_MD        Open/No arrest
    ## 361  Baltimore_MD      Closed by arrest
    ## 362  Baltimore_MD      Closed by arrest
    ## 363  Baltimore_MD        Open/No arrest
    ## 364  Baltimore_MD Closed without arrest
    ## 365  Baltimore_MD      Closed by arrest
    ## 366  Baltimore_MD      Closed by arrest
    ## 367  Baltimore_MD      Closed by arrest
    ## 368  Baltimore_MD      Closed by arrest
    ## 369  Baltimore_MD Closed without arrest
    ## 370  Baltimore_MD        Open/No arrest
    ## 371  Baltimore_MD        Open/No arrest
    ## 372  Baltimore_MD        Open/No arrest
    ## 373  Baltimore_MD        Open/No arrest
    ## 374  Baltimore_MD      Closed by arrest
    ## 375  Baltimore_MD        Open/No arrest
    ## 376  Baltimore_MD      Closed by arrest
    ## 377  Baltimore_MD      Closed by arrest
    ## 378  Baltimore_MD      Closed by arrest
    ## 379  Baltimore_MD        Open/No arrest
    ## 380  Baltimore_MD        Open/No arrest
    ## 381  Baltimore_MD        Open/No arrest
    ## 382  Baltimore_MD        Open/No arrest
    ## 383  Baltimore_MD      Closed by arrest
    ## 384  Baltimore_MD        Open/No arrest
    ## 385  Baltimore_MD      Closed by arrest
    ## 386  Baltimore_MD      Closed by arrest
    ## 387  Baltimore_MD        Open/No arrest
    ## 388  Baltimore_MD      Closed by arrest
    ## 389  Baltimore_MD      Closed by arrest
    ## 390  Baltimore_MD      Closed by arrest
    ## 391  Baltimore_MD        Open/No arrest
    ## 392  Baltimore_MD        Open/No arrest
    ## 393  Baltimore_MD        Open/No arrest
    ## 394  Baltimore_MD      Closed by arrest
    ## 395  Baltimore_MD        Open/No arrest
    ## 396  Baltimore_MD Closed without arrest
    ## 397  Baltimore_MD      Closed by arrest
    ## 398  Baltimore_MD        Open/No arrest
    ## 399  Baltimore_MD Closed without arrest
    ## 400  Baltimore_MD Closed without arrest
    ## 401  Baltimore_MD Closed without arrest
    ## 402  Baltimore_MD      Closed by arrest
    ## 403  Baltimore_MD      Closed by arrest
    ## 404  Baltimore_MD        Open/No arrest
    ## 405  Baltimore_MD      Closed by arrest
    ## 406  Baltimore_MD        Open/No arrest
    ## 407  Baltimore_MD        Open/No arrest
    ## 408  Baltimore_MD      Closed by arrest
    ## 409  Baltimore_MD      Closed by arrest
    ## 410  Baltimore_MD        Open/No arrest
    ## 411  Baltimore_MD        Open/No arrest
    ## 412  Baltimore_MD      Closed by arrest
    ## 413  Baltimore_MD        Open/No arrest
    ## 414  Baltimore_MD        Open/No arrest
    ## 415  Baltimore_MD        Open/No arrest
    ## 416  Baltimore_MD        Open/No arrest
    ## 417  Baltimore_MD        Open/No arrest
    ## 418  Baltimore_MD      Closed by arrest
    ## 419  Baltimore_MD      Closed by arrest
    ## 420  Baltimore_MD      Closed by arrest
    ## 421  Baltimore_MD        Open/No arrest
    ## 422  Baltimore_MD        Open/No arrest
    ## 423  Baltimore_MD        Open/No arrest
    ## 424  Baltimore_MD        Open/No arrest
    ## 425  Baltimore_MD        Open/No arrest
    ## 426  Baltimore_MD        Open/No arrest
    ## 427  Baltimore_MD      Closed by arrest
    ## 428  Baltimore_MD      Closed by arrest
    ## 429  Baltimore_MD      Closed by arrest
    ## 430  Baltimore_MD        Open/No arrest
    ## 431  Baltimore_MD        Open/No arrest
    ## 432  Baltimore_MD        Open/No arrest
    ## 433  Baltimore_MD Closed without arrest
    ## 434  Baltimore_MD      Closed by arrest
    ## 435  Baltimore_MD        Open/No arrest
    ## 436  Baltimore_MD        Open/No arrest
    ## 437  Baltimore_MD      Closed by arrest
    ## 438  Baltimore_MD      Closed by arrest
    ## 439  Baltimore_MD        Open/No arrest
    ## 440  Baltimore_MD        Open/No arrest
    ## 441  Baltimore_MD        Open/No arrest
    ## 442  Baltimore_MD      Closed by arrest
    ## 443  Baltimore_MD        Open/No arrest
    ## 444  Baltimore_MD        Open/No arrest
    ## 445  Baltimore_MD        Open/No arrest
    ## 446  Baltimore_MD        Open/No arrest
    ## 447  Baltimore_MD        Open/No arrest
    ## 448  Baltimore_MD      Closed by arrest
    ## 449  Baltimore_MD      Closed by arrest
    ## 450  Baltimore_MD      Closed by arrest
    ## 451  Baltimore_MD      Closed by arrest
    ## 452  Baltimore_MD        Open/No arrest
    ## 453  Baltimore_MD        Open/No arrest
    ## 454  Baltimore_MD Closed without arrest
    ## 455  Baltimore_MD        Open/No arrest
    ## 456  Baltimore_MD        Open/No arrest
    ## 457  Baltimore_MD Closed without arrest
    ## 458  Baltimore_MD Closed without arrest
    ## 459  Baltimore_MD        Open/No arrest
    ## 460  Baltimore_MD        Open/No arrest
    ## 461  Baltimore_MD        Open/No arrest
    ## 462  Baltimore_MD        Open/No arrest
    ## 463  Baltimore_MD      Closed by arrest
    ## 464  Baltimore_MD      Closed by arrest
    ## 465  Baltimore_MD        Open/No arrest
    ## 466  Baltimore_MD      Closed by arrest
    ## 467  Baltimore_MD        Open/No arrest
    ## 468  Baltimore_MD        Open/No arrest
    ## 469  Baltimore_MD Closed without arrest
    ## 470  Baltimore_MD      Closed by arrest
    ## 471  Baltimore_MD        Open/No arrest
    ## 472  Baltimore_MD      Closed by arrest
    ## 473  Baltimore_MD        Open/No arrest
    ## 474  Baltimore_MD        Open/No arrest
    ## 475  Baltimore_MD        Open/No arrest
    ## 476  Baltimore_MD        Open/No arrest
    ## 477  Baltimore_MD        Open/No arrest
    ## 478  Baltimore_MD      Closed by arrest
    ## 479  Baltimore_MD      Closed by arrest
    ## 480  Baltimore_MD      Closed by arrest
    ## 481  Baltimore_MD        Open/No arrest
    ## 482  Baltimore_MD      Closed by arrest
    ## 483  Baltimore_MD      Closed by arrest
    ## 484  Baltimore_MD      Closed by arrest
    ## 485  Baltimore_MD        Open/No arrest
    ## 486  Baltimore_MD        Open/No arrest
    ## 487  Baltimore_MD        Open/No arrest
    ## 488  Baltimore_MD        Open/No arrest
    ## 489  Baltimore_MD        Open/No arrest
    ## 490  Baltimore_MD        Open/No arrest
    ## 491  Baltimore_MD        Open/No arrest
    ## 492  Baltimore_MD      Closed by arrest
    ## 493  Baltimore_MD      Closed by arrest
    ## 494  Baltimore_MD        Open/No arrest
    ## 495  Baltimore_MD        Open/No arrest
    ## 496  Baltimore_MD        Open/No arrest
    ## 497  Baltimore_MD      Closed by arrest
    ## 498  Baltimore_MD        Open/No arrest
    ## 499  Baltimore_MD      Closed by arrest
    ## 500  Baltimore_MD      Closed by arrest
    ## 501  Baltimore_MD      Closed by arrest
    ## 502  Baltimore_MD        Open/No arrest
    ## 503  Baltimore_MD      Closed by arrest
    ## 504  Baltimore_MD        Open/No arrest
    ## 505  Baltimore_MD        Open/No arrest
    ## 506  Baltimore_MD      Closed by arrest
    ## 507  Baltimore_MD        Open/No arrest
    ## 508  Baltimore_MD        Open/No arrest
    ## 509  Baltimore_MD      Closed by arrest
    ## 510  Baltimore_MD        Open/No arrest
    ## 511  Baltimore_MD        Open/No arrest
    ## 512  Baltimore_MD        Open/No arrest
    ## 513  Baltimore_MD      Closed by arrest
    ## 514  Baltimore_MD      Closed by arrest
    ## 515  Baltimore_MD      Closed by arrest
    ## 516  Baltimore_MD        Open/No arrest
    ## 517  Baltimore_MD        Open/No arrest
    ## 518  Baltimore_MD        Open/No arrest
    ## 519  Baltimore_MD        Open/No arrest
    ## 520  Baltimore_MD        Open/No arrest
    ## 521  Baltimore_MD        Open/No arrest
    ## 522  Baltimore_MD        Open/No arrest
    ## 523  Baltimore_MD      Closed by arrest
    ## 524  Baltimore_MD      Closed by arrest
    ## 525  Baltimore_MD      Closed by arrest
    ## 526  Baltimore_MD      Closed by arrest
    ## 527  Baltimore_MD        Open/No arrest
    ## 528  Baltimore_MD        Open/No arrest
    ## 529  Baltimore_MD        Open/No arrest
    ## 530  Baltimore_MD      Closed by arrest
    ## 531  Baltimore_MD      Closed by arrest
    ## 532  Baltimore_MD      Closed by arrest
    ## 533  Baltimore_MD      Closed by arrest
    ## 534  Baltimore_MD        Open/No arrest
    ## 535  Baltimore_MD      Closed by arrest
    ## 536  Baltimore_MD      Closed by arrest
    ## 537  Baltimore_MD      Closed by arrest
    ## 538  Baltimore_MD      Closed by arrest
    ## 539  Baltimore_MD      Closed by arrest
    ## 540  Baltimore_MD      Closed by arrest
    ## 541  Baltimore_MD        Open/No arrest
    ## 542  Baltimore_MD        Open/No arrest
    ## 543  Baltimore_MD      Closed by arrest
    ## 544  Baltimore_MD      Closed by arrest
    ## 545  Baltimore_MD      Closed by arrest
    ## 546  Baltimore_MD        Open/No arrest
    ## 547  Baltimore_MD      Closed by arrest
    ## 548  Baltimore_MD      Closed by arrest
    ## 549  Baltimore_MD      Closed by arrest
    ## 550  Baltimore_MD        Open/No arrest
    ## 551  Baltimore_MD      Closed by arrest
    ## 552  Baltimore_MD Closed without arrest
    ## 553  Baltimore_MD      Closed by arrest
    ## 554  Baltimore_MD Closed without arrest
    ## 555  Baltimore_MD        Open/No arrest
    ## 556  Baltimore_MD      Closed by arrest
    ## 557  Baltimore_MD      Closed by arrest
    ## 558  Baltimore_MD      Closed by arrest
    ## 559  Baltimore_MD      Closed by arrest
    ## 560  Baltimore_MD        Open/No arrest
    ## 561  Baltimore_MD Closed without arrest
    ## 562  Baltimore_MD        Open/No arrest
    ## 563  Baltimore_MD        Open/No arrest
    ## 564  Baltimore_MD        Open/No arrest
    ## 565  Baltimore_MD        Open/No arrest
    ## 566  Baltimore_MD        Open/No arrest
    ## 567  Baltimore_MD        Open/No arrest
    ## 568  Baltimore_MD        Open/No arrest
    ## 569  Baltimore_MD      Closed by arrest
    ## 570  Baltimore_MD      Closed by arrest
    ## 571  Baltimore_MD        Open/No arrest
    ## 572  Baltimore_MD      Closed by arrest
    ## 573  Baltimore_MD        Open/No arrest
    ## 574  Baltimore_MD        Open/No arrest
    ## 575  Baltimore_MD      Closed by arrest
    ## 576  Baltimore_MD      Closed by arrest
    ## 577  Baltimore_MD        Open/No arrest
    ## 578  Baltimore_MD      Closed by arrest
    ## 579  Baltimore_MD      Closed by arrest
    ## 580  Baltimore_MD Closed without arrest
    ## 581  Baltimore_MD      Closed by arrest
    ## 582  Baltimore_MD      Closed by arrest
    ## 583  Baltimore_MD      Closed by arrest
    ## 584  Baltimore_MD        Open/No arrest
    ## 585  Baltimore_MD        Open/No arrest
    ## 586  Baltimore_MD      Closed by arrest
    ## 587  Baltimore_MD      Closed by arrest
    ## 588  Baltimore_MD        Open/No arrest
    ## 589  Baltimore_MD        Open/No arrest
    ## 590  Baltimore_MD        Open/No arrest
    ## 591  Baltimore_MD      Closed by arrest
    ## 592  Baltimore_MD      Closed by arrest
    ## 593  Baltimore_MD Closed without arrest
    ## 594  Baltimore_MD      Closed by arrest
    ## 595  Baltimore_MD Closed without arrest
    ## 596  Baltimore_MD        Open/No arrest
    ## 597  Baltimore_MD      Closed by arrest
    ## 598  Baltimore_MD        Open/No arrest
    ## 599  Baltimore_MD      Closed by arrest
    ## 600  Baltimore_MD        Open/No arrest
    ## 601  Baltimore_MD        Open/No arrest
    ## 602  Baltimore_MD        Open/No arrest
    ## 603  Baltimore_MD        Open/No arrest
    ## 604  Baltimore_MD      Closed by arrest
    ## 605  Baltimore_MD      Closed by arrest
    ## 606  Baltimore_MD        Open/No arrest
    ## 607  Baltimore_MD        Open/No arrest
    ## 608  Baltimore_MD      Closed by arrest
    ## 609  Baltimore_MD      Closed by arrest
    ## 610  Baltimore_MD        Open/No arrest
    ## 611  Baltimore_MD        Open/No arrest
    ## 612  Baltimore_MD      Closed by arrest
    ## 613  Baltimore_MD      Closed by arrest
    ## 614  Baltimore_MD      Closed by arrest
    ## 615  Baltimore_MD      Closed by arrest
    ## 616  Baltimore_MD      Closed by arrest
    ## 617  Baltimore_MD        Open/No arrest
    ## 618  Baltimore_MD      Closed by arrest
    ## 619  Baltimore_MD        Open/No arrest
    ## 620  Baltimore_MD      Closed by arrest
    ## 621  Baltimore_MD        Open/No arrest
    ## 622  Baltimore_MD        Open/No arrest
    ## 623  Baltimore_MD        Open/No arrest
    ## 624  Baltimore_MD        Open/No arrest
    ## 625  Baltimore_MD        Open/No arrest
    ## 626  Baltimore_MD      Closed by arrest
    ## 627  Baltimore_MD        Open/No arrest
    ## 628  Baltimore_MD      Closed by arrest
    ## 629  Baltimore_MD        Open/No arrest
    ## 630  Baltimore_MD        Open/No arrest
    ## 631  Baltimore_MD        Open/No arrest
    ## 632  Baltimore_MD Closed without arrest
    ## 633  Baltimore_MD        Open/No arrest
    ## 634  Baltimore_MD      Closed by arrest
    ## 635  Baltimore_MD      Closed by arrest
    ## 636  Baltimore_MD      Closed by arrest
    ## 637  Baltimore_MD        Open/No arrest
    ## 638  Baltimore_MD      Closed by arrest
    ## 639  Baltimore_MD        Open/No arrest
    ## 640  Baltimore_MD      Closed by arrest
    ## 641  Baltimore_MD      Closed by arrest
    ## 642  Baltimore_MD      Closed by arrest
    ## 643  Baltimore_MD      Closed by arrest
    ## 644  Baltimore_MD        Open/No arrest
    ## 645  Baltimore_MD      Closed by arrest
    ## 646  Baltimore_MD        Open/No arrest
    ## 647  Baltimore_MD        Open/No arrest
    ## 648  Baltimore_MD        Open/No arrest
    ## 649  Baltimore_MD      Closed by arrest
    ## 650  Baltimore_MD        Open/No arrest
    ## 651  Baltimore_MD        Open/No arrest
    ## 652  Baltimore_MD        Open/No arrest
    ## 653  Baltimore_MD        Open/No arrest
    ## 654  Baltimore_MD        Open/No arrest
    ## 655  Baltimore_MD        Open/No arrest
    ## 656  Baltimore_MD      Closed by arrest
    ## 657  Baltimore_MD      Closed by arrest
    ## 658  Baltimore_MD        Open/No arrest
    ## 659  Baltimore_MD      Closed by arrest
    ## 660  Baltimore_MD        Open/No arrest
    ## 661  Baltimore_MD        Open/No arrest
    ## 662  Baltimore_MD        Open/No arrest
    ## 663  Baltimore_MD      Closed by arrest
    ## 664  Baltimore_MD      Closed by arrest
    ## 665  Baltimore_MD        Open/No arrest
    ## 666  Baltimore_MD Closed without arrest
    ## 667  Baltimore_MD      Closed by arrest
    ## 668  Baltimore_MD Closed without arrest
    ## 669  Baltimore_MD        Open/No arrest
    ## 670  Baltimore_MD      Closed by arrest
    ## 671  Baltimore_MD        Open/No arrest
    ## 672  Baltimore_MD        Open/No arrest
    ## 673  Baltimore_MD      Closed by arrest
    ## 674  Baltimore_MD        Open/No arrest
    ## 675  Baltimore_MD      Closed by arrest
    ## 676  Baltimore_MD      Closed by arrest
    ## 677  Baltimore_MD Closed without arrest
    ## 678  Baltimore_MD Closed without arrest
    ## 679  Baltimore_MD        Open/No arrest
    ## 680  Baltimore_MD      Closed by arrest
    ## 681  Baltimore_MD      Closed by arrest
    ## 682  Baltimore_MD        Open/No arrest
    ## 683  Baltimore_MD      Closed by arrest
    ## 684  Baltimore_MD      Closed by arrest
    ## 685  Baltimore_MD        Open/No arrest
    ## 686  Baltimore_MD        Open/No arrest
    ## 687  Baltimore_MD      Closed by arrest
    ## 688  Baltimore_MD        Open/No arrest
    ## 689  Baltimore_MD      Closed by arrest
    ## 690  Baltimore_MD        Open/No arrest
    ## 691  Baltimore_MD        Open/No arrest
    ## 692  Baltimore_MD        Open/No arrest
    ## 693  Baltimore_MD        Open/No arrest
    ## 694  Baltimore_MD      Closed by arrest
    ## 695  Baltimore_MD        Open/No arrest
    ## 696  Baltimore_MD      Closed by arrest
    ## 697  Baltimore_MD        Open/No arrest
    ## 698  Baltimore_MD      Closed by arrest
    ## 699  Baltimore_MD        Open/No arrest
    ## 700  Baltimore_MD      Closed by arrest
    ## 701  Baltimore_MD      Closed by arrest
    ## 702  Baltimore_MD        Open/No arrest
    ## 703  Baltimore_MD      Closed by arrest
    ## 704  Baltimore_MD        Open/No arrest
    ## 705  Baltimore_MD      Closed by arrest
    ## 706  Baltimore_MD        Open/No arrest
    ## 707  Baltimore_MD        Open/No arrest
    ## 708  Baltimore_MD        Open/No arrest
    ## 709  Baltimore_MD        Open/No arrest
    ## 710  Baltimore_MD        Open/No arrest
    ## 711  Baltimore_MD        Open/No arrest
    ## 712  Baltimore_MD      Closed by arrest
    ## 713  Baltimore_MD        Open/No arrest
    ## 714  Baltimore_MD        Open/No arrest
    ## 715  Baltimore_MD        Open/No arrest
    ## 716  Baltimore_MD        Open/No arrest
    ## 717  Baltimore_MD        Open/No arrest
    ## 718  Baltimore_MD      Closed by arrest
    ## 719  Baltimore_MD        Open/No arrest
    ## 720  Baltimore_MD        Open/No arrest
    ## 721  Baltimore_MD        Open/No arrest
    ## 722  Baltimore_MD        Open/No arrest
    ## 723  Baltimore_MD Closed without arrest
    ## 724  Baltimore_MD        Open/No arrest
    ## 725  Baltimore_MD        Open/No arrest
    ## 726  Baltimore_MD      Closed by arrest
    ## 727  Baltimore_MD        Open/No arrest
    ## 728  Baltimore_MD Closed without arrest
    ## 729  Baltimore_MD      Closed by arrest
    ## 730  Baltimore_MD      Closed by arrest
    ## 731  Baltimore_MD        Open/No arrest
    ## 732  Baltimore_MD Closed without arrest
    ## 733  Baltimore_MD        Open/No arrest
    ## 734  Baltimore_MD        Open/No arrest
    ## 735  Baltimore_MD        Open/No arrest
    ## 736  Baltimore_MD        Open/No arrest
    ## 737  Baltimore_MD      Closed by arrest
    ## 738  Baltimore_MD      Closed by arrest
    ## 739  Baltimore_MD        Open/No arrest
    ## 740  Baltimore_MD        Open/No arrest
    ## 741  Baltimore_MD      Closed by arrest
    ## 742  Baltimore_MD      Closed by arrest
    ## 743  Baltimore_MD      Closed by arrest
    ## 744  Baltimore_MD      Closed by arrest
    ## 745  Baltimore_MD      Closed by arrest
    ## 746  Baltimore_MD        Open/No arrest
    ## 747  Baltimore_MD        Open/No arrest
    ## 748  Baltimore_MD      Closed by arrest
    ## 749  Baltimore_MD      Closed by arrest
    ## 750  Baltimore_MD        Open/No arrest
    ## 751  Baltimore_MD        Open/No arrest
    ## 752  Baltimore_MD        Open/No arrest
    ## 753  Baltimore_MD        Open/No arrest
    ## 754  Baltimore_MD Closed without arrest
    ## 755  Baltimore_MD        Open/No arrest
    ## 756  Baltimore_MD        Open/No arrest
    ## 757  Baltimore_MD        Open/No arrest
    ## 758  Baltimore_MD        Open/No arrest
    ## 759  Baltimore_MD      Closed by arrest
    ## 760  Baltimore_MD      Closed by arrest
    ## 761  Baltimore_MD      Closed by arrest
    ## 762  Baltimore_MD Closed without arrest
    ## 763  Baltimore_MD Closed without arrest
    ## 764  Baltimore_MD      Closed by arrest
    ## 765  Baltimore_MD        Open/No arrest
    ## 766  Baltimore_MD      Closed by arrest
    ## 767  Baltimore_MD      Closed by arrest
    ## 768  Baltimore_MD        Open/No arrest
    ## 769  Baltimore_MD      Closed by arrest
    ## 770  Baltimore_MD        Open/No arrest
    ## 771  Baltimore_MD      Closed by arrest
    ## 772  Baltimore_MD        Open/No arrest
    ## 773  Baltimore_MD        Open/No arrest
    ## 774  Baltimore_MD      Closed by arrest
    ## 775  Baltimore_MD Closed without arrest
    ## 776  Baltimore_MD      Closed by arrest
    ## 777  Baltimore_MD        Open/No arrest
    ## 778  Baltimore_MD      Closed by arrest
    ## 779  Baltimore_MD        Open/No arrest
    ## 780  Baltimore_MD      Closed by arrest
    ## 781  Baltimore_MD      Closed by arrest
    ## 782  Baltimore_MD        Open/No arrest
    ## 783  Baltimore_MD        Open/No arrest
    ## 784  Baltimore_MD        Open/No arrest
    ## 785  Baltimore_MD      Closed by arrest
    ## 786  Baltimore_MD        Open/No arrest
    ## 787  Baltimore_MD      Closed by arrest
    ## 788  Baltimore_MD        Open/No arrest
    ## 789  Baltimore_MD        Open/No arrest
    ## 790  Baltimore_MD        Open/No arrest
    ## 791  Baltimore_MD        Open/No arrest
    ## 792  Baltimore_MD        Open/No arrest
    ## 793  Baltimore_MD      Closed by arrest
    ## 794  Baltimore_MD      Closed by arrest
    ## 795  Baltimore_MD Closed without arrest
    ## 796  Baltimore_MD        Open/No arrest
    ## 797  Baltimore_MD      Closed by arrest
    ## 798  Baltimore_MD        Open/No arrest
    ## 799  Baltimore_MD      Closed by arrest
    ## 800  Baltimore_MD        Open/No arrest
    ## 801  Baltimore_MD      Closed by arrest
    ## 802  Baltimore_MD        Open/No arrest
    ## 803  Baltimore_MD      Closed by arrest
    ## 804  Baltimore_MD        Open/No arrest
    ## 805  Baltimore_MD        Open/No arrest
    ## 806  Baltimore_MD        Open/No arrest
    ## 807  Baltimore_MD        Open/No arrest
    ## 808  Baltimore_MD        Open/No arrest
    ## 809  Baltimore_MD        Open/No arrest
    ## 810  Baltimore_MD      Closed by arrest
    ## 811  Baltimore_MD      Closed by arrest
    ## 812  Baltimore_MD      Closed by arrest
    ## 813  Baltimore_MD        Open/No arrest
    ## 814  Baltimore_MD        Open/No arrest
    ## 815  Baltimore_MD        Open/No arrest
    ## 816  Baltimore_MD      Closed by arrest
    ## 817  Baltimore_MD        Open/No arrest
    ## 818  Baltimore_MD      Closed by arrest
    ## 819  Baltimore_MD        Open/No arrest
    ## 820  Baltimore_MD        Open/No arrest
    ## 821  Baltimore_MD        Open/No arrest
    ## 822  Baltimore_MD      Closed by arrest
    ## 823  Baltimore_MD      Closed by arrest
    ## 824  Baltimore_MD      Closed by arrest
    ## 825  Baltimore_MD      Closed by arrest
    ## 826  Baltimore_MD      Closed by arrest
    ## 827  Baltimore_MD      Closed by arrest
    ## 828  Baltimore_MD      Closed by arrest
    ## 829  Baltimore_MD        Open/No arrest
    ## 830  Baltimore_MD      Closed by arrest
    ## 831  Baltimore_MD        Open/No arrest
    ## 832  Baltimore_MD        Open/No arrest
    ## 833  Baltimore_MD      Closed by arrest
    ## 834  Baltimore_MD      Closed by arrest
    ## 835  Baltimore_MD      Closed by arrest
    ## 836  Baltimore_MD      Closed by arrest
    ## 837  Baltimore_MD        Open/No arrest
    ## 838  Baltimore_MD      Closed by arrest
    ## 839  Baltimore_MD      Closed by arrest
    ## 840  Baltimore_MD      Closed by arrest
    ## 841  Baltimore_MD      Closed by arrest
    ## 842  Baltimore_MD        Open/No arrest
    ## 843  Baltimore_MD        Open/No arrest
    ## 844  Baltimore_MD Closed without arrest
    ## 845  Baltimore_MD      Closed by arrest
    ## 846  Baltimore_MD        Open/No arrest
    ## 847  Baltimore_MD        Open/No arrest
    ## 848  Baltimore_MD      Closed by arrest
    ## 849  Baltimore_MD        Open/No arrest
    ## 850  Baltimore_MD        Open/No arrest
    ## 851  Baltimore_MD      Closed by arrest
    ## 852  Baltimore_MD      Closed by arrest
    ## 853  Baltimore_MD        Open/No arrest
    ## 854  Baltimore_MD        Open/No arrest
    ## 855  Baltimore_MD        Open/No arrest
    ## 856  Baltimore_MD        Open/No arrest
    ## 857  Baltimore_MD        Open/No arrest
    ## 858  Baltimore_MD        Open/No arrest
    ## 859  Baltimore_MD        Open/No arrest
    ## 860  Baltimore_MD      Closed by arrest
    ## 861  Baltimore_MD      Closed by arrest
    ## 862  Baltimore_MD      Closed by arrest
    ## 863  Baltimore_MD      Closed by arrest
    ## 864  Baltimore_MD        Open/No arrest
    ## 865  Baltimore_MD      Closed by arrest
    ## 866  Baltimore_MD      Closed by arrest
    ## 867  Baltimore_MD        Open/No arrest
    ## 868  Baltimore_MD      Closed by arrest
    ## 869  Baltimore_MD      Closed by arrest
    ## 870  Baltimore_MD        Open/No arrest
    ## 871  Baltimore_MD        Open/No arrest
    ## 872  Baltimore_MD        Open/No arrest
    ## 873  Baltimore_MD Closed without arrest
    ## 874  Baltimore_MD      Closed by arrest
    ## 875  Baltimore_MD      Closed by arrest
    ## 876  Baltimore_MD      Closed by arrest
    ## 877  Baltimore_MD        Open/No arrest
    ## 878  Baltimore_MD      Closed by arrest
    ## 879  Baltimore_MD        Open/No arrest
    ## 880  Baltimore_MD        Open/No arrest
    ## 881  Baltimore_MD        Open/No arrest
    ## 882  Baltimore_MD        Open/No arrest
    ## 883  Baltimore_MD        Open/No arrest
    ## 884  Baltimore_MD      Closed by arrest
    ## 885  Baltimore_MD        Open/No arrest
    ## 886  Baltimore_MD      Closed by arrest
    ## 887  Baltimore_MD Closed without arrest
    ## 888  Baltimore_MD      Closed by arrest
    ## 889  Baltimore_MD        Open/No arrest
    ## 890  Baltimore_MD      Closed by arrest
    ## 891  Baltimore_MD        Open/No arrest
    ## 892  Baltimore_MD      Closed by arrest
    ## 893  Baltimore_MD        Open/No arrest
    ## 894  Baltimore_MD        Open/No arrest
    ## 895  Baltimore_MD      Closed by arrest
    ## 896  Baltimore_MD        Open/No arrest
    ## 897  Baltimore_MD      Closed by arrest
    ## 898  Baltimore_MD        Open/No arrest
    ## 899  Baltimore_MD      Closed by arrest
    ## 900  Baltimore_MD      Closed by arrest
    ## 901  Baltimore_MD        Open/No arrest
    ## 902  Baltimore_MD        Open/No arrest
    ## 903  Baltimore_MD Closed without arrest
    ## 904  Baltimore_MD      Closed by arrest
    ## 905  Baltimore_MD      Closed by arrest
    ## 906  Baltimore_MD Closed without arrest
    ## 907  Baltimore_MD        Open/No arrest
    ## 908  Baltimore_MD      Closed by arrest
    ## 909  Baltimore_MD      Closed by arrest
    ## 910  Baltimore_MD        Open/No arrest
    ## 911  Baltimore_MD        Open/No arrest
    ## 912  Baltimore_MD      Closed by arrest
    ## 913  Baltimore_MD      Closed by arrest
    ## 914  Baltimore_MD      Closed by arrest
    ## 915  Baltimore_MD        Open/No arrest
    ## 916  Baltimore_MD        Open/No arrest
    ## 917  Baltimore_MD      Closed by arrest
    ## 918  Baltimore_MD      Closed by arrest
    ## 919  Baltimore_MD Closed without arrest
    ## 920  Baltimore_MD        Open/No arrest
    ## 921  Baltimore_MD        Open/No arrest
    ## 922  Baltimore_MD      Closed by arrest
    ## 923  Baltimore_MD      Closed by arrest
    ## 924  Baltimore_MD      Closed by arrest
    ## 925  Baltimore_MD        Open/No arrest
    ## 926  Baltimore_MD        Open/No arrest
    ## 927  Baltimore_MD      Closed by arrest
    ## 928  Baltimore_MD Closed without arrest
    ## 929  Baltimore_MD        Open/No arrest
    ## 930  Baltimore_MD      Closed by arrest
    ## 931  Baltimore_MD      Closed by arrest
    ## 932  Baltimore_MD        Open/No arrest
    ## 933  Baltimore_MD        Open/No arrest
    ## 934  Baltimore_MD      Closed by arrest
    ## 935  Baltimore_MD        Open/No arrest
    ## 936  Baltimore_MD        Open/No arrest
    ## 937  Baltimore_MD      Closed by arrest
    ## 938  Baltimore_MD      Closed by arrest
    ## 939  Baltimore_MD        Open/No arrest
    ## 940  Baltimore_MD        Open/No arrest
    ## 941  Baltimore_MD        Open/No arrest
    ## 942  Baltimore_MD      Closed by arrest
    ## 943  Baltimore_MD        Open/No arrest
    ## 944  Baltimore_MD Closed without arrest
    ## 945  Baltimore_MD        Open/No arrest
    ## 946  Baltimore_MD        Open/No arrest
    ## 947  Baltimore_MD      Closed by arrest
    ## 948  Baltimore_MD        Open/No arrest
    ## 949  Baltimore_MD        Open/No arrest
    ## 950  Baltimore_MD      Closed by arrest
    ## 951  Baltimore_MD        Open/No arrest
    ## 952  Baltimore_MD      Closed by arrest
    ## 953  Baltimore_MD        Open/No arrest
    ## 954  Baltimore_MD        Open/No arrest
    ## 955  Baltimore_MD        Open/No arrest
    ## 956  Baltimore_MD        Open/No arrest
    ## 957  Baltimore_MD        Open/No arrest
    ## 958  Baltimore_MD      Closed by arrest
    ## 959  Baltimore_MD      Closed by arrest
    ## 960  Baltimore_MD Closed without arrest
    ## 961  Baltimore_MD        Open/No arrest
    ## 962  Baltimore_MD        Open/No arrest
    ## 963  Baltimore_MD        Open/No arrest
    ## 964  Baltimore_MD      Closed by arrest
    ## 965  Baltimore_MD      Closed by arrest
    ## 966  Baltimore_MD        Open/No arrest
    ## 967  Baltimore_MD        Open/No arrest
    ## 968  Baltimore_MD Closed without arrest
    ## 969  Baltimore_MD        Open/No arrest
    ## 970  Baltimore_MD        Open/No arrest
    ## 971  Baltimore_MD      Closed by arrest
    ## 972  Baltimore_MD        Open/No arrest
    ## 973  Baltimore_MD        Open/No arrest
    ## 974  Baltimore_MD        Open/No arrest
    ## 975  Baltimore_MD        Open/No arrest
    ## 976  Baltimore_MD        Open/No arrest
    ## 977  Baltimore_MD        Open/No arrest
    ## 978  Baltimore_MD      Closed by arrest
    ## 979  Baltimore_MD      Closed by arrest
    ## 980  Baltimore_MD      Closed by arrest
    ## 981  Baltimore_MD      Closed by arrest
    ## 982  Baltimore_MD        Open/No arrest
    ## 983  Baltimore_MD        Open/No arrest
    ## 984  Baltimore_MD      Closed by arrest
    ## 985  Baltimore_MD      Closed by arrest
    ## 986  Baltimore_MD        Open/No arrest
    ## 987  Baltimore_MD      Closed by arrest
    ## 988  Baltimore_MD        Open/No arrest
    ## 989  Baltimore_MD        Open/No arrest
    ## 990  Baltimore_MD      Closed by arrest
    ## 991  Baltimore_MD      Closed by arrest
    ## 992  Baltimore_MD      Closed by arrest
    ## 993  Baltimore_MD        Open/No arrest
    ## 994  Baltimore_MD      Closed by arrest
    ## 995  Baltimore_MD      Closed by arrest
    ## 996  Baltimore_MD Closed without arrest
    ## 997  Baltimore_MD      Closed by arrest
    ## 998  Baltimore_MD        Open/No arrest
    ## 999  Baltimore_MD      Closed by arrest
    ## 1000 Baltimore_MD      Closed by arrest
    ## 1001 Baltimore_MD      Closed by arrest
    ## 1002 Baltimore_MD        Open/No arrest
    ## 1003 Baltimore_MD      Closed by arrest
    ## 1004 Baltimore_MD      Closed by arrest
    ## 1005 Baltimore_MD      Closed by arrest
    ## 1006 Baltimore_MD      Closed by arrest
    ## 1007 Baltimore_MD      Closed by arrest
    ## 1008 Baltimore_MD        Open/No arrest
    ## 1009 Baltimore_MD      Closed by arrest
    ## 1010 Baltimore_MD        Open/No arrest
    ## 1011 Baltimore_MD      Closed by arrest
    ## 1012 Baltimore_MD        Open/No arrest
    ## 1013 Baltimore_MD        Open/No arrest
    ## 1014 Baltimore_MD        Open/No arrest
    ## 1015 Baltimore_MD        Open/No arrest
    ## 1016 Baltimore_MD        Open/No arrest
    ## 1017 Baltimore_MD      Closed by arrest
    ## 1018 Baltimore_MD      Closed by arrest
    ## 1019 Baltimore_MD        Open/No arrest
    ## 1020 Baltimore_MD      Closed by arrest
    ## 1021 Baltimore_MD      Closed by arrest
    ## 1022 Baltimore_MD      Closed by arrest
    ## 1023 Baltimore_MD Closed without arrest
    ## 1024 Baltimore_MD Closed without arrest
    ## 1025 Baltimore_MD        Open/No arrest
    ## 1026 Baltimore_MD        Open/No arrest
    ## 1027 Baltimore_MD      Closed by arrest
    ## 1028 Baltimore_MD      Closed by arrest
    ## 1029 Baltimore_MD        Open/No arrest
    ## 1030 Baltimore_MD      Closed by arrest
    ## 1031 Baltimore_MD        Open/No arrest
    ## 1032 Baltimore_MD        Open/No arrest
    ## 1033 Baltimore_MD        Open/No arrest
    ## 1034 Baltimore_MD        Open/No arrest
    ## 1035 Baltimore_MD        Open/No arrest
    ## 1036 Baltimore_MD        Open/No arrest
    ## 1037 Baltimore_MD        Open/No arrest
    ## 1038 Baltimore_MD      Closed by arrest
    ## 1039 Baltimore_MD      Closed by arrest
    ## 1040 Baltimore_MD      Closed by arrest
    ## 1041 Baltimore_MD Closed without arrest
    ## 1042 Baltimore_MD        Open/No arrest
    ## 1043 Baltimore_MD Closed without arrest
    ## 1044 Baltimore_MD        Open/No arrest
    ## 1045 Baltimore_MD        Open/No arrest
    ## 1046 Baltimore_MD      Closed by arrest
    ## 1047 Baltimore_MD        Open/No arrest
    ## 1048 Baltimore_MD        Open/No arrest
    ## 1049 Baltimore_MD      Closed by arrest
    ## 1050 Baltimore_MD      Closed by arrest
    ## 1051 Baltimore_MD        Open/No arrest
    ## 1052 Baltimore_MD        Open/No arrest
    ## 1053 Baltimore_MD      Closed by arrest
    ## 1054 Baltimore_MD        Open/No arrest
    ## 1055 Baltimore_MD        Open/No arrest
    ## 1056 Baltimore_MD        Open/No arrest
    ## 1057 Baltimore_MD Closed without arrest
    ## 1058 Baltimore_MD        Open/No arrest
    ## 1059 Baltimore_MD        Open/No arrest
    ## 1060 Baltimore_MD        Open/No arrest
    ## 1061 Baltimore_MD        Open/No arrest
    ## 1062 Baltimore_MD      Closed by arrest
    ## 1063 Baltimore_MD        Open/No arrest
    ## 1064 Baltimore_MD        Open/No arrest
    ## 1065 Baltimore_MD        Open/No arrest
    ## 1066 Baltimore_MD        Open/No arrest
    ## 1067 Baltimore_MD      Closed by arrest
    ## 1068 Baltimore_MD      Closed by arrest
    ## 1069 Baltimore_MD      Closed by arrest
    ## 1070 Baltimore_MD        Open/No arrest
    ## 1071 Baltimore_MD      Closed by arrest
    ## 1072 Baltimore_MD        Open/No arrest
    ## 1073 Baltimore_MD        Open/No arrest
    ## 1074 Baltimore_MD      Closed by arrest
    ## 1075 Baltimore_MD      Closed by arrest
    ## 1076 Baltimore_MD        Open/No arrest
    ## 1077 Baltimore_MD      Closed by arrest
    ## 1078 Baltimore_MD        Open/No arrest
    ## 1079 Baltimore_MD      Closed by arrest
    ## 1080 Baltimore_MD      Closed by arrest
    ## 1081 Baltimore_MD        Open/No arrest
    ## 1082 Baltimore_MD      Closed by arrest
    ## 1083 Baltimore_MD        Open/No arrest
    ## 1084 Baltimore_MD        Open/No arrest
    ## 1085 Baltimore_MD        Open/No arrest
    ## 1086 Baltimore_MD      Closed by arrest
    ## 1087 Baltimore_MD        Open/No arrest
    ## 1088 Baltimore_MD        Open/No arrest
    ## 1089 Baltimore_MD        Open/No arrest
    ## 1090 Baltimore_MD        Open/No arrest
    ## 1091 Baltimore_MD      Closed by arrest
    ## 1092 Baltimore_MD        Open/No arrest
    ## 1093 Baltimore_MD        Open/No arrest
    ## 1094 Baltimore_MD        Open/No arrest
    ## 1095 Baltimore_MD        Open/No arrest
    ## 1096 Baltimore_MD        Open/No arrest
    ## 1097 Baltimore_MD      Closed by arrest
    ## 1098 Baltimore_MD      Closed by arrest
    ## 1099 Baltimore_MD        Open/No arrest
    ## 1100 Baltimore_MD        Open/No arrest
    ## 1101 Baltimore_MD      Closed by arrest
    ## 1102 Baltimore_MD        Open/No arrest
    ## 1103 Baltimore_MD        Open/No arrest
    ## 1104 Baltimore_MD        Open/No arrest
    ## 1105 Baltimore_MD        Open/No arrest
    ## 1106 Baltimore_MD        Open/No arrest
    ## 1107 Baltimore_MD      Closed by arrest
    ## 1108 Baltimore_MD      Closed by arrest
    ## 1109 Baltimore_MD      Closed by arrest
    ## 1110 Baltimore_MD Closed without arrest
    ## 1111 Baltimore_MD        Open/No arrest
    ## 1112 Baltimore_MD      Closed by arrest
    ## 1113 Baltimore_MD        Open/No arrest
    ## 1114 Baltimore_MD        Open/No arrest
    ## 1115 Baltimore_MD        Open/No arrest
    ## 1116 Baltimore_MD        Open/No arrest
    ## 1117 Baltimore_MD        Open/No arrest
    ## 1118 Baltimore_MD        Open/No arrest
    ## 1119 Baltimore_MD Closed without arrest
    ## 1120 Baltimore_MD      Closed by arrest
    ## 1121 Baltimore_MD        Open/No arrest
    ## 1122 Baltimore_MD        Open/No arrest
    ## 1123 Baltimore_MD        Open/No arrest
    ## 1124 Baltimore_MD        Open/No arrest
    ## 1125 Baltimore_MD        Open/No arrest
    ## 1126 Baltimore_MD        Open/No arrest
    ## 1127 Baltimore_MD        Open/No arrest
    ## 1128 Baltimore_MD        Open/No arrest
    ## 1129 Baltimore_MD      Closed by arrest
    ## 1130 Baltimore_MD      Closed by arrest
    ## 1131 Baltimore_MD        Open/No arrest
    ## 1132 Baltimore_MD Closed without arrest
    ## 1133 Baltimore_MD      Closed by arrest
    ## 1134 Baltimore_MD      Closed by arrest
    ## 1135 Baltimore_MD        Open/No arrest
    ## 1136 Baltimore_MD      Closed by arrest
    ## 1137 Baltimore_MD      Closed by arrest
    ## 1138 Baltimore_MD        Open/No arrest
    ## 1139 Baltimore_MD        Open/No arrest
    ## 1140 Baltimore_MD        Open/No arrest
    ## 1141 Baltimore_MD        Open/No arrest
    ## 1142 Baltimore_MD      Closed by arrest
    ## 1143 Baltimore_MD Closed without arrest
    ## 1144 Baltimore_MD      Closed by arrest
    ## 1145 Baltimore_MD Closed without arrest
    ## 1146 Baltimore_MD      Closed by arrest
    ## 1147 Baltimore_MD        Open/No arrest
    ## 1148 Baltimore_MD      Closed by arrest
    ## 1149 Baltimore_MD        Open/No arrest
    ## 1150 Baltimore_MD      Closed by arrest
    ## 1151 Baltimore_MD        Open/No arrest
    ## 1152 Baltimore_MD      Closed by arrest
    ## 1153 Baltimore_MD      Closed by arrest
    ## 1154 Baltimore_MD        Open/No arrest
    ## 1155 Baltimore_MD Closed without arrest
    ## 1156 Baltimore_MD        Open/No arrest
    ## 1157 Baltimore_MD        Open/No arrest
    ## 1158 Baltimore_MD      Closed by arrest
    ## 1159 Baltimore_MD        Open/No arrest
    ## 1160 Baltimore_MD        Open/No arrest
    ## 1161 Baltimore_MD        Open/No arrest
    ## 1162 Baltimore_MD      Closed by arrest
    ## 1163 Baltimore_MD      Closed by arrest
    ## 1164 Baltimore_MD        Open/No arrest
    ## 1165 Baltimore_MD        Open/No arrest
    ## 1166 Baltimore_MD        Open/No arrest
    ## 1167 Baltimore_MD      Closed by arrest
    ## 1168 Baltimore_MD        Open/No arrest
    ## 1169 Baltimore_MD Closed without arrest
    ## 1170 Baltimore_MD        Open/No arrest
    ## 1171 Baltimore_MD        Open/No arrest
    ## 1172 Baltimore_MD        Open/No arrest
    ## 1173 Baltimore_MD        Open/No arrest
    ## 1174 Baltimore_MD        Open/No arrest
    ## 1175 Baltimore_MD      Closed by arrest
    ## 1176 Baltimore_MD      Closed by arrest
    ## 1177 Baltimore_MD        Open/No arrest
    ## 1178 Baltimore_MD      Closed by arrest
    ## 1179 Baltimore_MD      Closed by arrest
    ## 1180 Baltimore_MD      Closed by arrest
    ## 1181 Baltimore_MD        Open/No arrest
    ## 1182 Baltimore_MD        Open/No arrest
    ## 1183 Baltimore_MD      Closed by arrest
    ## 1184 Baltimore_MD        Open/No arrest
    ## 1185 Baltimore_MD      Closed by arrest
    ## 1186 Baltimore_MD Closed without arrest
    ## 1187 Baltimore_MD      Closed by arrest
    ## 1188 Baltimore_MD      Closed by arrest
    ## 1189 Baltimore_MD        Open/No arrest
    ## 1190 Baltimore_MD      Closed by arrest
    ## 1191 Baltimore_MD      Closed by arrest
    ## 1192 Baltimore_MD        Open/No arrest
    ## 1193 Baltimore_MD      Closed by arrest
    ## 1194 Baltimore_MD      Closed by arrest
    ## 1195 Baltimore_MD        Open/No arrest
    ## 1196 Baltimore_MD      Closed by arrest
    ## 1197 Baltimore_MD Closed without arrest
    ## 1198 Baltimore_MD      Closed by arrest
    ## 1199 Baltimore_MD        Open/No arrest
    ## 1200 Baltimore_MD      Closed by arrest
    ## 1201 Baltimore_MD      Closed by arrest
    ## 1202 Baltimore_MD        Open/No arrest
    ## 1203 Baltimore_MD        Open/No arrest
    ## 1204 Baltimore_MD        Open/No arrest
    ## 1205 Baltimore_MD      Closed by arrest
    ## 1206 Baltimore_MD      Closed by arrest
    ## 1207 Baltimore_MD        Open/No arrest
    ## 1208 Baltimore_MD      Closed by arrest
    ## 1209 Baltimore_MD        Open/No arrest
    ## 1210 Baltimore_MD        Open/No arrest
    ## 1211 Baltimore_MD Closed without arrest
    ## 1212 Baltimore_MD      Closed by arrest
    ## 1213 Baltimore_MD      Closed by arrest
    ## 1214 Baltimore_MD      Closed by arrest
    ## 1215 Baltimore_MD      Closed by arrest
    ## 1216 Baltimore_MD        Open/No arrest
    ## 1217 Baltimore_MD        Open/No arrest
    ## 1218 Baltimore_MD      Closed by arrest
    ## 1219 Baltimore_MD        Open/No arrest
    ## 1220 Baltimore_MD        Open/No arrest
    ## 1221 Baltimore_MD        Open/No arrest
    ## 1222 Baltimore_MD        Open/No arrest
    ## 1223 Baltimore_MD        Open/No arrest
    ## 1224 Baltimore_MD      Closed by arrest
    ## 1225 Baltimore_MD      Closed by arrest
    ## 1226 Baltimore_MD        Open/No arrest
    ## 1227 Baltimore_MD Closed without arrest
    ## 1228 Baltimore_MD      Closed by arrest
    ## 1229 Baltimore_MD        Open/No arrest
    ## 1230 Baltimore_MD      Closed by arrest
    ## 1231 Baltimore_MD      Closed by arrest
    ## 1232 Baltimore_MD        Open/No arrest
    ## 1233 Baltimore_MD        Open/No arrest
    ## 1234 Baltimore_MD        Open/No arrest
    ## 1235 Baltimore_MD        Open/No arrest
    ## 1236 Baltimore_MD Closed without arrest
    ## 1237 Baltimore_MD        Open/No arrest
    ## 1238 Baltimore_MD      Closed by arrest
    ## 1239 Baltimore_MD      Closed by arrest
    ## 1240 Baltimore_MD        Open/No arrest
    ## 1241 Baltimore_MD        Open/No arrest
    ## 1242 Baltimore_MD        Open/No arrest
    ## 1243 Baltimore_MD      Closed by arrest
    ## 1244 Baltimore_MD      Closed by arrest
    ## 1245 Baltimore_MD      Closed by arrest
    ## 1246 Baltimore_MD        Open/No arrest
    ## 1247 Baltimore_MD        Open/No arrest
    ## 1248 Baltimore_MD        Open/No arrest
    ## 1249 Baltimore_MD        Open/No arrest
    ## 1250 Baltimore_MD        Open/No arrest
    ## 1251 Baltimore_MD      Closed by arrest
    ## 1252 Baltimore_MD      Closed by arrest
    ## 1253 Baltimore_MD      Closed by arrest
    ## 1254 Baltimore_MD      Closed by arrest
    ## 1255 Baltimore_MD      Closed by arrest
    ## 1256 Baltimore_MD      Closed by arrest
    ## 1257 Baltimore_MD        Open/No arrest
    ## 1258 Baltimore_MD        Open/No arrest
    ## 1259 Baltimore_MD      Closed by arrest
    ## 1260 Baltimore_MD        Open/No arrest
    ## 1261 Baltimore_MD        Open/No arrest
    ## 1262 Baltimore_MD        Open/No arrest
    ## 1263 Baltimore_MD        Open/No arrest
    ## 1264 Baltimore_MD Closed without arrest
    ## 1265 Baltimore_MD      Closed by arrest
    ## 1266 Baltimore_MD      Closed by arrest
    ## 1267 Baltimore_MD        Open/No arrest
    ## 1268 Baltimore_MD      Closed by arrest
    ## 1269 Baltimore_MD        Open/No arrest
    ## 1270 Baltimore_MD      Closed by arrest
    ## 1271 Baltimore_MD        Open/No arrest
    ## 1272 Baltimore_MD        Open/No arrest
    ## 1273 Baltimore_MD        Open/No arrest
    ## 1274 Baltimore_MD      Closed by arrest
    ## 1275 Baltimore_MD      Closed by arrest
    ## 1276 Baltimore_MD        Open/No arrest
    ## 1277 Baltimore_MD        Open/No arrest
    ## 1278 Baltimore_MD      Closed by arrest
    ## 1279 Baltimore_MD Closed without arrest
    ## 1280 Baltimore_MD        Open/No arrest
    ## 1281 Baltimore_MD        Open/No arrest
    ## 1282 Baltimore_MD        Open/No arrest
    ## 1283 Baltimore_MD        Open/No arrest
    ## 1284 Baltimore_MD      Closed by arrest
    ## 1285 Baltimore_MD      Closed by arrest
    ## 1286 Baltimore_MD      Closed by arrest
    ## 1287 Baltimore_MD        Open/No arrest
    ## 1288 Baltimore_MD        Open/No arrest
    ## 1289 Baltimore_MD        Open/No arrest
    ## 1290 Baltimore_MD        Open/No arrest
    ## 1291 Baltimore_MD        Open/No arrest
    ## 1292 Baltimore_MD      Closed by arrest
    ## 1293 Baltimore_MD      Closed by arrest
    ## 1294 Baltimore_MD      Closed by arrest
    ## 1295 Baltimore_MD      Closed by arrest
    ## 1296 Baltimore_MD      Closed by arrest
    ## 1297 Baltimore_MD      Closed by arrest
    ## 1298 Baltimore_MD        Open/No arrest
    ## 1299 Baltimore_MD        Open/No arrest
    ## 1300 Baltimore_MD        Open/No arrest
    ## 1301 Baltimore_MD      Closed by arrest
    ## 1302 Baltimore_MD        Open/No arrest
    ## 1303 Baltimore_MD        Open/No arrest
    ## 1304 Baltimore_MD        Open/No arrest
    ## 1305 Baltimore_MD      Closed by arrest
    ## 1306 Baltimore_MD      Closed by arrest
    ## 1307 Baltimore_MD      Closed by arrest
    ## 1308 Baltimore_MD        Open/No arrest
    ## 1309 Baltimore_MD        Open/No arrest
    ## 1310 Baltimore_MD        Open/No arrest
    ## 1311 Baltimore_MD      Closed by arrest
    ## 1312 Baltimore_MD        Open/No arrest
    ## 1313 Baltimore_MD        Open/No arrest
    ## 1314 Baltimore_MD        Open/No arrest
    ## 1315 Baltimore_MD        Open/No arrest
    ## 1316 Baltimore_MD        Open/No arrest
    ## 1317 Baltimore_MD      Closed by arrest
    ## 1318 Baltimore_MD        Open/No arrest
    ## 1319 Baltimore_MD        Open/No arrest
    ## 1320 Baltimore_MD        Open/No arrest
    ## 1321 Baltimore_MD        Open/No arrest
    ## 1322 Baltimore_MD      Closed by arrest
    ## 1323 Baltimore_MD Closed without arrest
    ## 1324 Baltimore_MD        Open/No arrest
    ## 1325 Baltimore_MD        Open/No arrest
    ## 1326 Baltimore_MD        Open/No arrest
    ## 1327 Baltimore_MD        Open/No arrest
    ## 1328 Baltimore_MD        Open/No arrest
    ## 1329 Baltimore_MD        Open/No arrest
    ## 1330 Baltimore_MD        Open/No arrest
    ## 1331 Baltimore_MD      Closed by arrest
    ## 1332 Baltimore_MD        Open/No arrest
    ## 1333 Baltimore_MD        Open/No arrest
    ## 1334 Baltimore_MD        Open/No arrest
    ## 1335 Baltimore_MD      Closed by arrest
    ## 1336 Baltimore_MD        Open/No arrest
    ## 1337 Baltimore_MD        Open/No arrest
    ## 1338 Baltimore_MD        Open/No arrest
    ## 1339 Baltimore_MD      Closed by arrest
    ## 1340 Baltimore_MD        Open/No arrest
    ## 1341 Baltimore_MD      Closed by arrest
    ## 1342 Baltimore_MD        Open/No arrest
    ## 1343 Baltimore_MD        Open/No arrest
    ## 1344 Baltimore_MD        Open/No arrest
    ## 1345 Baltimore_MD      Closed by arrest
    ## 1346 Baltimore_MD      Closed by arrest
    ## 1347 Baltimore_MD        Open/No arrest
    ## 1348 Baltimore_MD        Open/No arrest
    ## 1349 Baltimore_MD Closed without arrest
    ## 1350 Baltimore_MD        Open/No arrest
    ## 1351 Baltimore_MD      Closed by arrest
    ## 1352 Baltimore_MD        Open/No arrest
    ## 1353 Baltimore_MD      Closed by arrest
    ## 1354 Baltimore_MD        Open/No arrest
    ## 1355 Baltimore_MD      Closed by arrest
    ## 1356 Baltimore_MD      Closed by arrest
    ## 1357 Baltimore_MD      Closed by arrest
    ## 1358 Baltimore_MD      Closed by arrest
    ## 1359 Baltimore_MD        Open/No arrest
    ## 1360 Baltimore_MD        Open/No arrest
    ## 1361 Baltimore_MD      Closed by arrest
    ## 1362 Baltimore_MD      Closed by arrest
    ## 1363 Baltimore_MD Closed without arrest
    ## 1364 Baltimore_MD        Open/No arrest
    ## 1365 Baltimore_MD        Open/No arrest
    ## 1366 Baltimore_MD        Open/No arrest
    ## 1367 Baltimore_MD        Open/No arrest
    ## 1368 Baltimore_MD      Closed by arrest
    ## 1369 Baltimore_MD        Open/No arrest
    ## 1370 Baltimore_MD      Closed by arrest
    ## 1371 Baltimore_MD      Closed by arrest
    ## 1372 Baltimore_MD      Closed by arrest
    ## 1373 Baltimore_MD        Open/No arrest
    ## 1374 Baltimore_MD        Open/No arrest
    ## 1375 Baltimore_MD      Closed by arrest
    ## 1376 Baltimore_MD        Open/No arrest
    ## 1377 Baltimore_MD      Closed by arrest
    ## 1378 Baltimore_MD        Open/No arrest
    ## 1379 Baltimore_MD      Closed by arrest
    ## 1380 Baltimore_MD Closed without arrest
    ## 1381 Baltimore_MD        Open/No arrest
    ## 1382 Baltimore_MD        Open/No arrest
    ## 1383 Baltimore_MD        Open/No arrest
    ## 1384 Baltimore_MD      Closed by arrest
    ## 1385 Baltimore_MD        Open/No arrest
    ## 1386 Baltimore_MD        Open/No arrest
    ## 1387 Baltimore_MD      Closed by arrest
    ## 1388 Baltimore_MD        Open/No arrest
    ## 1389 Baltimore_MD      Closed by arrest
    ## 1390 Baltimore_MD      Closed by arrest
    ## 1391 Baltimore_MD        Open/No arrest
    ## 1392 Baltimore_MD      Closed by arrest
    ## 1393 Baltimore_MD        Open/No arrest
    ## 1394 Baltimore_MD        Open/No arrest
    ## 1395 Baltimore_MD        Open/No arrest
    ## 1396 Baltimore_MD        Open/No arrest
    ## 1397 Baltimore_MD      Closed by arrest
    ## 1398 Baltimore_MD        Open/No arrest
    ## 1399 Baltimore_MD      Closed by arrest
    ## 1400 Baltimore_MD      Closed by arrest
    ## 1401 Baltimore_MD      Closed by arrest
    ## 1402 Baltimore_MD        Open/No arrest
    ## 1403 Baltimore_MD      Closed by arrest
    ## 1404 Baltimore_MD      Closed by arrest
    ## 1405 Baltimore_MD        Open/No arrest
    ## 1406 Baltimore_MD      Closed by arrest
    ## 1407 Baltimore_MD Closed without arrest
    ## 1408 Baltimore_MD      Closed by arrest
    ## 1409 Baltimore_MD        Open/No arrest
    ## 1410 Baltimore_MD        Open/No arrest
    ## 1411 Baltimore_MD      Closed by arrest
    ## 1412 Baltimore_MD      Closed by arrest
    ## 1413 Baltimore_MD      Closed by arrest
    ## 1414 Baltimore_MD        Open/No arrest
    ## 1415 Baltimore_MD      Closed by arrest
    ## 1416 Baltimore_MD      Closed by arrest
    ## 1417 Baltimore_MD        Open/No arrest
    ## 1418 Baltimore_MD      Closed by arrest
    ## 1419 Baltimore_MD      Closed by arrest
    ## 1420 Baltimore_MD      Closed by arrest
    ## 1421 Baltimore_MD      Closed by arrest
    ## 1422 Baltimore_MD      Closed by arrest
    ## 1423 Baltimore_MD        Open/No arrest
    ## 1424 Baltimore_MD        Open/No arrest
    ## 1425 Baltimore_MD      Closed by arrest
    ## 1426 Baltimore_MD        Open/No arrest
    ## 1427 Baltimore_MD        Open/No arrest
    ## 1428 Baltimore_MD        Open/No arrest
    ## 1429 Baltimore_MD        Open/No arrest
    ## 1430 Baltimore_MD        Open/No arrest
    ## 1431 Baltimore_MD        Open/No arrest
    ## 1432 Baltimore_MD        Open/No arrest
    ## 1433 Baltimore_MD      Closed by arrest
    ## 1434 Baltimore_MD      Closed by arrest
    ## 1435 Baltimore_MD        Open/No arrest
    ## 1436 Baltimore_MD        Open/No arrest
    ## 1437 Baltimore_MD      Closed by arrest
    ## 1438 Baltimore_MD      Closed by arrest
    ## 1439 Baltimore_MD      Closed by arrest
    ## 1440 Baltimore_MD        Open/No arrest
    ## 1441 Baltimore_MD      Closed by arrest
    ## 1442 Baltimore_MD        Open/No arrest
    ## 1443 Baltimore_MD        Open/No arrest
    ## 1444 Baltimore_MD      Closed by arrest
    ## 1445 Baltimore_MD        Open/No arrest
    ## 1446 Baltimore_MD        Open/No arrest
    ## 1447 Baltimore_MD        Open/No arrest
    ## 1448 Baltimore_MD      Closed by arrest
    ## 1449 Baltimore_MD        Open/No arrest
    ## 1450 Baltimore_MD      Closed by arrest
    ## 1451 Baltimore_MD        Open/No arrest
    ## 1452 Baltimore_MD      Closed by arrest
    ## 1453 Baltimore_MD        Open/No arrest
    ## 1454 Baltimore_MD      Closed by arrest
    ## 1455 Baltimore_MD      Closed by arrest
    ## 1456 Baltimore_MD        Open/No arrest
    ## 1457 Baltimore_MD        Open/No arrest
    ## 1458 Baltimore_MD      Closed by arrest
    ## 1459 Baltimore_MD      Closed by arrest
    ## 1460 Baltimore_MD      Closed by arrest
    ## 1461 Baltimore_MD        Open/No arrest
    ## 1462 Baltimore_MD      Closed by arrest
    ## 1463 Baltimore_MD      Closed by arrest
    ## 1464 Baltimore_MD      Closed by arrest
    ## 1465 Baltimore_MD      Closed by arrest
    ## 1466 Baltimore_MD      Closed by arrest
    ## 1467 Baltimore_MD      Closed by arrest
    ## 1468 Baltimore_MD      Closed by arrest
    ## 1469 Baltimore_MD        Open/No arrest
    ## 1470 Baltimore_MD        Open/No arrest
    ## 1471 Baltimore_MD      Closed by arrest
    ## 1472 Baltimore_MD Closed without arrest
    ## 1473 Baltimore_MD        Open/No arrest
    ## 1474 Baltimore_MD Closed without arrest
    ## 1475 Baltimore_MD      Closed by arrest
    ## 1476 Baltimore_MD        Open/No arrest
    ## 1477 Baltimore_MD        Open/No arrest
    ## 1478 Baltimore_MD        Open/No arrest
    ## 1479 Baltimore_MD      Closed by arrest
    ## 1480 Baltimore_MD        Open/No arrest
    ## 1481 Baltimore_MD        Open/No arrest
    ## 1482 Baltimore_MD        Open/No arrest
    ## 1483 Baltimore_MD        Open/No arrest
    ## 1484 Baltimore_MD Closed without arrest
    ## 1485 Baltimore_MD        Open/No arrest
    ## 1486 Baltimore_MD      Closed by arrest
    ## 1487 Baltimore_MD        Open/No arrest
    ## 1488 Baltimore_MD        Open/No arrest
    ## 1489 Baltimore_MD        Open/No arrest
    ## 1490 Baltimore_MD      Closed by arrest
    ## 1491 Baltimore_MD        Open/No arrest
    ## 1492 Baltimore_MD        Open/No arrest
    ## 1493 Baltimore_MD        Open/No arrest
    ## 1494 Baltimore_MD        Open/No arrest
    ## 1495 Baltimore_MD      Closed by arrest
    ## 1496 Baltimore_MD      Closed by arrest
    ## 1497 Baltimore_MD        Open/No arrest
    ## 1498 Baltimore_MD      Closed by arrest
    ## 1499 Baltimore_MD      Closed by arrest
    ## 1500 Baltimore_MD        Open/No arrest
    ## 1501 Baltimore_MD        Open/No arrest
    ## 1502 Baltimore_MD      Closed by arrest
    ## 1503 Baltimore_MD        Open/No arrest
    ## 1504 Baltimore_MD        Open/No arrest
    ## 1505 Baltimore_MD        Open/No arrest
    ## 1506 Baltimore_MD      Closed by arrest
    ## 1507 Baltimore_MD      Closed by arrest
    ## 1508 Baltimore_MD      Closed by arrest
    ## 1509 Baltimore_MD      Closed by arrest
    ## 1510 Baltimore_MD Closed without arrest
    ## 1511 Baltimore_MD        Open/No arrest
    ## 1512 Baltimore_MD        Open/No arrest
    ## 1513 Baltimore_MD      Closed by arrest
    ## 1514 Baltimore_MD      Closed by arrest
    ## 1515 Baltimore_MD        Open/No arrest
    ## 1516 Baltimore_MD Closed without arrest
    ## 1517 Baltimore_MD      Closed by arrest
    ## 1518 Baltimore_MD Closed without arrest
    ## 1519 Baltimore_MD      Closed by arrest
    ## 1520 Baltimore_MD        Open/No arrest
    ## 1521 Baltimore_MD        Open/No arrest
    ## 1522 Baltimore_MD        Open/No arrest
    ## 1523 Baltimore_MD      Closed by arrest
    ## 1524 Baltimore_MD        Open/No arrest
    ## 1525 Baltimore_MD Closed without arrest
    ## 1526 Baltimore_MD      Closed by arrest
    ## 1527 Baltimore_MD      Closed by arrest
    ## 1528 Baltimore_MD      Closed by arrest
    ## 1529 Baltimore_MD        Open/No arrest
    ## 1530 Baltimore_MD        Open/No arrest
    ## 1531 Baltimore_MD        Open/No arrest
    ## 1532 Baltimore_MD Closed without arrest
    ## 1533 Baltimore_MD      Closed by arrest
    ## 1534 Baltimore_MD        Open/No arrest
    ## 1535 Baltimore_MD        Open/No arrest
    ## 1536 Baltimore_MD      Closed by arrest
    ## 1537 Baltimore_MD        Open/No arrest
    ## 1538 Baltimore_MD        Open/No arrest
    ## 1539 Baltimore_MD      Closed by arrest
    ## 1540 Baltimore_MD        Open/No arrest
    ## 1541 Baltimore_MD        Open/No arrest
    ## 1542 Baltimore_MD        Open/No arrest
    ## 1543 Baltimore_MD        Open/No arrest
    ## 1544 Baltimore_MD        Open/No arrest
    ## 1545 Baltimore_MD      Closed by arrest
    ## 1546 Baltimore_MD        Open/No arrest
    ## 1547 Baltimore_MD      Closed by arrest
    ## 1548 Baltimore_MD Closed without arrest
    ## 1549 Baltimore_MD      Closed by arrest
    ## 1550 Baltimore_MD        Open/No arrest
    ## 1551 Baltimore_MD        Open/No arrest
    ## 1552 Baltimore_MD      Closed by arrest
    ## 1553 Baltimore_MD        Open/No arrest
    ## 1554 Baltimore_MD      Closed by arrest
    ## 1555 Baltimore_MD        Open/No arrest
    ## 1556 Baltimore_MD        Open/No arrest
    ## 1557 Baltimore_MD        Open/No arrest
    ## 1558 Baltimore_MD        Open/No arrest
    ## 1559 Baltimore_MD        Open/No arrest
    ## 1560 Baltimore_MD      Closed by arrest
    ## 1561 Baltimore_MD        Open/No arrest
    ## 1562 Baltimore_MD        Open/No arrest
    ## 1563 Baltimore_MD        Open/No arrest
    ## 1564 Baltimore_MD        Open/No arrest
    ## 1565 Baltimore_MD      Closed by arrest
    ## 1566 Baltimore_MD      Closed by arrest
    ## 1567 Baltimore_MD      Closed by arrest
    ## 1568 Baltimore_MD        Open/No arrest
    ## 1569 Baltimore_MD Closed without arrest
    ## 1570 Baltimore_MD        Open/No arrest
    ## 1571 Baltimore_MD      Closed by arrest
    ## 1572 Baltimore_MD        Open/No arrest
    ## 1573 Baltimore_MD        Open/No arrest
    ## 1574 Baltimore_MD        Open/No arrest
    ## 1575 Baltimore_MD      Closed by arrest
    ## 1576 Baltimore_MD      Closed by arrest
    ## 1577 Baltimore_MD        Open/No arrest
    ## 1578 Baltimore_MD        Open/No arrest
    ## 1579 Baltimore_MD      Closed by arrest
    ## 1580 Baltimore_MD      Closed by arrest
    ## 1581 Baltimore_MD        Open/No arrest
    ## 1582 Baltimore_MD        Open/No arrest
    ## 1583 Baltimore_MD        Open/No arrest
    ## 1584 Baltimore_MD        Open/No arrest
    ## 1585 Baltimore_MD      Closed by arrest
    ## 1586 Baltimore_MD        Open/No arrest
    ## 1587 Baltimore_MD        Open/No arrest
    ## 1588 Baltimore_MD        Open/No arrest
    ## 1589 Baltimore_MD Closed without arrest
    ## 1590 Baltimore_MD        Open/No arrest
    ## 1591 Baltimore_MD        Open/No arrest
    ## 1592 Baltimore_MD        Open/No arrest
    ## 1593 Baltimore_MD        Open/No arrest
    ## 1594 Baltimore_MD Closed without arrest
    ## 1595 Baltimore_MD        Open/No arrest
    ## 1596 Baltimore_MD        Open/No arrest
    ## 1597 Baltimore_MD        Open/No arrest
    ## 1598 Baltimore_MD        Open/No arrest
    ## 1599 Baltimore_MD        Open/No arrest
    ## 1600 Baltimore_MD        Open/No arrest
    ## 1601 Baltimore_MD        Open/No arrest
    ## 1602 Baltimore_MD        Open/No arrest
    ## 1603 Baltimore_MD        Open/No arrest
    ## 1604 Baltimore_MD        Open/No arrest
    ## 1605 Baltimore_MD      Closed by arrest
    ## 1606 Baltimore_MD      Closed by arrest
    ## 1607 Baltimore_MD      Closed by arrest
    ## 1608 Baltimore_MD        Open/No arrest
    ## 1609 Baltimore_MD      Closed by arrest
    ## 1610 Baltimore_MD        Open/No arrest
    ## 1611 Baltimore_MD      Closed by arrest
    ## 1612 Baltimore_MD      Closed by arrest
    ## 1613 Baltimore_MD        Open/No arrest
    ## 1614 Baltimore_MD        Open/No arrest
    ## 1615 Baltimore_MD      Closed by arrest
    ## 1616 Baltimore_MD      Closed by arrest
    ## 1617 Baltimore_MD        Open/No arrest
    ## 1618 Baltimore_MD        Open/No arrest
    ## 1619 Baltimore_MD        Open/No arrest
    ## 1620 Baltimore_MD        Open/No arrest
    ## 1621 Baltimore_MD      Closed by arrest
    ## 1622 Baltimore_MD        Open/No arrest
    ## 1623 Baltimore_MD        Open/No arrest
    ## 1624 Baltimore_MD Closed without arrest
    ## 1625 Baltimore_MD Closed without arrest
    ## 1626 Baltimore_MD      Closed by arrest
    ## 1627 Baltimore_MD        Open/No arrest
    ## 1628 Baltimore_MD        Open/No arrest
    ## 1629 Baltimore_MD        Open/No arrest
    ## 1630 Baltimore_MD        Open/No arrest
    ## 1631 Baltimore_MD        Open/No arrest
    ## 1632 Baltimore_MD      Closed by arrest
    ## 1633 Baltimore_MD        Open/No arrest
    ## 1634 Baltimore_MD        Open/No arrest
    ## 1635 Baltimore_MD        Open/No arrest
    ## 1636 Baltimore_MD      Closed by arrest
    ## 1637 Baltimore_MD        Open/No arrest
    ## 1638 Baltimore_MD        Open/No arrest
    ## 1639 Baltimore_MD Closed without arrest
    ## 1640 Baltimore_MD        Open/No arrest
    ## 1641 Baltimore_MD      Closed by arrest
    ## 1642 Baltimore_MD Closed without arrest
    ## 1643 Baltimore_MD      Closed by arrest
    ## 1644 Baltimore_MD        Open/No arrest
    ## 1645 Baltimore_MD        Open/No arrest
    ## 1646 Baltimore_MD      Closed by arrest
    ## 1647 Baltimore_MD        Open/No arrest
    ## 1648 Baltimore_MD        Open/No arrest
    ## 1649 Baltimore_MD        Open/No arrest
    ## 1650 Baltimore_MD      Closed by arrest
    ## 1651 Baltimore_MD        Open/No arrest
    ## 1652 Baltimore_MD      Closed by arrest
    ## 1653 Baltimore_MD        Open/No arrest
    ## 1654 Baltimore_MD      Closed by arrest
    ## 1655 Baltimore_MD      Closed by arrest
    ## 1656 Baltimore_MD      Closed by arrest
    ## 1657 Baltimore_MD      Closed by arrest
    ## 1658 Baltimore_MD        Open/No arrest
    ## 1659 Baltimore_MD      Closed by arrest
    ## 1660 Baltimore_MD      Closed by arrest
    ## 1661 Baltimore_MD      Closed by arrest
    ## 1662 Baltimore_MD      Closed by arrest
    ## 1663 Baltimore_MD      Closed by arrest
    ## 1664 Baltimore_MD        Open/No arrest
    ## 1665 Baltimore_MD        Open/No arrest
    ## 1666 Baltimore_MD        Open/No arrest
    ## 1667 Baltimore_MD      Closed by arrest
    ## 1668 Baltimore_MD        Open/No arrest
    ## 1669 Baltimore_MD      Closed by arrest
    ## 1670 Baltimore_MD      Closed by arrest
    ## 1671 Baltimore_MD        Open/No arrest
    ## 1672 Baltimore_MD        Open/No arrest
    ## 1673 Baltimore_MD      Closed by arrest
    ## 1674 Baltimore_MD      Closed by arrest
    ## 1675 Baltimore_MD        Open/No arrest
    ## 1676 Baltimore_MD      Closed by arrest
    ## 1677 Baltimore_MD      Closed by arrest
    ## 1678 Baltimore_MD        Open/No arrest
    ## 1679 Baltimore_MD        Open/No arrest
    ## 1680 Baltimore_MD      Closed by arrest
    ## 1681 Baltimore_MD        Open/No arrest
    ## 1682 Baltimore_MD        Open/No arrest
    ## 1683 Baltimore_MD      Closed by arrest
    ## 1684 Baltimore_MD        Open/No arrest
    ## 1685 Baltimore_MD        Open/No arrest
    ## 1686 Baltimore_MD      Closed by arrest
    ## 1687 Baltimore_MD        Open/No arrest
    ## 1688 Baltimore_MD      Closed by arrest
    ## 1689 Baltimore_MD      Closed by arrest
    ## 1690 Baltimore_MD      Closed by arrest
    ## 1691 Baltimore_MD      Closed by arrest
    ## 1692 Baltimore_MD        Open/No arrest
    ## 1693 Baltimore_MD        Open/No arrest
    ## 1694 Baltimore_MD        Open/No arrest
    ## 1695 Baltimore_MD        Open/No arrest
    ## 1696 Baltimore_MD        Open/No arrest
    ## 1697 Baltimore_MD        Open/No arrest
    ## 1698 Baltimore_MD        Open/No arrest
    ## 1699 Baltimore_MD        Open/No arrest
    ## 1700 Baltimore_MD        Open/No arrest
    ## 1701 Baltimore_MD Closed without arrest
    ## 1702 Baltimore_MD        Open/No arrest
    ## 1703 Baltimore_MD        Open/No arrest
    ## 1704 Baltimore_MD      Closed by arrest
    ## 1705 Baltimore_MD      Closed by arrest
    ## 1706 Baltimore_MD        Open/No arrest
    ## 1707 Baltimore_MD        Open/No arrest
    ## 1708 Baltimore_MD Closed without arrest
    ## 1709 Baltimore_MD      Closed by arrest
    ## 1710 Baltimore_MD        Open/No arrest
    ## 1711 Baltimore_MD        Open/No arrest
    ## 1712 Baltimore_MD        Open/No arrest
    ## 1713 Baltimore_MD      Closed by arrest
    ## 1714 Baltimore_MD      Closed by arrest
    ## 1715 Baltimore_MD      Closed by arrest
    ## 1716 Baltimore_MD        Open/No arrest
    ## 1717 Baltimore_MD Closed without arrest
    ## 1718 Baltimore_MD      Closed by arrest
    ## 1719 Baltimore_MD        Open/No arrest
    ## 1720 Baltimore_MD        Open/No arrest
    ## 1721 Baltimore_MD      Closed by arrest
    ## 1722 Baltimore_MD      Closed by arrest
    ## 1723 Baltimore_MD      Closed by arrest
    ## 1724 Baltimore_MD Closed without arrest
    ## 1725 Baltimore_MD        Open/No arrest
    ## 1726 Baltimore_MD        Open/No arrest
    ## 1727 Baltimore_MD Closed without arrest
    ## 1728 Baltimore_MD      Closed by arrest
    ## 1729 Baltimore_MD        Open/No arrest
    ## 1730 Baltimore_MD        Open/No arrest
    ## 1731 Baltimore_MD        Open/No arrest
    ## 1732 Baltimore_MD      Closed by arrest
    ## 1733 Baltimore_MD        Open/No arrest
    ## 1734 Baltimore_MD        Open/No arrest
    ## 1735 Baltimore_MD        Open/No arrest
    ## 1736 Baltimore_MD        Open/No arrest
    ## 1737 Baltimore_MD      Closed by arrest
    ## 1738 Baltimore_MD      Closed by arrest
    ## 1739 Baltimore_MD        Open/No arrest
    ## 1740 Baltimore_MD        Open/No arrest
    ## 1741 Baltimore_MD      Closed by arrest
    ## 1742 Baltimore_MD        Open/No arrest
    ## 1743 Baltimore_MD      Closed by arrest
    ## 1744 Baltimore_MD        Open/No arrest
    ## 1745 Baltimore_MD        Open/No arrest
    ## 1746 Baltimore_MD        Open/No arrest
    ## 1747 Baltimore_MD      Closed by arrest
    ## 1748 Baltimore_MD        Open/No arrest
    ## 1749 Baltimore_MD        Open/No arrest
    ## 1750 Baltimore_MD        Open/No arrest
    ## 1751 Baltimore_MD        Open/No arrest
    ## 1752 Baltimore_MD      Closed by arrest
    ## 1753 Baltimore_MD      Closed by arrest
    ## 1754 Baltimore_MD        Open/No arrest
    ## 1755 Baltimore_MD        Open/No arrest
    ## 1756 Baltimore_MD      Closed by arrest
    ## 1757 Baltimore_MD      Closed by arrest
    ## 1758 Baltimore_MD      Closed by arrest
    ## 1759 Baltimore_MD        Open/No arrest
    ## 1760 Baltimore_MD      Closed by arrest
    ## 1761 Baltimore_MD        Open/No arrest
    ## 1762 Baltimore_MD        Open/No arrest
    ## 1763 Baltimore_MD      Closed by arrest
    ## 1764 Baltimore_MD        Open/No arrest
    ## 1765 Baltimore_MD      Closed by arrest
    ## 1766 Baltimore_MD      Closed by arrest
    ## 1767 Baltimore_MD      Closed by arrest
    ## 1768 Baltimore_MD        Open/No arrest
    ## 1769 Baltimore_MD        Open/No arrest
    ## 1770 Baltimore_MD      Closed by arrest
    ## 1771 Baltimore_MD      Closed by arrest
    ## 1772 Baltimore_MD        Open/No arrest
    ## 1773 Baltimore_MD      Closed by arrest
    ## 1774 Baltimore_MD        Open/No arrest
    ## 1775 Baltimore_MD        Open/No arrest
    ## 1776 Baltimore_MD      Closed by arrest
    ## 1777 Baltimore_MD      Closed by arrest
    ## 1778 Baltimore_MD        Open/No arrest
    ## 1779 Baltimore_MD        Open/No arrest
    ## 1780 Baltimore_MD      Closed by arrest
    ## 1781 Baltimore_MD      Closed by arrest
    ## 1782 Baltimore_MD        Open/No arrest
    ## 1783 Baltimore_MD        Open/No arrest
    ## 1784 Baltimore_MD        Open/No arrest
    ## 1785 Baltimore_MD Closed without arrest
    ## 1786 Baltimore_MD        Open/No arrest
    ## 1787 Baltimore_MD      Closed by arrest
    ## 1788 Baltimore_MD        Open/No arrest
    ## 1789 Baltimore_MD      Closed by arrest
    ## 1790 Baltimore_MD      Closed by arrest
    ## 1791 Baltimore_MD      Closed by arrest
    ## 1792 Baltimore_MD        Open/No arrest
    ## 1793 Baltimore_MD        Open/No arrest
    ## 1794 Baltimore_MD        Open/No arrest
    ## 1795 Baltimore_MD      Closed by arrest
    ## 1796 Baltimore_MD      Closed by arrest
    ## 1797 Baltimore_MD        Open/No arrest
    ## 1798 Baltimore_MD        Open/No arrest
    ## 1799 Baltimore_MD        Open/No arrest
    ## 1800 Baltimore_MD Closed without arrest
    ## 1801 Baltimore_MD      Closed by arrest
    ## 1802 Baltimore_MD        Open/No arrest
    ## 1803 Baltimore_MD        Open/No arrest
    ## 1804 Baltimore_MD      Closed by arrest
    ## 1805 Baltimore_MD        Open/No arrest
    ## 1806 Baltimore_MD Closed without arrest
    ## 1807 Baltimore_MD      Closed by arrest
    ## 1808 Baltimore_MD      Closed by arrest
    ## 1809 Baltimore_MD        Open/No arrest
    ## 1810 Baltimore_MD      Closed by arrest
    ## 1811 Baltimore_MD      Closed by arrest
    ## 1812 Baltimore_MD      Closed by arrest
    ## 1813 Baltimore_MD        Open/No arrest
    ## 1814 Baltimore_MD      Closed by arrest
    ## 1815 Baltimore_MD      Closed by arrest
    ## 1816 Baltimore_MD Closed without arrest
    ## 1817 Baltimore_MD        Open/No arrest
    ## 1818 Baltimore_MD        Open/No arrest
    ## 1819 Baltimore_MD        Open/No arrest
    ## 1820 Baltimore_MD      Closed by arrest
    ## 1821 Baltimore_MD        Open/No arrest
    ## 1822 Baltimore_MD      Closed by arrest
    ## 1823 Baltimore_MD        Open/No arrest
    ## 1824 Baltimore_MD      Closed by arrest
    ## 1825 Baltimore_MD        Open/No arrest
    ## 1826 Baltimore_MD        Open/No arrest
    ## 1827 Baltimore_MD      Closed by arrest
    ## 1828 Baltimore_MD        Open/No arrest
    ## 1829 Baltimore_MD Closed without arrest
    ## 1830 Baltimore_MD        Open/No arrest
    ## 1831 Baltimore_MD        Open/No arrest
    ## 1832 Baltimore_MD      Closed by arrest
    ## 1833 Baltimore_MD      Closed by arrest
    ## 1834 Baltimore_MD      Closed by arrest
    ## 1835 Baltimore_MD Closed without arrest
    ## 1836 Baltimore_MD        Open/No arrest
    ## 1837 Baltimore_MD        Open/No arrest
    ## 1838 Baltimore_MD        Open/No arrest
    ## 1839 Baltimore_MD      Closed by arrest
    ## 1840 Baltimore_MD      Closed by arrest
    ## 1841 Baltimore_MD        Open/No arrest
    ## 1842 Baltimore_MD      Closed by arrest
    ## 1843 Baltimore_MD      Closed by arrest
    ## 1844 Baltimore_MD        Open/No arrest
    ## 1845 Baltimore_MD      Closed by arrest
    ## 1846 Baltimore_MD        Open/No arrest
    ## 1847 Baltimore_MD        Open/No arrest
    ## 1848 Baltimore_MD        Open/No arrest
    ## 1849 Baltimore_MD      Closed by arrest
    ## 1850 Baltimore_MD        Open/No arrest
    ## 1851 Baltimore_MD        Open/No arrest
    ## 1852 Baltimore_MD        Open/No arrest
    ## 1853 Baltimore_MD        Open/No arrest
    ## 1854 Baltimore_MD        Open/No arrest
    ## 1855 Baltimore_MD        Open/No arrest
    ## 1856 Baltimore_MD      Closed by arrest
    ## 1857 Baltimore_MD        Open/No arrest
    ## 1858 Baltimore_MD      Closed by arrest
    ## 1859 Baltimore_MD        Open/No arrest
    ## 1860 Baltimore_MD        Open/No arrest
    ## 1861 Baltimore_MD        Open/No arrest
    ## 1862 Baltimore_MD      Closed by arrest
    ## 1863 Baltimore_MD        Open/No arrest
    ## 1864 Baltimore_MD        Open/No arrest
    ## 1865 Baltimore_MD        Open/No arrest
    ## 1866 Baltimore_MD        Open/No arrest
    ## 1867 Baltimore_MD        Open/No arrest
    ## 1868 Baltimore_MD        Open/No arrest
    ## 1869 Baltimore_MD        Open/No arrest
    ## 1870 Baltimore_MD      Closed by arrest
    ## 1871 Baltimore_MD        Open/No arrest
    ## 1872 Baltimore_MD      Closed by arrest
    ## 1873 Baltimore_MD      Closed by arrest
    ## 1874 Baltimore_MD        Open/No arrest
    ## 1875 Baltimore_MD        Open/No arrest
    ## 1876 Baltimore_MD      Closed by arrest
    ## 1877 Baltimore_MD        Open/No arrest
    ## 1878 Baltimore_MD      Closed by arrest
    ## 1879 Baltimore_MD        Open/No arrest
    ## 1880 Baltimore_MD        Open/No arrest
    ## 1881 Baltimore_MD      Closed by arrest
    ## 1882 Baltimore_MD        Open/No arrest
    ## 1883 Baltimore_MD        Open/No arrest
    ## 1884 Baltimore_MD        Open/No arrest
    ## 1885 Baltimore_MD        Open/No arrest
    ## 1886 Baltimore_MD        Open/No arrest
    ## 1887 Baltimore_MD        Open/No arrest
    ## 1888 Baltimore_MD Closed without arrest
    ## 1889 Baltimore_MD      Closed by arrest
    ## 1890 Baltimore_MD        Open/No arrest
    ## 1891 Baltimore_MD      Closed by arrest
    ## 1892 Baltimore_MD      Closed by arrest
    ## 1893 Baltimore_MD        Open/No arrest
    ## 1894 Baltimore_MD        Open/No arrest
    ## 1895 Baltimore_MD        Open/No arrest
    ## 1896 Baltimore_MD        Open/No arrest
    ## 1897 Baltimore_MD        Open/No arrest
    ## 1898 Baltimore_MD        Open/No arrest
    ## 1899 Baltimore_MD        Open/No arrest
    ## 1900 Baltimore_MD      Closed by arrest
    ## 1901 Baltimore_MD      Closed by arrest
    ## 1902 Baltimore_MD        Open/No arrest
    ## 1903 Baltimore_MD        Open/No arrest
    ## 1904 Baltimore_MD        Open/No arrest
    ## 1905 Baltimore_MD        Open/No arrest
    ## 1906 Baltimore_MD        Open/No arrest
    ## 1907 Baltimore_MD Closed without arrest
    ## 1908 Baltimore_MD        Open/No arrest
    ## 1909 Baltimore_MD        Open/No arrest
    ## 1910 Baltimore_MD        Open/No arrest
    ## 1911 Baltimore_MD      Closed by arrest
    ## 1912 Baltimore_MD      Closed by arrest
    ## 1913 Baltimore_MD        Open/No arrest
    ## 1914 Baltimore_MD        Open/No arrest
    ## 1915 Baltimore_MD      Closed by arrest
    ## 1916 Baltimore_MD        Open/No arrest
    ## 1917 Baltimore_MD        Open/No arrest
    ## 1918 Baltimore_MD        Open/No arrest
    ## 1919 Baltimore_MD        Open/No arrest
    ## 1920 Baltimore_MD        Open/No arrest
    ## 1921 Baltimore_MD      Closed by arrest
    ## 1922 Baltimore_MD        Open/No arrest
    ## 1923 Baltimore_MD        Open/No arrest
    ## 1924 Baltimore_MD        Open/No arrest
    ## 1925 Baltimore_MD      Closed by arrest
    ## 1926 Baltimore_MD      Closed by arrest
    ## 1927 Baltimore_MD        Open/No arrest
    ## 1928 Baltimore_MD        Open/No arrest
    ## 1929 Baltimore_MD        Open/No arrest
    ## 1930 Baltimore_MD        Open/No arrest
    ## 1931 Baltimore_MD        Open/No arrest
    ## 1932 Baltimore_MD Closed without arrest
    ## 1933 Baltimore_MD        Open/No arrest
    ## 1934 Baltimore_MD        Open/No arrest
    ## 1935 Baltimore_MD      Closed by arrest
    ## 1936 Baltimore_MD      Closed by arrest
    ## 1937 Baltimore_MD        Open/No arrest
    ## 1938 Baltimore_MD        Open/No arrest
    ## 1939 Baltimore_MD        Open/No arrest
    ## 1940 Baltimore_MD      Closed by arrest
    ## 1941 Baltimore_MD        Open/No arrest
    ## 1942 Baltimore_MD        Open/No arrest
    ## 1943 Baltimore_MD      Closed by arrest
    ## 1944 Baltimore_MD      Closed by arrest
    ## 1945 Baltimore_MD        Open/No arrest
    ## 1946 Baltimore_MD        Open/No arrest
    ## 1947 Baltimore_MD      Closed by arrest
    ## 1948 Baltimore_MD      Closed by arrest
    ## 1949 Baltimore_MD        Open/No arrest
    ## 1950 Baltimore_MD        Open/No arrest
    ## 1951 Baltimore_MD        Open/No arrest
    ## 1952 Baltimore_MD        Open/No arrest
    ## 1953 Baltimore_MD        Open/No arrest
    ## 1954 Baltimore_MD        Open/No arrest
    ## 1955 Baltimore_MD        Open/No arrest
    ## 1956 Baltimore_MD      Closed by arrest
    ## 1957 Baltimore_MD        Open/No arrest
    ## 1958 Baltimore_MD      Closed by arrest
    ## 1959 Baltimore_MD        Open/No arrest
    ## 1960 Baltimore_MD        Open/No arrest
    ## 1961 Baltimore_MD      Closed by arrest
    ## 1962 Baltimore_MD      Closed by arrest
    ## 1963 Baltimore_MD        Open/No arrest
    ## 1964 Baltimore_MD        Open/No arrest
    ## 1965 Baltimore_MD        Open/No arrest
    ## 1966 Baltimore_MD        Open/No arrest
    ## 1967 Baltimore_MD        Open/No arrest
    ## 1968 Baltimore_MD        Open/No arrest
    ## 1969 Baltimore_MD      Closed by arrest
    ## 1970 Baltimore_MD      Closed by arrest
    ## 1971 Baltimore_MD      Closed by arrest
    ## 1972 Baltimore_MD        Open/No arrest
    ## 1973 Baltimore_MD        Open/No arrest
    ## 1974 Baltimore_MD      Closed by arrest
    ## 1975 Baltimore_MD        Open/No arrest
    ## 1976 Baltimore_MD        Open/No arrest
    ## 1977 Baltimore_MD        Open/No arrest
    ## 1978 Baltimore_MD        Open/No arrest
    ## 1979 Baltimore_MD      Closed by arrest
    ## 1980 Baltimore_MD        Open/No arrest
    ## 1981 Baltimore_MD        Open/No arrest
    ## 1982 Baltimore_MD        Open/No arrest
    ## 1983 Baltimore_MD        Open/No arrest
    ## 1984 Baltimore_MD      Closed by arrest
    ## 1985 Baltimore_MD        Open/No arrest
    ## 1986 Baltimore_MD        Open/No arrest
    ## 1987 Baltimore_MD        Open/No arrest
    ## 1988 Baltimore_MD        Open/No arrest
    ## 1989 Baltimore_MD        Open/No arrest
    ## 1990 Baltimore_MD      Closed by arrest
    ## 1991 Baltimore_MD        Open/No arrest
    ## 1992 Baltimore_MD        Open/No arrest
    ## 1993 Baltimore_MD        Open/No arrest
    ## 1994 Baltimore_MD        Open/No arrest
    ## 1995 Baltimore_MD      Closed by arrest
    ## 1996 Baltimore_MD        Open/No arrest
    ## 1997 Baltimore_MD        Open/No arrest
    ## 1998 Baltimore_MD Closed without arrest
    ## 1999 Baltimore_MD        Open/No arrest
    ## 2000 Baltimore_MD      Closed by arrest
    ## 2001 Baltimore_MD      Closed by arrest
    ## 2002 Baltimore_MD        Open/No arrest
    ## 2003 Baltimore_MD        Open/No arrest
    ## 2004 Baltimore_MD      Closed by arrest
    ## 2005 Baltimore_MD        Open/No arrest
    ## 2006 Baltimore_MD      Closed by arrest
    ## 2007 Baltimore_MD        Open/No arrest
    ## 2008 Baltimore_MD        Open/No arrest
    ## 2009 Baltimore_MD        Open/No arrest
    ## 2010 Baltimore_MD        Open/No arrest
    ## 2011 Baltimore_MD Closed without arrest
    ## 2012 Baltimore_MD        Open/No arrest
    ## 2013 Baltimore_MD        Open/No arrest
    ## 2014 Baltimore_MD        Open/No arrest
    ## 2015 Baltimore_MD        Open/No arrest
    ## 2016 Baltimore_MD        Open/No arrest
    ## 2017 Baltimore_MD        Open/No arrest
    ## 2018 Baltimore_MD        Open/No arrest
    ## 2019 Baltimore_MD        Open/No arrest
    ## 2020 Baltimore_MD      Closed by arrest
    ## 2021 Baltimore_MD        Open/No arrest
    ## 2022 Baltimore_MD        Open/No arrest
    ## 2023 Baltimore_MD      Closed by arrest
    ## 2024 Baltimore_MD        Open/No arrest
    ## 2025 Baltimore_MD        Open/No arrest
    ## 2026 Baltimore_MD        Open/No arrest
    ## 2027 Baltimore_MD        Open/No arrest
    ## 2028 Baltimore_MD        Open/No arrest
    ## 2029 Baltimore_MD        Open/No arrest
    ## 2030 Baltimore_MD        Open/No arrest
    ## 2031 Baltimore_MD      Closed by arrest
    ## 2032 Baltimore_MD        Open/No arrest
    ## 2033 Baltimore_MD      Closed by arrest
    ## 2034 Baltimore_MD Closed without arrest
    ## 2035 Baltimore_MD        Open/No arrest
    ## 2036 Baltimore_MD        Open/No arrest
    ## 2037 Baltimore_MD      Closed by arrest
    ## 2038 Baltimore_MD      Closed by arrest
    ## 2039 Baltimore_MD        Open/No arrest
    ## 2040 Baltimore_MD      Closed by arrest
    ## 2041 Baltimore_MD        Open/No arrest
    ## 2042 Baltimore_MD        Open/No arrest
    ## 2043 Baltimore_MD        Open/No arrest
    ## 2044 Baltimore_MD      Closed by arrest
    ## 2045 Baltimore_MD        Open/No arrest
    ## 2046 Baltimore_MD        Open/No arrest
    ## 2047 Baltimore_MD      Closed by arrest
    ## 2048 Baltimore_MD        Open/No arrest
    ## 2049 Baltimore_MD        Open/No arrest
    ## 2050 Baltimore_MD        Open/No arrest
    ## 2051 Baltimore_MD      Closed by arrest
    ## 2052 Baltimore_MD        Open/No arrest
    ## 2053 Baltimore_MD        Open/No arrest
    ## 2054 Baltimore_MD        Open/No arrest
    ## 2055 Baltimore_MD        Open/No arrest
    ## 2056 Baltimore_MD        Open/No arrest
    ## 2057 Baltimore_MD        Open/No arrest
    ## 2058 Baltimore_MD        Open/No arrest
    ## 2059 Baltimore_MD        Open/No arrest
    ## 2060 Baltimore_MD        Open/No arrest
    ## 2061 Baltimore_MD        Open/No arrest
    ## 2062 Baltimore_MD        Open/No arrest
    ## 2063 Baltimore_MD        Open/No arrest
    ## 2064 Baltimore_MD        Open/No arrest
    ## 2065 Baltimore_MD        Open/No arrest
    ## 2066 Baltimore_MD Closed without arrest
    ## 2067 Baltimore_MD      Closed by arrest
    ## 2068 Baltimore_MD        Open/No arrest
    ## 2069 Baltimore_MD      Closed by arrest
    ## 2070 Baltimore_MD        Open/No arrest
    ## 2071 Baltimore_MD        Open/No arrest
    ## 2072 Baltimore_MD      Closed by arrest
    ## 2073 Baltimore_MD        Open/No arrest
    ## 2074 Baltimore_MD      Closed by arrest
    ## 2075 Baltimore_MD        Open/No arrest
    ## 2076 Baltimore_MD        Open/No arrest
    ## 2077 Baltimore_MD      Closed by arrest
    ## 2078 Baltimore_MD        Open/No arrest
    ## 2079 Baltimore_MD        Open/No arrest
    ## 2080 Baltimore_MD      Closed by arrest
    ## 2081 Baltimore_MD        Open/No arrest
    ## 2082 Baltimore_MD Closed without arrest
    ## 2083 Baltimore_MD        Open/No arrest
    ## 2084 Baltimore_MD        Open/No arrest
    ## 2085 Baltimore_MD        Open/No arrest
    ## 2086 Baltimore_MD        Open/No arrest
    ## 2087 Baltimore_MD        Open/No arrest
    ## 2088 Baltimore_MD      Closed by arrest
    ## 2089 Baltimore_MD      Closed by arrest
    ## 2090 Baltimore_MD      Closed by arrest
    ## 2091 Baltimore_MD        Open/No arrest
    ## 2092 Baltimore_MD        Open/No arrest
    ## 2093 Baltimore_MD        Open/No arrest
    ## 2094 Baltimore_MD        Open/No arrest
    ## 2095 Baltimore_MD        Open/No arrest
    ## 2096 Baltimore_MD        Open/No arrest
    ## 2097 Baltimore_MD        Open/No arrest
    ## 2098 Baltimore_MD        Open/No arrest
    ## 2099 Baltimore_MD        Open/No arrest
    ## 2100 Baltimore_MD        Open/No arrest
    ## 2101 Baltimore_MD        Open/No arrest
    ## 2102 Baltimore_MD        Open/No arrest
    ## 2103 Baltimore_MD        Open/No arrest
    ## 2104 Baltimore_MD      Closed by arrest
    ## 2105 Baltimore_MD        Open/No arrest
    ## 2106 Baltimore_MD Closed without arrest
    ## 2107 Baltimore_MD      Closed by arrest
    ## 2108 Baltimore_MD        Open/No arrest
    ## 2109 Baltimore_MD        Open/No arrest
    ## 2110 Baltimore_MD      Closed by arrest
    ## 2111 Baltimore_MD      Closed by arrest
    ## 2112 Baltimore_MD      Closed by arrest
    ## 2113 Baltimore_MD        Open/No arrest
    ## 2114 Baltimore_MD        Open/No arrest
    ## 2115 Baltimore_MD        Open/No arrest
    ## 2116 Baltimore_MD        Open/No arrest
    ## 2117 Baltimore_MD        Open/No arrest
    ## 2118 Baltimore_MD      Closed by arrest
    ## 2119 Baltimore_MD Closed without arrest
    ## 2120 Baltimore_MD        Open/No arrest
    ## 2121 Baltimore_MD        Open/No arrest
    ## 2122 Baltimore_MD        Open/No arrest
    ## 2123 Baltimore_MD      Closed by arrest
    ## 2124 Baltimore_MD        Open/No arrest
    ## 2125 Baltimore_MD        Open/No arrest
    ## 2126 Baltimore_MD        Open/No arrest
    ## 2127 Baltimore_MD        Open/No arrest
    ## 2128 Baltimore_MD        Open/No arrest
    ## 2129 Baltimore_MD        Open/No arrest
    ## 2130 Baltimore_MD        Open/No arrest
    ## 2131 Baltimore_MD        Open/No arrest
    ## 2132 Baltimore_MD        Open/No arrest
    ## 2133 Baltimore_MD        Open/No arrest
    ## 2134 Baltimore_MD        Open/No arrest
    ## 2135 Baltimore_MD      Closed by arrest
    ## 2136 Baltimore_MD        Open/No arrest
    ## 2137 Baltimore_MD        Open/No arrest
    ## 2138 Baltimore_MD        Open/No arrest
    ## 2139 Baltimore_MD        Open/No arrest
    ## 2140 Baltimore_MD        Open/No arrest
    ## 2141 Baltimore_MD        Open/No arrest
    ## 2142 Baltimore_MD        Open/No arrest
    ## 2143 Baltimore_MD        Open/No arrest
    ## 2144 Baltimore_MD        Open/No arrest
    ## 2145 Baltimore_MD        Open/No arrest
    ## 2146 Baltimore_MD        Open/No arrest
    ## 2147 Baltimore_MD        Open/No arrest
    ## 2148 Baltimore_MD        Open/No arrest
    ## 2149 Baltimore_MD        Open/No arrest
    ## 2150 Baltimore_MD      Closed by arrest
    ## 2151 Baltimore_MD      Closed by arrest
    ## 2152 Baltimore_MD      Closed by arrest
    ## 2153 Baltimore_MD      Closed by arrest
    ## 2154 Baltimore_MD        Open/No arrest
    ## 2155 Baltimore_MD Closed without arrest
    ## 2156 Baltimore_MD Closed without arrest
    ## 2157 Baltimore_MD        Open/No arrest
    ## 2158 Baltimore_MD      Closed by arrest
    ## 2159 Baltimore_MD        Open/No arrest
    ## 2160 Baltimore_MD        Open/No arrest
    ## 2161 Baltimore_MD        Open/No arrest
    ## 2162 Baltimore_MD      Closed by arrest
    ## 2163 Baltimore_MD        Open/No arrest
    ## 2164 Baltimore_MD      Closed by arrest
    ## 2165 Baltimore_MD        Open/No arrest
    ## 2166 Baltimore_MD        Open/No arrest
    ## 2167 Baltimore_MD        Open/No arrest
    ## 2168 Baltimore_MD      Closed by arrest
    ## 2169 Baltimore_MD      Closed by arrest
    ## 2170 Baltimore_MD        Open/No arrest
    ## 2171 Baltimore_MD        Open/No arrest
    ## 2172 Baltimore_MD        Open/No arrest
    ## 2173 Baltimore_MD      Closed by arrest
    ## 2174 Baltimore_MD      Closed by arrest
    ## 2175 Baltimore_MD      Closed by arrest
    ## 2176 Baltimore_MD        Open/No arrest
    ## 2177 Baltimore_MD        Open/No arrest
    ## 2178 Baltimore_MD        Open/No arrest
    ## 2179 Baltimore_MD      Closed by arrest
    ## 2180 Baltimore_MD        Open/No arrest
    ## 2181 Baltimore_MD        Open/No arrest
    ## 2182 Baltimore_MD      Closed by arrest
    ## 2183 Baltimore_MD        Open/No arrest
    ## 2184 Baltimore_MD        Open/No arrest
    ## 2185 Baltimore_MD        Open/No arrest
    ## 2186 Baltimore_MD        Open/No arrest
    ## 2187 Baltimore_MD        Open/No arrest
    ## 2188 Baltimore_MD      Closed by arrest
    ## 2189 Baltimore_MD        Open/No arrest
    ## 2190 Baltimore_MD        Open/No arrest
    ## 2191 Baltimore_MD        Open/No arrest
    ## 2192 Baltimore_MD        Open/No arrest
    ## 2193 Baltimore_MD        Open/No arrest
    ## 2194 Baltimore_MD        Open/No arrest
    ## 2195 Baltimore_MD      Closed by arrest
    ## 2196 Baltimore_MD      Closed by arrest
    ## 2197 Baltimore_MD      Closed by arrest
    ## 2198 Baltimore_MD      Closed by arrest
    ## 2199 Baltimore_MD        Open/No arrest
    ## 2200 Baltimore_MD        Open/No arrest
    ## 2201 Baltimore_MD        Open/No arrest
    ## 2202 Baltimore_MD      Closed by arrest
    ## 2203 Baltimore_MD        Open/No arrest
    ## 2204 Baltimore_MD        Open/No arrest
    ## 2205 Baltimore_MD        Open/No arrest
    ## 2206 Baltimore_MD      Closed by arrest
    ## 2207 Baltimore_MD        Open/No arrest
    ## 2208 Baltimore_MD        Open/No arrest
    ## 2209 Baltimore_MD        Open/No arrest
    ## 2210 Baltimore_MD        Open/No arrest
    ## 2211 Baltimore_MD      Closed by arrest
    ## 2212 Baltimore_MD        Open/No arrest
    ## 2213 Baltimore_MD        Open/No arrest
    ## 2214 Baltimore_MD        Open/No arrest
    ## 2215 Baltimore_MD        Open/No arrest
    ## 2216 Baltimore_MD        Open/No arrest
    ## 2217 Baltimore_MD        Open/No arrest
    ## 2218 Baltimore_MD        Open/No arrest
    ## 2219 Baltimore_MD        Open/No arrest
    ## 2220 Baltimore_MD        Open/No arrest
    ## 2221 Baltimore_MD        Open/No arrest
    ## 2222 Baltimore_MD        Open/No arrest
    ## 2223 Baltimore_MD        Open/No arrest
    ## 2224 Baltimore_MD        Open/No arrest
    ## 2225 Baltimore_MD        Open/No arrest
    ## 2226 Baltimore_MD      Closed by arrest
    ## 2227 Baltimore_MD        Open/No arrest
    ## 2228 Baltimore_MD      Closed by arrest
    ## 2229 Baltimore_MD        Open/No arrest
    ## 2230 Baltimore_MD        Open/No arrest
    ## 2231 Baltimore_MD        Open/No arrest
    ## 2232 Baltimore_MD        Open/No arrest
    ## 2233 Baltimore_MD        Open/No arrest
    ## 2234 Baltimore_MD        Open/No arrest
    ## 2235 Baltimore_MD      Closed by arrest
    ## 2236 Baltimore_MD        Open/No arrest
    ## 2237 Baltimore_MD        Open/No arrest
    ## 2238 Baltimore_MD        Open/No arrest
    ## 2239 Baltimore_MD      Closed by arrest
    ## 2240 Baltimore_MD        Open/No arrest
    ## 2241 Baltimore_MD        Open/No arrest
    ## 2242 Baltimore_MD        Open/No arrest
    ## 2243 Baltimore_MD        Open/No arrest
    ## 2244 Baltimore_MD        Open/No arrest
    ## 2245 Baltimore_MD        Open/No arrest
    ## 2246 Baltimore_MD        Open/No arrest
    ## 2247 Baltimore_MD        Open/No arrest
    ## 2248 Baltimore_MD        Open/No arrest
    ## 2249 Baltimore_MD        Open/No arrest
    ## 2250 Baltimore_MD        Open/No arrest
    ## 2251 Baltimore_MD        Open/No arrest
    ## 2252 Baltimore_MD        Open/No arrest
    ## 2253 Baltimore_MD      Closed by arrest
    ## 2254 Baltimore_MD        Open/No arrest
    ## 2255 Baltimore_MD        Open/No arrest
    ## 2256 Baltimore_MD        Open/No arrest
    ## 2257 Baltimore_MD        Open/No arrest
    ## 2258 Baltimore_MD        Open/No arrest
    ## 2259 Baltimore_MD        Open/No arrest
    ## 2260 Baltimore_MD        Open/No arrest
    ## 2261 Baltimore_MD        Open/No arrest
    ## 2262 Baltimore_MD        Open/No arrest
    ## 2263 Baltimore_MD      Closed by arrest
    ## 2264 Baltimore_MD        Open/No arrest
    ## 2265 Baltimore_MD        Open/No arrest
    ## 2266 Baltimore_MD        Open/No arrest
    ## 2267 Baltimore_MD        Open/No arrest
    ## 2268 Baltimore_MD        Open/No arrest
    ## 2269 Baltimore_MD      Closed by arrest
    ## 2270 Baltimore_MD      Closed by arrest
    ## 2271 Baltimore_MD        Open/No arrest
    ## 2272 Baltimore_MD        Open/No arrest
    ## 2273 Baltimore_MD        Open/No arrest
    ## 2274 Baltimore_MD      Closed by arrest
    ## 2275 Baltimore_MD        Open/No arrest
    ## 2276 Baltimore_MD        Open/No arrest
    ## 2277 Baltimore_MD        Open/No arrest
    ## 2278 Baltimore_MD        Open/No arrest
    ## 2279 Baltimore_MD      Closed by arrest
    ## 2280 Baltimore_MD        Open/No arrest
    ## 2281 Baltimore_MD        Open/No arrest
    ## 2282 Baltimore_MD        Open/No arrest
    ## 2283 Baltimore_MD      Closed by arrest
    ## 2284 Baltimore_MD        Open/No arrest
    ## 2285 Baltimore_MD        Open/No arrest
    ## 2286 Baltimore_MD        Open/No arrest
    ## 2287 Baltimore_MD        Open/No arrest
    ## 2288 Baltimore_MD        Open/No arrest
    ## 2289 Baltimore_MD      Closed by arrest
    ## 2290 Baltimore_MD        Open/No arrest
    ## 2291 Baltimore_MD Closed without arrest
    ## 2292 Baltimore_MD        Open/No arrest
    ## 2293 Baltimore_MD        Open/No arrest
    ## 2294 Baltimore_MD        Open/No arrest
    ## 2295 Baltimore_MD        Open/No arrest
    ## 2296 Baltimore_MD        Open/No arrest
    ## 2297 Baltimore_MD        Open/No arrest
    ## 2298 Baltimore_MD        Open/No arrest
    ## 2299 Baltimore_MD        Open/No arrest
    ## 2300 Baltimore_MD        Open/No arrest
    ## 2301 Baltimore_MD        Open/No arrest
    ## 2302 Baltimore_MD      Closed by arrest
    ## 2303 Baltimore_MD        Open/No arrest
    ## 2304 Baltimore_MD        Open/No arrest
    ## 2305 Baltimore_MD      Closed by arrest
    ## 2306 Baltimore_MD Closed without arrest
    ## 2307 Baltimore_MD        Open/No arrest
    ## 2308 Baltimore_MD      Closed by arrest
    ## 2309 Baltimore_MD        Open/No arrest
    ## 2310 Baltimore_MD        Open/No arrest
    ## 2311 Baltimore_MD        Open/No arrest
    ## 2312 Baltimore_MD        Open/No arrest
    ## 2313 Baltimore_MD        Open/No arrest
    ## 2314 Baltimore_MD        Open/No arrest
    ## 2315 Baltimore_MD      Closed by arrest
    ## 2316 Baltimore_MD        Open/No arrest
    ## 2317 Baltimore_MD      Closed by arrest
    ## 2318 Baltimore_MD        Open/No arrest
    ## 2319 Baltimore_MD        Open/No arrest
    ## 2320 Baltimore_MD        Open/No arrest
    ## 2321 Baltimore_MD        Open/No arrest
    ## 2322 Baltimore_MD        Open/No arrest
    ## 2323 Baltimore_MD        Open/No arrest
    ## 2324 Baltimore_MD        Open/No arrest
    ## 2325 Baltimore_MD        Open/No arrest
    ## 2326 Baltimore_MD        Open/No arrest
    ## 2327 Baltimore_MD        Open/No arrest
    ## 2328 Baltimore_MD        Open/No arrest
    ## 2329 Baltimore_MD        Open/No arrest
    ## 2330 Baltimore_MD        Open/No arrest
    ## 2331 Baltimore_MD        Open/No arrest
    ## 2332 Baltimore_MD      Closed by arrest
    ## 2333 Baltimore_MD        Open/No arrest
    ## 2334 Baltimore_MD        Open/No arrest
    ## 2335 Baltimore_MD        Open/No arrest
    ## 2336 Baltimore_MD        Open/No arrest
    ## 2337 Baltimore_MD        Open/No arrest
    ## 2338 Baltimore_MD        Open/No arrest
    ## 2339 Baltimore_MD      Closed by arrest
    ## 2340 Baltimore_MD Closed without arrest
    ## 2341 Baltimore_MD        Open/No arrest
    ## 2342 Baltimore_MD        Open/No arrest
    ## 2343 Baltimore_MD        Open/No arrest
    ## 2344 Baltimore_MD        Open/No arrest
    ## 2345 Baltimore_MD      Closed by arrest
    ## 2346 Baltimore_MD        Open/No arrest
    ## 2347 Baltimore_MD        Open/No arrest
    ## 2348 Baltimore_MD        Open/No arrest
    ## 2349 Baltimore_MD      Closed by arrest
    ## 2350 Baltimore_MD        Open/No arrest
    ## 2351 Baltimore_MD        Open/No arrest
    ## 2352 Baltimore_MD        Open/No arrest
    ## 2353 Baltimore_MD        Open/No arrest
    ## 2354 Baltimore_MD        Open/No arrest
    ## 2355 Baltimore_MD      Closed by arrest
    ## 2356 Baltimore_MD        Open/No arrest
    ## 2357 Baltimore_MD        Open/No arrest
    ## 2358 Baltimore_MD        Open/No arrest
    ## 2359 Baltimore_MD        Open/No arrest
    ## 2360 Baltimore_MD      Closed by arrest
    ## 2361 Baltimore_MD        Open/No arrest
    ## 2362 Baltimore_MD        Open/No arrest
    ## 2363 Baltimore_MD        Open/No arrest
    ## 2364 Baltimore_MD      Closed by arrest
    ## 2365 Baltimore_MD      Closed by arrest
    ## 2366 Baltimore_MD        Open/No arrest
    ## 2367 Baltimore_MD      Closed by arrest
    ## 2368 Baltimore_MD      Closed by arrest
    ## 2369 Baltimore_MD        Open/No arrest
    ## 2370 Baltimore_MD        Open/No arrest
    ## 2371 Baltimore_MD      Closed by arrest
    ## 2372 Baltimore_MD Closed without arrest
    ## 2373 Baltimore_MD      Closed by arrest
    ## 2374 Baltimore_MD        Open/No arrest
    ## 2375 Baltimore_MD      Closed by arrest
    ## 2376 Baltimore_MD Closed without arrest
    ## 2377 Baltimore_MD        Open/No arrest
    ## 2378 Baltimore_MD        Open/No arrest
    ## 2379 Baltimore_MD        Open/No arrest
    ## 2380 Baltimore_MD      Closed by arrest
    ## 2381 Baltimore_MD        Open/No arrest
    ## 2382 Baltimore_MD      Closed by arrest
    ## 2383 Baltimore_MD      Closed by arrest
    ## 2384 Baltimore_MD        Open/No arrest
    ## 2385 Baltimore_MD        Open/No arrest
    ## 2386 Baltimore_MD        Open/No arrest
    ## 2387 Baltimore_MD        Open/No arrest
    ## 2388 Baltimore_MD        Open/No arrest
    ## 2389 Baltimore_MD        Open/No arrest
    ## 2390 Baltimore_MD        Open/No arrest
    ## 2391 Baltimore_MD        Open/No arrest
    ## 2392 Baltimore_MD      Closed by arrest
    ## 2393 Baltimore_MD        Open/No arrest
    ## 2394 Baltimore_MD      Closed by arrest
    ## 2395 Baltimore_MD      Closed by arrest
    ## 2396 Baltimore_MD        Open/No arrest
    ## 2397 Baltimore_MD        Open/No arrest
    ## 2398 Baltimore_MD      Closed by arrest
    ## 2399 Baltimore_MD        Open/No arrest
    ## 2400 Baltimore_MD        Open/No arrest
    ## 2401 Baltimore_MD        Open/No arrest
    ## 2402 Baltimore_MD        Open/No arrest
    ## 2403 Baltimore_MD        Open/No arrest
    ## 2404 Baltimore_MD        Open/No arrest
    ## 2405 Baltimore_MD        Open/No arrest
    ## 2406 Baltimore_MD        Open/No arrest
    ## 2407 Baltimore_MD        Open/No arrest
    ## 2408 Baltimore_MD        Open/No arrest
    ## 2409 Baltimore_MD        Open/No arrest
    ## 2410 Baltimore_MD      Closed by arrest
    ## 2411 Baltimore_MD        Open/No arrest
    ## 2412 Baltimore_MD      Closed by arrest
    ## 2413 Baltimore_MD        Open/No arrest
    ## 2414 Baltimore_MD        Open/No arrest
    ## 2415 Baltimore_MD        Open/No arrest
    ## 2416 Baltimore_MD        Open/No arrest
    ## 2417 Baltimore_MD        Open/No arrest
    ## 2418 Baltimore_MD      Closed by arrest
    ## 2419 Baltimore_MD        Open/No arrest
    ## 2420 Baltimore_MD        Open/No arrest
    ## 2421 Baltimore_MD        Open/No arrest
    ## 2422 Baltimore_MD        Open/No arrest
    ## 2423 Baltimore_MD      Closed by arrest
    ## 2424 Baltimore_MD        Open/No arrest
    ## 2425 Baltimore_MD      Closed by arrest
    ## 2426 Baltimore_MD        Open/No arrest
    ## 2427 Baltimore_MD        Open/No arrest
    ## 2428 Baltimore_MD      Closed by arrest
    ## 2429 Baltimore_MD        Open/No arrest
    ## 2430 Baltimore_MD        Open/No arrest
    ## 2431 Baltimore_MD        Open/No arrest
    ## 2432 Baltimore_MD        Open/No arrest
    ## 2433 Baltimore_MD        Open/No arrest
    ## 2434 Baltimore_MD        Open/No arrest
    ## 2435 Baltimore_MD        Open/No arrest
    ## 2436 Baltimore_MD        Open/No arrest
    ## 2437 Baltimore_MD        Open/No arrest
    ## 2438 Baltimore_MD        Open/No arrest
    ## 2439 Baltimore_MD      Closed by arrest
    ## 2440 Baltimore_MD        Open/No arrest
    ## 2441 Baltimore_MD      Closed by arrest
    ## 2442 Baltimore_MD        Open/No arrest
    ## 2443 Baltimore_MD        Open/No arrest
    ## 2444 Baltimore_MD        Open/No arrest
    ## 2445 Baltimore_MD        Open/No arrest
    ## 2446 Baltimore_MD      Closed by arrest
    ## 2447 Baltimore_MD        Open/No arrest
    ## 2448 Baltimore_MD        Open/No arrest
    ## 2449 Baltimore_MD        Open/No arrest
    ## 2450 Baltimore_MD        Open/No arrest
    ## 2451 Baltimore_MD        Open/No arrest
    ## 2452 Baltimore_MD        Open/No arrest
    ## 2453 Baltimore_MD        Open/No arrest
    ## 2454 Baltimore_MD        Open/No arrest
    ## 2455 Baltimore_MD      Closed by arrest
    ## 2456 Baltimore_MD      Closed by arrest
    ## 2457 Baltimore_MD        Open/No arrest
    ## 2458 Baltimore_MD      Closed by arrest
    ## 2459 Baltimore_MD      Closed by arrest
    ## 2460 Baltimore_MD        Open/No arrest
    ## 2461 Baltimore_MD        Open/No arrest
    ## 2462 Baltimore_MD        Open/No arrest
    ## 2463 Baltimore_MD      Closed by arrest
    ## 2464 Baltimore_MD      Closed by arrest
    ## 2465 Baltimore_MD        Open/No arrest
    ## 2466 Baltimore_MD        Open/No arrest
    ## 2467 Baltimore_MD        Open/No arrest
    ## 2468 Baltimore_MD        Open/No arrest
    ## 2469 Baltimore_MD        Open/No arrest
    ## 2470 Baltimore_MD      Closed by arrest
    ## 2471 Baltimore_MD        Open/No arrest
    ## 2472 Baltimore_MD      Closed by arrest
    ## 2473 Baltimore_MD        Open/No arrest
    ## 2474 Baltimore_MD        Open/No arrest
    ## 2475 Baltimore_MD        Open/No arrest
    ## 2476 Baltimore_MD      Closed by arrest
    ## 2477 Baltimore_MD        Open/No arrest
    ## 2478 Baltimore_MD        Open/No arrest
    ## 2479 Baltimore_MD      Closed by arrest
    ## 2480 Baltimore_MD        Open/No arrest
    ## 2481 Baltimore_MD        Open/No arrest
    ## 2482 Baltimore_MD        Open/No arrest
    ## 2483 Baltimore_MD      Closed by arrest
    ## 2484 Baltimore_MD        Open/No arrest
    ## 2485 Baltimore_MD        Open/No arrest
    ## 2486 Baltimore_MD        Open/No arrest
    ## 2487 Baltimore_MD        Open/No arrest
    ## 2488 Baltimore_MD        Open/No arrest
    ## 2489 Baltimore_MD      Closed by arrest
    ## 2490 Baltimore_MD        Open/No arrest
    ## 2491 Baltimore_MD        Open/No arrest
    ## 2492 Baltimore_MD      Closed by arrest
    ## 2493 Baltimore_MD        Open/No arrest
    ## 2494 Baltimore_MD        Open/No arrest
    ## 2495 Baltimore_MD Closed without arrest
    ## 2496 Baltimore_MD        Open/No arrest
    ## 2497 Baltimore_MD        Open/No arrest
    ## 2498 Baltimore_MD        Open/No arrest
    ## 2499 Baltimore_MD        Open/No arrest
    ## 2500 Baltimore_MD      Closed by arrest
    ## 2501 Baltimore_MD        Open/No arrest
    ## 2502 Baltimore_MD        Open/No arrest
    ## 2503 Baltimore_MD        Open/No arrest
    ## 2504 Baltimore_MD      Closed by arrest
    ## 2505 Baltimore_MD        Open/No arrest
    ## 2506 Baltimore_MD        Open/No arrest
    ## 2507 Baltimore_MD        Open/No arrest
    ## 2508 Baltimore_MD        Open/No arrest
    ## 2509 Baltimore_MD      Closed by arrest
    ## 2510 Baltimore_MD      Closed by arrest
    ## 2511 Baltimore_MD        Open/No arrest
    ## 2512 Baltimore_MD        Open/No arrest
    ## 2513 Baltimore_MD        Open/No arrest
    ## 2514 Baltimore_MD      Closed by arrest
    ## 2515 Baltimore_MD        Open/No arrest
    ## 2516 Baltimore_MD        Open/No arrest
    ## 2517 Baltimore_MD      Closed by arrest
    ## 2518 Baltimore_MD Closed without arrest
    ## 2519 Baltimore_MD      Closed by arrest
    ## 2520 Baltimore_MD        Open/No arrest
    ## 2521 Baltimore_MD      Closed by arrest
    ## 2522 Baltimore_MD      Closed by arrest
    ## 2523 Baltimore_MD      Closed by arrest
    ## 2524 Baltimore_MD        Open/No arrest
    ## 2525 Baltimore_MD        Open/No arrest
    ## 2526 Baltimore_MD        Open/No arrest
    ## 2527 Baltimore_MD        Open/No arrest
    ## 2528 Baltimore_MD      Closed by arrest
    ## 2529 Baltimore_MD        Open/No arrest
    ## 2530 Baltimore_MD        Open/No arrest
    ## 2531 Baltimore_MD      Closed by arrest
    ## 2532 Baltimore_MD        Open/No arrest
    ## 2533 Baltimore_MD        Open/No arrest
    ## 2534 Baltimore_MD      Closed by arrest
    ## 2535 Baltimore_MD        Open/No arrest
    ## 2536 Baltimore_MD        Open/No arrest
    ## 2537 Baltimore_MD      Closed by arrest
    ## 2538 Baltimore_MD      Closed by arrest
    ## 2539 Baltimore_MD      Closed by arrest
    ## 2540 Baltimore_MD        Open/No arrest
    ## 2541 Baltimore_MD        Open/No arrest
    ## 2542 Baltimore_MD      Closed by arrest
    ## 2543 Baltimore_MD      Closed by arrest
    ## 2544 Baltimore_MD Closed without arrest
    ## 2545 Baltimore_MD      Closed by arrest
    ## 2546 Baltimore_MD        Open/No arrest
    ## 2547 Baltimore_MD      Closed by arrest
    ## 2548 Baltimore_MD      Closed by arrest
    ## 2549 Baltimore_MD        Open/No arrest
    ## 2550 Baltimore_MD        Open/No arrest
    ## 2551 Baltimore_MD        Open/No arrest
    ## 2552 Baltimore_MD        Open/No arrest
    ## 2553 Baltimore_MD      Closed by arrest
    ## 2554 Baltimore_MD      Closed by arrest
    ## 2555 Baltimore_MD      Closed by arrest
    ## 2556 Baltimore_MD        Open/No arrest
    ## 2557 Baltimore_MD        Open/No arrest
    ## 2558 Baltimore_MD      Closed by arrest
    ## 2559 Baltimore_MD        Open/No arrest
    ## 2560 Baltimore_MD      Closed by arrest
    ## 2561 Baltimore_MD        Open/No arrest
    ## 2562 Baltimore_MD      Closed by arrest
    ## 2563 Baltimore_MD Closed without arrest
    ## 2564 Baltimore_MD        Open/No arrest
    ## 2565 Baltimore_MD        Open/No arrest
    ## 2566 Baltimore_MD        Open/No arrest
    ## 2567 Baltimore_MD Closed without arrest
    ## 2568 Baltimore_MD Closed without arrest
    ## 2569 Baltimore_MD        Open/No arrest
    ## 2570 Baltimore_MD      Closed by arrest
    ## 2571 Baltimore_MD        Open/No arrest
    ## 2572 Baltimore_MD      Closed by arrest
    ## 2573 Baltimore_MD        Open/No arrest
    ## 2574 Baltimore_MD        Open/No arrest
    ## 2575 Baltimore_MD        Open/No arrest
    ## 2576 Baltimore_MD      Closed by arrest
    ## 2577 Baltimore_MD        Open/No arrest
    ## 2578 Baltimore_MD      Closed by arrest
    ## 2579 Baltimore_MD Closed without arrest
    ## 2580 Baltimore_MD        Open/No arrest
    ## 2581 Baltimore_MD        Open/No arrest
    ## 2582 Baltimore_MD      Closed by arrest
    ## 2583 Baltimore_MD      Closed by arrest
    ## 2584 Baltimore_MD      Closed by arrest
    ## 2585 Baltimore_MD      Closed by arrest
    ## 2586 Baltimore_MD      Closed by arrest
    ## 2587 Baltimore_MD      Closed by arrest
    ## 2588 Baltimore_MD      Closed by arrest
    ## 2589 Baltimore_MD      Closed by arrest
    ## 2590 Baltimore_MD        Open/No arrest
    ## 2591 Baltimore_MD        Open/No arrest
    ## 2592 Baltimore_MD        Open/No arrest
    ## 2593 Baltimore_MD        Open/No arrest
    ## 2594 Baltimore_MD      Closed by arrest
    ## 2595 Baltimore_MD        Open/No arrest
    ## 2596 Baltimore_MD        Open/No arrest
    ## 2597 Baltimore_MD        Open/No arrest
    ## 2598 Baltimore_MD        Open/No arrest
    ## 2599 Baltimore_MD        Open/No arrest
    ## 2600 Baltimore_MD      Closed by arrest
    ## 2601 Baltimore_MD      Closed by arrest
    ## 2602 Baltimore_MD      Closed by arrest
    ## 2603 Baltimore_MD        Open/No arrest
    ## 2604 Baltimore_MD        Open/No arrest
    ## 2605 Baltimore_MD        Open/No arrest
    ## 2606 Baltimore_MD        Open/No arrest
    ## 2607 Baltimore_MD        Open/No arrest
    ## 2608 Baltimore_MD        Open/No arrest
    ## 2609 Baltimore_MD        Open/No arrest
    ## 2610 Baltimore_MD      Closed by arrest
    ## 2611 Baltimore_MD        Open/No arrest
    ## 2612 Baltimore_MD        Open/No arrest
    ## 2613 Baltimore_MD        Open/No arrest
    ## 2614 Baltimore_MD        Open/No arrest
    ## 2615 Baltimore_MD      Closed by arrest
    ## 2616 Baltimore_MD        Open/No arrest
    ## 2617 Baltimore_MD        Open/No arrest
    ## 2618 Baltimore_MD        Open/No arrest
    ## 2619 Baltimore_MD        Open/No arrest
    ## 2620 Baltimore_MD        Open/No arrest
    ## 2621 Baltimore_MD        Open/No arrest
    ## 2622 Baltimore_MD        Open/No arrest
    ## 2623 Baltimore_MD        Open/No arrest
    ## 2624 Baltimore_MD        Open/No arrest
    ## 2625 Baltimore_MD      Closed by arrest
    ## 2626 Baltimore_MD        Open/No arrest
    ## 2627 Baltimore_MD      Closed by arrest
    ## 2628 Baltimore_MD        Open/No arrest
    ## 2629 Baltimore_MD        Open/No arrest
    ## 2630 Baltimore_MD        Open/No arrest
    ## 2631 Baltimore_MD        Open/No arrest
    ## 2632 Baltimore_MD        Open/No arrest
    ## 2633 Baltimore_MD      Closed by arrest
    ## 2634 Baltimore_MD        Open/No arrest
    ## 2635 Baltimore_MD        Open/No arrest
    ## 2636 Baltimore_MD      Closed by arrest
    ## 2637 Baltimore_MD        Open/No arrest
    ## 2638 Baltimore_MD      Closed by arrest
    ## 2639 Baltimore_MD        Open/No arrest
    ## 2640 Baltimore_MD      Closed by arrest
    ## 2641 Baltimore_MD        Open/No arrest
    ## 2642 Baltimore_MD        Open/No arrest
    ## 2643 Baltimore_MD        Open/No arrest
    ## 2644 Baltimore_MD        Open/No arrest
    ## 2645 Baltimore_MD        Open/No arrest
    ## 2646 Baltimore_MD        Open/No arrest
    ## 2647 Baltimore_MD      Closed by arrest
    ## 2648 Baltimore_MD      Closed by arrest
    ## 2649 Baltimore_MD        Open/No arrest
    ## 2650 Baltimore_MD        Open/No arrest
    ## 2651 Baltimore_MD      Closed by arrest
    ## 2652 Baltimore_MD        Open/No arrest
    ## 2653 Baltimore_MD        Open/No arrest
    ## 2654 Baltimore_MD        Open/No arrest
    ## 2655 Baltimore_MD        Open/No arrest
    ## 2656 Baltimore_MD      Closed by arrest
    ## 2657 Baltimore_MD        Open/No arrest
    ## 2658 Baltimore_MD      Closed by arrest
    ## 2659 Baltimore_MD        Open/No arrest
    ## 2660 Baltimore_MD      Closed by arrest
    ## 2661 Baltimore_MD        Open/No arrest
    ## 2662 Baltimore_MD        Open/No arrest
    ## 2663 Baltimore_MD      Closed by arrest
    ## 2664 Baltimore_MD        Open/No arrest
    ## 2665 Baltimore_MD      Closed by arrest
    ## 2666 Baltimore_MD        Open/No arrest
    ## 2667 Baltimore_MD        Open/No arrest
    ## 2668 Baltimore_MD        Open/No arrest
    ## 2669 Baltimore_MD      Closed by arrest
    ## 2670 Baltimore_MD        Open/No arrest
    ## 2671 Baltimore_MD        Open/No arrest
    ## 2672 Baltimore_MD        Open/No arrest
    ## 2673 Baltimore_MD      Closed by arrest
    ## 2674 Baltimore_MD        Open/No arrest
    ## 2675 Baltimore_MD      Closed by arrest
    ## 2676 Baltimore_MD        Open/No arrest
    ## 2677 Baltimore_MD      Closed by arrest
    ## 2678 Baltimore_MD      Closed by arrest
    ## 2679 Baltimore_MD        Open/No arrest
    ## 2680 Baltimore_MD      Closed by arrest
    ## 2681 Baltimore_MD      Closed by arrest
    ## 2682 Baltimore_MD        Open/No arrest
    ## 2683 Baltimore_MD        Open/No arrest
    ## 2684 Baltimore_MD        Open/No arrest
    ## 2685 Baltimore_MD        Open/No arrest
    ## 2686 Baltimore_MD        Open/No arrest
    ## 2687 Baltimore_MD        Open/No arrest
    ## 2688 Baltimore_MD        Open/No arrest
    ## 2689 Baltimore_MD        Open/No arrest
    ## 2690 Baltimore_MD        Open/No arrest
    ## 2691 Baltimore_MD        Open/No arrest
    ## 2692 Baltimore_MD        Open/No arrest
    ## 2693 Baltimore_MD        Open/No arrest
    ## 2694 Baltimore_MD      Closed by arrest
    ## 2695 Baltimore_MD        Open/No arrest
    ## 2696 Baltimore_MD        Open/No arrest
    ## 2697 Baltimore_MD      Closed by arrest
    ## 2698 Baltimore_MD      Closed by arrest
    ## 2699 Baltimore_MD        Open/No arrest
    ## 2700 Baltimore_MD        Open/No arrest
    ## 2701 Baltimore_MD        Open/No arrest
    ## 2702 Baltimore_MD        Open/No arrest
    ## 2703 Baltimore_MD        Open/No arrest
    ## 2704 Baltimore_MD        Open/No arrest
    ## 2705 Baltimore_MD        Open/No arrest
    ## 2706 Baltimore_MD        Open/No arrest
    ## 2707 Baltimore_MD        Open/No arrest
    ## 2708 Baltimore_MD        Open/No arrest
    ## 2709 Baltimore_MD        Open/No arrest
    ## 2710 Baltimore_MD      Closed by arrest
    ## 2711 Baltimore_MD        Open/No arrest
    ## 2712 Baltimore_MD      Closed by arrest
    ## 2713 Baltimore_MD        Open/No arrest
    ## 2714 Baltimore_MD        Open/No arrest
    ## 2715 Baltimore_MD      Closed by arrest
    ## 2716 Baltimore_MD        Open/No arrest
    ## 2717 Baltimore_MD      Closed by arrest
    ## 2718 Baltimore_MD        Open/No arrest
    ## 2719 Baltimore_MD        Open/No arrest
    ## 2720 Baltimore_MD        Open/No arrest
    ## 2721 Baltimore_MD Closed without arrest
    ## 2722 Baltimore_MD        Open/No arrest
    ## 2723 Baltimore_MD      Closed by arrest
    ## 2724 Baltimore_MD      Closed by arrest
    ## 2725 Baltimore_MD      Closed by arrest
    ## 2726 Baltimore_MD        Open/No arrest
    ## 2727 Baltimore_MD        Open/No arrest
    ## 2728 Baltimore_MD        Open/No arrest
    ## 2729 Baltimore_MD        Open/No arrest
    ## 2730 Baltimore_MD      Closed by arrest
    ## 2731 Baltimore_MD        Open/No arrest
    ## 2732 Baltimore_MD      Closed by arrest
    ## 2733 Baltimore_MD        Open/No arrest
    ## 2734 Baltimore_MD        Open/No arrest
    ## 2735 Baltimore_MD Closed without arrest
    ## 2736 Baltimore_MD        Open/No arrest
    ## 2737 Baltimore_MD        Open/No arrest
    ## 2738 Baltimore_MD Closed without arrest
    ## 2739 Baltimore_MD        Open/No arrest
    ## 2740 Baltimore_MD        Open/No arrest
    ## 2741 Baltimore_MD        Open/No arrest
    ## 2742 Baltimore_MD        Open/No arrest
    ## 2743 Baltimore_MD      Closed by arrest
    ## 2744 Baltimore_MD        Open/No arrest
    ## 2745 Baltimore_MD        Open/No arrest
    ## 2746 Baltimore_MD        Open/No arrest
    ## 2747 Baltimore_MD        Open/No arrest
    ## 2748 Baltimore_MD        Open/No arrest
    ## 2749 Baltimore_MD        Open/No arrest
    ## 2750 Baltimore_MD        Open/No arrest
    ## 2751 Baltimore_MD        Open/No arrest
    ## 2752 Baltimore_MD      Closed by arrest
    ## 2753 Baltimore_MD        Open/No arrest
    ## 2754 Baltimore_MD        Open/No arrest
    ## 2755 Baltimore_MD        Open/No arrest
    ## 2756 Baltimore_MD      Closed by arrest
    ## 2757 Baltimore_MD      Closed by arrest
    ## 2758 Baltimore_MD Closed without arrest
    ## 2759 Baltimore_MD        Open/No arrest
    ## 2760 Baltimore_MD        Open/No arrest
    ## 2761 Baltimore_MD        Open/No arrest
    ## 2762 Baltimore_MD        Open/No arrest
    ## 2763 Baltimore_MD        Open/No arrest
    ## 2764 Baltimore_MD        Open/No arrest
    ## 2765 Baltimore_MD      Closed by arrest
    ## 2766 Baltimore_MD        Open/No arrest
    ## 2767 Baltimore_MD      Closed by arrest
    ## 2768 Baltimore_MD        Open/No arrest
    ## 2769 Baltimore_MD        Open/No arrest
    ## 2770 Baltimore_MD        Open/No arrest
    ## 2771 Baltimore_MD        Open/No arrest
    ## 2772 Baltimore_MD        Open/No arrest
    ## 2773 Baltimore_MD        Open/No arrest
    ## 2774 Baltimore_MD        Open/No arrest
    ## 2775 Baltimore_MD      Closed by arrest
    ## 2776 Baltimore_MD      Closed by arrest
    ## 2777 Baltimore_MD        Open/No arrest
    ## 2778 Baltimore_MD      Closed by arrest
    ## 2779 Baltimore_MD        Open/No arrest
    ## 2780 Baltimore_MD        Open/No arrest
    ## 2781 Baltimore_MD        Open/No arrest
    ## 2782 Baltimore_MD        Open/No arrest
    ## 2783 Baltimore_MD        Open/No arrest
    ## 2784 Baltimore_MD        Open/No arrest
    ## 2785 Baltimore_MD        Open/No arrest
    ## 2786 Baltimore_MD        Open/No arrest
    ## 2787 Baltimore_MD        Open/No arrest
    ## 2788 Baltimore_MD        Open/No arrest
    ## 2789 Baltimore_MD        Open/No arrest
    ## 2790 Baltimore_MD        Open/No arrest
    ## 2791 Baltimore_MD      Closed by arrest
    ## 2792 Baltimore_MD        Open/No arrest
    ## 2793 Baltimore_MD        Open/No arrest
    ## 2794 Baltimore_MD        Open/No arrest
    ## 2795 Baltimore_MD        Open/No arrest
    ## 2796 Baltimore_MD        Open/No arrest
    ## 2797 Baltimore_MD        Open/No arrest
    ## 2798 Baltimore_MD        Open/No arrest
    ## 2799 Baltimore_MD        Open/No arrest
    ## 2800 Baltimore_MD        Open/No arrest
    ## 2801 Baltimore_MD        Open/No arrest
    ## 2802 Baltimore_MD        Open/No arrest
    ## 2803 Baltimore_MD        Open/No arrest
    ## 2804 Baltimore_MD        Open/No arrest
    ## 2805 Baltimore_MD      Closed by arrest
    ## 2806 Baltimore_MD        Open/No arrest
    ## 2807 Baltimore_MD        Open/No arrest
    ## 2808 Baltimore_MD        Open/No arrest
    ## 2809 Baltimore_MD        Open/No arrest
    ## 2810 Baltimore_MD        Open/No arrest
    ## 2811 Baltimore_MD        Open/No arrest
    ## 2812 Baltimore_MD        Open/No arrest
    ## 2813 Baltimore_MD        Open/No arrest
    ## 2814 Baltimore_MD      Closed by arrest
    ## 2815 Baltimore_MD      Closed by arrest
    ## 2816 Baltimore_MD        Open/No arrest
    ## 2817 Baltimore_MD        Open/No arrest
    ## 2818 Baltimore_MD        Open/No arrest
    ## 2819 Baltimore_MD        Open/No arrest
    ## 2820 Baltimore_MD        Open/No arrest
    ## 2821 Baltimore_MD        Open/No arrest
    ## 2822 Baltimore_MD        Open/No arrest
    ## 2823 Baltimore_MD        Open/No arrest
    ## 2824 Baltimore_MD        Open/No arrest
    ## 2825 Baltimore_MD        Open/No arrest
    ## 2826 Baltimore_MD        Open/No arrest
    ## 2827 Baltimore_MD        Open/No arrest

``` r
baltimore_unsolved
```

    ## # A tibble: 1 x 2
    ##   city_state   total_unsolved
    ##   <chr>                 <int>
    ## 1 Baltimore_MD           1825
