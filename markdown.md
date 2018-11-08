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
  filter(city_state != "Tulsa,AL")
allcities_proptest = 
  partial_table %>%
  na.omit(partial_table) %>%
  mutate(homicide_prop = map2(.x = total_unsolved, .y = total, ~prop.test(x = .x, n = .y)))
allcities_proptest
```

    ##           city_state total total_unsolved
    ## 1     Albuquerque,NM   378            146
    ## 2         Atlanta,GA   973            373
    ## 3       Baltimore,MD  2827           1825
    ## 4     Baton Rouge,LA   424            196
    ## 5      Birmingham,AL   800            347
    ## 6          Boston,MA   614            310
    ## 7         Buffalo,NY   521            319
    ## 8       Charlotte,NC   687            206
    ## 9         Chicago,IL  5535           4073
    ## 10     Cincinnati,OH   694            309
    ## 11       Columbus,OH  1084            575
    ## 12         Dallas,TX  1567            754
    ## 13         Denver,CO   312            169
    ## 14        Detroit,MI  2519           1482
    ## 15         Durham,NC   276            101
    ## 16     Fort Worth,TX   549            255
    ## 17         Fresno,CA   487            169
    ## 18        Houston,TX  2942           1493
    ## 19   Indianapolis,IN  1322            594
    ## 20   Jacksonville,FL  1168            597
    ## 21    Kansas City,MO  1190            486
    ## 22      Las Vegas,NV  1381            572
    ## 23     Long Beach,CA   378            156
    ## 24    Los Angeles,CA  2257           1106
    ## 25     Louisville,KY   576            261
    ## 26        Memphis,TN  1514            483
    ## 27          Miami,FL   744            450
    ## 28      Milwaukee,wI  1115            403
    ## 29    Minneapolis,MN   366            187
    ## 30      Nashville,TN   767            278
    ## 31    New Orleans,LA  1434            930
    ## 32       New York,NY   627            243
    ## 33        Oakland,CA   947            508
    ## 34  Oklahoma City,OK   672            326
    ## 35          Omaha,NE   409            169
    ## 36   Philadelphia,PA  3037           1360
    ## 37        Phoenix,AZ   914            504
    ## 38     Pittsburgh,PA   631            337
    ## 39       Richmond,VA   429            113
    ## 40     Sacramento,CA   376            139
    ## 41    San Antonio,TX   833            357
    ## 42 San Bernardino,CA   275            170
    ## 43      San Diego,CA   461            175
    ## 44  San Francisco,CA   663            336
    ## 45       Savannah,GA   246            115
    ## 46      St. Louis,MO  1677            905
    ## 47       Stockton,CA   444            266
    ## 48          Tampa,FL   208             95
    ## 49          Tulsa,OK   583            193
    ## 50     Washington,DC  1345            589
    ##                                                                                                                                                                                                    homicide_prop
    ## 1   19.1137566137566, 1, 1.23154001810821e-05, 0.386243386243386, 0.5, 0.337260384254284, 0.437576606555521, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 2   52.4933196300103, 1, 4.31714353011968e-13, 0.383350462487153, 0.5, 0.352811897036302, 0.414821883953622, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 3   239.010965688008, 1, 6.46191094418276e-54, 0.645560665015918, 0.5, 0.627562457662644, 0.663159860401662, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 4      2.26650943396226, 1, 0.132197352925728, 0.462264150943396, 0.5, 0.414198741860307, 0.511023960018796, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 5                     13.78125, 1, 0.000205375736141218, 0.43375, 0.5, 0.399188948632167, 0.468955748189036, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 6    0.0407166123778502, 1, 0.840085920279627, 0.504885993485342, 0.5, 0.464621930200304, 0.545088051772638, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 7   25.8272552783109, 1, 3.73378626340806e-07, 0.612284069097889, 0.5, 0.568798964634228, 0.654087939253532, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 8    109.280931586608, 1, 1.40840653990335e-25, 0.299854439592431, 0.5, 0.26608198188312, 0.335899860867845, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 9  1230.73170731707, 1, 1.27794337263715e-269, 0.735862691960253, 0.5, 0.723995888425454, 0.747399787306647, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 10    8.10518731988473, 1, 0.0044138760082113, 0.445244956772334, 0.5, 0.407960574220688, 0.483143880618937, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 11    3.89760147601476, 1, 0.0483550958797455, 0.530442804428044, 0.5, 0.500216719334982, 0.560450554069058, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 12     2.14677728142948, 1, 0.142869501770931, 0.481174218251436, 0.5, 0.456194154046593, 0.506247544691615, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 13      2.00320512820513, 1, 0.156966988253579, 0.541666666666667, 0.5, 0.484609835539695, 0.59768074787163, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 14   78.2596268360461, 1, 9.03497051508298e-19, 0.58832870186582, 0.5, 0.568790304428009, 0.607595332109926, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 15  19.3079710144928, 1, 1.11240985163748e-05, 0.365942028985507, 0.5, 0.309587370745465, 0.426093646122628, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 16      2.63023679417122, 1, 0.104846075824535, 0.46448087431694, 0.5, 0.422254218817489, 0.507211882429337, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 17   44.9774127310062, 1, 1.9932031428594e-11, 0.347022587268994, 0.5, 0.305101254947731, 0.391396303001031, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 18      0.628484024473148, 1, 0.427911929474704, 0.507477906186268, 0.5, 0.48924468839782, 0.52569143782639, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 19  13.3804841149773, 1, 0.000254256092810618, 0.449319213313162, 0.5, 0.422315592149053, 0.476620653261321, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 20    0.535102739726027, 1, 0.464469426655636, 0.511130136986301, 0.5, 0.482045992882616, 0.540140220940324, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 21  39.5705882352941, 1, 3.16412970113392e-10, 0.408403361344538, 0.5, 0.380399622526014, 0.437005420291775, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 22   40.3301955104996, 1, 2.14468483792985e-10, 0.414192614047791, 0.5, 0.388128427543561, 0.44073947345246, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 23   11.1772486772487, 1, 0.00082806474000556, 0.412698412698413, 0.5, 0.362902632730428, 0.464297334473225, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 24      0.857775808595481, 1, 0.354361865122856, 0.490031014621179, 0.5, 0.4692208307642, 0.510875439229204, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 25             4.87673611111111, 1, 0.0272210453226514, 0.453125, 0.5, 0.412060862911807, 0.494823450153796, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 26  197.628137384412, 1, 6.87765680241998e-45, 0.319022457067371, 0.5, 0.295704710084932, 0.343269132061918, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 27    32.2916666666667, 1, 1.3268114800848e-08, 0.604838709677419, 0.5, 0.568578324827888, 0.64000148914404, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 28  85.0798206278027, 1, 2.86560200387094e-20, 0.361434977578475, 0.5, 0.333317185397031, 0.390519378344496, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 29     0.133879781420765, 1, 0.71444278464302, 0.510928961748634, 0.5, 0.458514982041853, 0.563109883458537, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 30     57.496740547588, 1, 3.38539835774077e-14, 0.36245110821382, 0.5, 0.32855921177063, 0.397740143098163, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 31  125.958856345886, 1, 3.13924756184415e-29, 0.648535564853556, 0.5, 0.623104762061361, 0.673161504551066, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 32   31.2599681020734, 1, 2.25685690055446e-08, 0.38755980861244, 0.5, 0.349442079407787, 0.427075473225884, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 33    4.88278775079197, 1, 0.0271257750901525, 0.536430834213305, 0.5, 0.504058764486028, 0.568503654694674, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 34    0.537202380952381, 1, 0.463594468631247, 0.485119047619048, 0.5, 0.446786082294775, 0.523624500045037, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 35    11.9804400977995, 1, 0.000537618853843276, 0.41320293398533, 0.5, 0.365314587740629, 0.46274774106504, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 36    32.8798156075074, 1, 9.80358808318087e-09, 0.447810339150477, 0.5, 0.43003802587334, 0.46571574030871, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 37    9.46280087527352, 1, 0.00209680659797679, 0.551422319474836, 0.5, 0.518482503921835, 0.58392440997268, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 38    2.79556259904913, 1, 0.0945255840653185, 0.534072900158479, 0.5, 0.494270601334985, 0.573454475783427, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 39  95.1142191142191, 1, 1.79711251331279e-22, 0.263403263403263, 0.5, 0.222857090697159, 0.308265775687954, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 40   25.0239361702128, 1, 5.66229979421114e-07, 0.36968085106383, 0.5, 0.321155936941272, 0.420913133871621, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 41   16.7154861944778, 1, 4.3425074873034e-05, 0.428571428571429, 0.5, 0.394777213342685, 0.463033104312304, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 42  14.8945454545455, 1, 0.000113691242149523, 0.618181818181818, 0.5, 0.557662796603596, 0.675342223114347, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 43  26.2472885032538, 1, 3.00376272365054e-07, 0.379609544468547, 0.5, 0.335425902281662, 0.425831508201927, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 44   0.0965309200603318, 1, 0.756033065147004, 0.506787330316742, 0.5, 0.468051609424123, 0.545443306766946, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 45    0.914634146341463, 1, 0.338887825853668, 0.467479674796748, 0.5, 0.404125196559388, 0.531866539046565, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 46   10.3899821109123, 1, 0.00126700851158641, 0.539654144305307, 0.5, 0.515436912252685, 0.563687859400505, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 47  17.0472972972973, 1, 3.64602307188997e-05, 0.599099099099099, 0.5, 0.551714515213198, 0.644741783449581, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 48       1.38942307692308, 1, 0.238502552272325, 0.456730769230769, 0.5, 0.388100903021656, 0.5269851074937, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 49   65.893653516295, 1, 4.75921685526583e-16, 0.331046312178388, 0.5, 0.293234872894204, 0.371119167790918, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
    ## 50   20.4877323420074, 1, 6.00146707319468e-06, 0.437918215613383, 0.5, 0.411249523972199, 0.46494547194691, two.sided, 1-sample proportions test with continuity correction, .x out of .y, null probability 0.5
