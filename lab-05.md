Lab 05 - Wrangling spatial data
================
Alex Connolly
IFeb 21 2022

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
```

### Exercise 1

``` r
dn_ak <- dennys %>%
  filter(state == "AK")
nrow(dn_ak)
```

    ## [1] 3

3 locations in Alaska

### Exercise 2

``` r
lq_ak <- laquinta %>%
  filter(state == "AK")
nrow(lq_ak)
```

    ## [1] 2

2

### Exercise 3

``` r
dn_lq_ak <- full_join(dn_ak, lq_ak, by = "state")
dn_lq_ak
```

    ## # A tibble: 6 × 11
    ##   address.x     city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##   <chr>         <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ## 1 2900 Denali   Ancho… AK    99503       -150.       61.2 3501 Min… "\nAn… 99503
    ## 2 2900 Denali   Ancho… AK    99503       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 3 3850 Debarr … Ancho… AK    99508       -150.       61.2 3501 Min… "\nAn… 99503
    ## 4 3850 Debarr … Ancho… AK    99508       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 5 1929 Airport… Fairb… AK    99701       -148.       64.8 3501 Min… "\nAn… 99503
    ## 6 1929 Airport… Fairb… AK    99701       -148.       64.8 4920 Dal… "\nFa… 99709
    ## # … with 2 more variables: longitude.y <dbl>, latitude.y <dbl>

### Exercise 4

6 observations, the variables are Address, city, zip, longitude, and
latitude of both x and y (seperately) and state

### Exercise 5

``` r
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 = long1 * pi / 180
  lat1  = lat1  * pi / 180
  long2 = long2 * pi / 180
  lat2  = lat2  * pi / 180
  
  R = 6371 # Earth mean radius in km
  
  a = sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2
  d = R * 2 * asin(sqrt(a))
  
  return( round(d,round) ) # distance in km
}
```

### Exercise 6

``` r
dn_lq_ak <- dn_lq_ak %>%
  mutate(dn_lq_ak, distance = haversine(longitude.x, latitude.x, longitude.y, latitude.y, round = 3))
dn_lq_ak
```

    ## # A tibble: 6 × 12
    ##   address.x     city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##   <chr>         <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ## 1 2900 Denali   Ancho… AK    99503       -150.       61.2 3501 Min… "\nAn… 99503
    ## 2 2900 Denali   Ancho… AK    99503       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 3 3850 Debarr … Ancho… AK    99508       -150.       61.2 3501 Min… "\nAn… 99503
    ## 4 3850 Debarr … Ancho… AK    99508       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 5 1929 Airport… Fairb… AK    99701       -148.       64.8 3501 Min… "\nAn… 99503
    ## 6 1929 Airport… Fairb… AK    99701       -148.       64.8 4920 Dal… "\nFa… 99709
    ## # … with 3 more variables: longitude.y <dbl>, latitude.y <dbl>, distance <dbl>

### Exercise 7

``` r
dn_lq_ak_mindist <- dn_lq_ak %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))
dn_lq_ak_mindist
```

    ## # A tibble: 3 × 2
    ##   address.x        closest
    ##   <chr>              <dbl>
    ## 1 1929 Airport Way    5.20
    ## 2 2900 Denali         2.04
    ## 3 3850 Debarr Road    6.00

### Exercise 8

``` r
dn_lq_ak%>%
  ggplot(mapping = aes(x=address.x, y=distance)) + geom_point()
```

![](lab-05_files/figure-gfm/plot-1.png)<!-- -->

``` r
dn_lq_ak_mindist %>%
  ggplot(mapping = aes(x=address.x, y=closest)) + geom_point()
```

![](lab-05_files/figure-gfm/plot2-1.png)<!-- --> As you can see, each
Dennys has a La Quinta fairly close to it. Zooming in, you can see how
one Dennys is even closer to a La Quinta, the one on 2900 Denali.

### Exercise 9

``` r
dn_nc <- dennys %>%
  filter(state == "NC")
nrow(dn_nc)
```

    ## [1] 28

28 locations in North Carolina

``` r
lq_nc <- laquinta %>%
  filter(state == "NC")
nrow(lq_nc)
```

    ## [1] 12

12 La Quinta locations in NC

``` r
dn_lq_nc <- full_join(dn_nc, lq_nc, by = "state")
dn_lq_nc
```

    ## # A tibble: 336 × 11
    ##    address.x    city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##    <chr>        <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ##  1 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 165 Hwy … "\nBo… 28607
    ##  2 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 3127 Slo… "\nCh… 28208
    ##  3 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 4900 Sou… "\nCh… 28217
    ##  4 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 4414 Dur… "\nDu… 27707
    ##  5 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 1910 Wes… "\nDu… 27713
    ##  6 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 1201 Lan… "\nGr… 27407
    ##  7 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 1607 Fai… "\nCo… 28613
    ##  8 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 191 Cres… "\nCa… 27518
    ##  9 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 2211 Sum… "\nRa… 27612
    ## 10 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 1001 Aer… "\nMo… 27560
    ## # … with 326 more rows, and 2 more variables: longitude.y <dbl>,
    ## #   latitude.y <dbl>

336 observations, the variables are Address, city, zip, longitude, and
latitude of both x and y (separately) and state

``` r
dn_lq_nc <- dn_lq_nc %>%
  mutate(dn_lq_nc, distance = haversine(longitude.x, latitude.x, longitude.y, latitude.y, round = 3))
dn_lq_nc
```

    ## # A tibble: 336 × 12
    ##    address.x    city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##    <chr>        <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ##  1 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 165 Hwy … "\nBo… 28607
    ##  2 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 3127 Slo… "\nCh… 28208
    ##  3 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 4900 Sou… "\nCh… 28217
    ##  4 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 4414 Dur… "\nDu… 27707
    ##  5 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 1910 Wes… "\nDu… 27713
    ##  6 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 1201 Lan… "\nGr… 27407
    ##  7 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 1607 Fai… "\nCo… 28613
    ##  8 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 191 Cres… "\nCa… 27518
    ##  9 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 2211 Sum… "\nRa… 27612
    ## 10 1 Regent Pa… Ashev… NC    28806       -82.6       35.6 1001 Aer… "\nMo… 27560
    ## # … with 326 more rows, and 3 more variables: longitude.y <dbl>,
    ## #   latitude.y <dbl>, distance <dbl>

``` r
dn_lq_nc_mindist <- dn_lq_nc %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))
dn_lq_nc_mindist
```

    ## # A tibble: 28 × 2
    ##    address.x                 closest
    ##    <chr>                       <dbl>
    ##  1 1 Regent Park Boulevard     108. 
    ##  2 101 Wintergreen Dr          120. 
    ##  3 103 Sedgehill Dr             26.7
    ##  4 1043 Jimmie Kerr Road        36.1
    ##  5 1201 S College Road         188. 
    ##  6 1209 Burkemount Avenue       39.1
    ##  7 1493 Us Hwy 74-A Bypass      70.1
    ##  8 1524 Dabney Dr               59.5
    ##  9 1550 Four Seasons           115. 
    ## 10 1800 Princeton-Kenly Road    55.9
    ## # … with 18 more rows

``` r
dn_lq_nc%>%
  ggplot(mapping = aes(x=address.x, y=distance)) + geom_point()
```

![](lab-05_files/figure-gfm/plot3-1.png)<!-- --> As you can see here,
there are some La Quintas that are close to Dennys, but some far away.

``` r
dn_lq_nc_mindist%>%
  ggplot(mapping = aes(x=address.x, y=closest)) + geom_point()
```

![](lab-05_files/figure-gfm/plot11-1.png)<!-- --> So, lets look at the
closest for each one. It does seem as each of them have one that is
fairly close, over half within 50.

### Exercise 10

``` r
dn_tx <- dennys %>%
  filter(state == "TX")
nrow(dn_tx)
```

    ## [1] 200

200 Dennys locations in Texas

``` r
lq_tx <- laquinta %>%
  filter(state == "TX")
nrow(lq_tx)
```

    ## [1] 237

237 La Quinta locations in TX

``` r
dn_lq_tx <- full_join(dn_tx, lq_tx, by = "state")
dn_lq_tx
```

    ## # A tibble: 47,400 × 11
    ##    address.x    city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##    <chr>        <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ##  1 120 East I-… Abile… TX    79601       -99.6       32.4 3018 Cat… "\nAb… 79606
    ##  2 120 East I-… Abile… TX    79601       -99.6       32.4 3501 Wes… "\nAb… 79601
    ##  3 120 East I-… Abile… TX    79601       -99.6       32.4 14925 La… "\nAd… 75254
    ##  4 120 East I-… Abile… TX    79601       -99.6       32.4 909 East… "\nAl… 78516
    ##  5 120 East I-… Abile… TX    79601       -99.6       32.4 2400 Eas… "\nAl… 78332
    ##  6 120 East I-… Abile… TX    79601       -99.6       32.4 1220 Nor… "\nAl… 75013
    ##  7 120 East I-… Abile… TX    79601       -99.6       32.4 1165 Hwy… "\nAl… 76009
    ##  8 120 East I-… Abile… TX    79601       -99.6       32.4 880 Sout… "\nAl… 77511
    ##  9 120 East I-… Abile… TX    79601       -99.6       32.4 1708 Int… "\nAm… 79103
    ## 10 120 East I-… Abile… TX    79601       -99.6       32.4 9305 Eas… "\nAm… 79118
    ## # … with 47,390 more rows, and 2 more variables: longitude.y <dbl>,
    ## #   latitude.y <dbl>

47,400 observations, the variables are Address, city, zip, longitude,
and latitude of both x and y (separately) and state

``` r
dn_lq_tx <- dn_lq_tx %>%
  mutate(dn_lq_tx, distance = haversine(longitude.x, latitude.x, longitude.y, latitude.y, round = 3))
dn_lq_tx
```

    ## # A tibble: 47,400 × 12
    ##    address.x    city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##    <chr>        <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ##  1 120 East I-… Abile… TX    79601       -99.6       32.4 3018 Cat… "\nAb… 79606
    ##  2 120 East I-… Abile… TX    79601       -99.6       32.4 3501 Wes… "\nAb… 79601
    ##  3 120 East I-… Abile… TX    79601       -99.6       32.4 14925 La… "\nAd… 75254
    ##  4 120 East I-… Abile… TX    79601       -99.6       32.4 909 East… "\nAl… 78516
    ##  5 120 East I-… Abile… TX    79601       -99.6       32.4 2400 Eas… "\nAl… 78332
    ##  6 120 East I-… Abile… TX    79601       -99.6       32.4 1220 Nor… "\nAl… 75013
    ##  7 120 East I-… Abile… TX    79601       -99.6       32.4 1165 Hwy… "\nAl… 76009
    ##  8 120 East I-… Abile… TX    79601       -99.6       32.4 880 Sout… "\nAl… 77511
    ##  9 120 East I-… Abile… TX    79601       -99.6       32.4 1708 Int… "\nAm… 79103
    ## 10 120 East I-… Abile… TX    79601       -99.6       32.4 9305 Eas… "\nAm… 79118
    ## # … with 47,390 more rows, and 3 more variables: longitude.y <dbl>,
    ## #   latitude.y <dbl>, distance <dbl>

``` r
dn_lq_tx_mindist <- dn_lq_tx %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))
dn_lq_tx_mindist
```

    ## # A tibble: 200 × 2
    ##    address.x             closest
    ##    <chr>                   <dbl>
    ##  1 100 Cottonwood         33.6  
    ##  2 100 E Pinehurst         1.39 
    ##  3 100 Us Highway 79 S    33.9  
    ##  4 101 N Fm 707           10.3  
    ##  5 1011 Beltway Parkway   14.0  
    ##  6 1015 Spur 350 West      1.74 
    ##  7 1015 West Main St       1.10 
    ##  8 10367 Highway 59       37.6  
    ##  9 10433 N Central Expwy   0.618
    ## 10 105 W 42nd St           6.88 
    ## # … with 190 more rows

``` r
dn_lq_tx%>%
  ggplot(mapping = aes(x=address.x, y=distance)) + geom_boxplot()
```

![](lab-05_files/figure-gfm/plot10-1.png)<!-- --> With how many
observations were here, the scatter plot we have been using was not that
helpful. This box and whisker plot, while tough to read, shows the sheer
number of obserations, and how some are close but a lot are very far
away from some Laquitas. But this is not that helpful since there are so
many of each. So let’s look at closest.

``` r
dn_lq_tx_mindist%>%
  ggplot(mapping = aes(x=address.x, y=closest)) + geom_point()
```

![](lab-05_files/figure-gfm/plot4-1.png)<!-- --> As you can see here, it
does appear as if there is a La Quinta very close to almost every
Dennys, only a handful were farther away. ### Exercise 11

``` r
dn_pa <- dennys %>%
  filter(state == "PA")
nrow(dn_pa)
```

    ## [1] 40

40 locations in Pennsylvania

``` r
lq_pa <- laquinta %>%
  filter(state == "PA")
nrow(lq_pa)
```

    ## [1] 10

10 La Quinta locations in PA

``` r
dn_lq_pa <- full_join(dn_pa, lq_pa, by = "state")
dn_lq_pa
```

    ## # A tibble: 400 × 11
    ##    address.x    city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##    <chr>        <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ##  1 1871 Catasa… Allen… PA    18109       -75.4       40.6 199 Walk… "\nCh… 17201
    ##  2 1871 Catasa… Allen… PA    18109       -75.4       40.6 7820 Per… "\nEr… 16509
    ##  3 1871 Catasa… Allen… PA    18109       -75.4       40.6 990 Eise… "\nHa… 17111
    ##  4 1871 Catasa… Allen… PA    18109       -75.4       40.6 265 N. H… "\nHa… 17112
    ##  5 1871 Catasa… Allen… PA    18109       -75.4       40.6 25 Eastb… "\nRo… 17572
    ##  6 1871 Catasa… Allen… PA    18109       -75.4       40.6 350 Bent… "\nMe… 17050
    ##  7 1871 Catasa… Allen… PA    18109       -75.4       40.6 130 Lime… "\nNe… 17070
    ##  8 1871 Catasa… Allen… PA    18109       -75.4       40.6 53 Indus… "\nEs… 19029
    ##  9 1871 Catasa… Allen… PA    18109       -75.4       40.6 8507 Uni… "\nMo… 15108
    ## 10 1871 Catasa… Allen… PA    18109       -75.4       40.6 1405 Ken… "\nYo… 17408
    ## # … with 390 more rows, and 2 more variables: longitude.y <dbl>,
    ## #   latitude.y <dbl>

400 observations, the variables are Address, city, zip, longitude, and
latitude of both x and y (separately) and state

``` r
dn_lq_pa <- dn_lq_pa %>%
  mutate(dn_lq_pa, distance = haversine(longitude.x, latitude.x, longitude.y, latitude.y, round = 3))
dn_lq_pa
```

    ## # A tibble: 400 × 12
    ##    address.x    city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##    <chr>        <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ##  1 1871 Catasa… Allen… PA    18109       -75.4       40.6 199 Walk… "\nCh… 17201
    ##  2 1871 Catasa… Allen… PA    18109       -75.4       40.6 7820 Per… "\nEr… 16509
    ##  3 1871 Catasa… Allen… PA    18109       -75.4       40.6 990 Eise… "\nHa… 17111
    ##  4 1871 Catasa… Allen… PA    18109       -75.4       40.6 265 N. H… "\nHa… 17112
    ##  5 1871 Catasa… Allen… PA    18109       -75.4       40.6 25 Eastb… "\nRo… 17572
    ##  6 1871 Catasa… Allen… PA    18109       -75.4       40.6 350 Bent… "\nMe… 17050
    ##  7 1871 Catasa… Allen… PA    18109       -75.4       40.6 130 Lime… "\nNe… 17070
    ##  8 1871 Catasa… Allen… PA    18109       -75.4       40.6 53 Indus… "\nEs… 19029
    ##  9 1871 Catasa… Allen… PA    18109       -75.4       40.6 8507 Uni… "\nMo… 15108
    ## 10 1871 Catasa… Allen… PA    18109       -75.4       40.6 1405 Ken… "\nYo… 17408
    ## # … with 390 more rows, and 3 more variables: longitude.y <dbl>,
    ## #   latitude.y <dbl>, distance <dbl>

``` r
dn_lq_pa_mindist <- dn_lq_pa %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))
dn_lq_pa_mindist
```

    ## # A tibble: 40 × 2
    ##    address.x            closest
    ##    <chr>                  <dbl>
    ##  1 1071 Wayne Avenue      2.83 
    ##  2 1199 Louck Road        0.509
    ##  3 122 Fitz Henry Road   55.9  
    ##  4 131 Papermill Rd      78.7  
    ##  5 1346 Old Freedom Rd   22.2  
    ##  6 1395 W Chestnut St    38.4  
    ##  7 1501 Harrisburg Pike   8.26 
    ##  8 1623 Oliver Road     185.   
    ##  9 16414 Lincoln Hwy     52.3  
    ## 10 1716 E 3rd St        102.   
    ## # … with 30 more rows

``` r
dn_lq_pa%>%
  ggplot(mapping = aes(x=address.x, y=distance)) + geom_point()
```

![](lab-05_files/figure-gfm/plot7-1.png)<!-- --> There are a lot here.
Some seem close, but not as close as the other states. Lets take a
closer look

``` r
dn_lq_pa_mindist%>%
  ggplot(mapping = aes(x=address.x, y=closest)) + geom_point()
```

![](lab-05_files/figure-gfm/plot8-1.png)<!-- --> Looking at the closest,
there are some that are close. But this is probably the worst of the
four states. ### Exercise 12

I would say that Texas it probably was the most applicable. Pretty much
every Dennys had a LaQuinta super close. The Alaska ones did too - very
close - but there was only 3, so the joke is funnnier in Texas. NC and
PA were not nearly as good.
