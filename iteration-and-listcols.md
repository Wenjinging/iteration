iteration_and_listcols
================
2022-11-05

## lists

``` r
vec_numeric = 5:8
vec_logical = c(T,F,T,T)
```

Let’s look at a list

``` r
l = list(
  vec_numeric = 5:8,
  mat = matrix(1:8,2,4),
  vec_logical = c(T,F),
  summary = summary(rnorm(1000))
)
```

Accessing list items

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[3]]
```

    ## [1]  TRUE FALSE

``` r
l[["mat"]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

## loops

let’s write a `for`loop to take the mean and SD of four samples from
normal distribution.

``` r
list_norm = 
  list(
    a = rnorm(20,5,4),
    b = rnorm(20,-12,3),
    c = rnorm(20,17,.4),
    d = rnorm(20,100,1)
  )
```

here is my function.

``` r
mean_and_sd = function(x){

  if(!is.numeric(x)){
    stop("Z scores only work for numbers")
  }
  
  if(length(x)<3){
    stop("Z scores really only work if you have thee or more numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
}
```

let’s try to make it work.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.48  3.64

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -10.9  2.39

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.1 0.400

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  99.9 0.867

let’s a for loop instead.

``` r
output= vector("list", length = 4)

for (i in 1:4){
  output[[i]] = mean_and_sd(list_norm[[i]])
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.48  3.64
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -10.9  2.39
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.1 0.400
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  99.9 0.867

## can we map?

we can map!

``` r
map(list_norm,mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.48  3.64
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -10.9  2.39
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.1 0.400
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  99.9 0.867

so… what about other functions?

``` r
map(list_norm,summary)
```

    ## $a
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -1.289   2.131   3.878   4.481   6.254  12.730 
    ## 
    ## $b
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -18.290 -11.668 -11.022 -10.944  -9.528  -7.190 
    ## 
    ## $c
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   16.59   16.84   17.09   17.14   17.38   17.95 
    ## 
    ## $d
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   98.39   99.33   99.88   99.94  100.55  101.36

map variants?

``` r
map_dbl(list_norm,median)
```

    ##          a          b          c          d 
    ##   3.878411 -11.022210  17.089234  99.884395

``` r
map_df(list_norm,mean_and_sd)
```

    ## # A tibble: 4 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1   4.48 3.64 
    ## 2 -10.9  2.39 
    ## 3  17.1  0.400
    ## 4  99.9  0.867

## list columns…

``` r
listcol_df =
  tibble(
    name = c("a","b","c","d"),
    norm = list_norm
  )

listcol_df[["norm"]]
```

    ## $a
    ##  [1]  8.0536822  3.9006845  3.1331620 -1.2892849  3.6217878  7.9279715
    ##  [7]  0.9045724  5.1467457  1.5198174 12.7297353  3.0513085  2.2891863
    ## [13]  6.1184269 11.9438171  1.6561135 -0.2252860  4.4375516  3.8561371
    ## [19]  4.1871946  6.6622802
    ## 
    ## $b
    ##  [1]  -7.189773 -10.218789 -12.866771  -8.761223 -11.010956 -12.209159
    ##  [7] -11.033464 -11.002954 -13.619168 -11.301565  -9.418582 -18.290276
    ## [13] -11.092617 -11.890622 -11.594457  -7.354335 -11.176666 -10.291295
    ## [19]  -9.565055  -9.001553
    ## 
    ## $c
    ##  [1] 17.44696 16.85973 17.14528 17.64567 17.37627 17.23254 16.87731 17.03319
    ##  [9] 16.66107 16.96834 16.62571 17.40485 16.78445 17.03108 16.59183 17.87364
    ## [17] 17.25165 16.68533 17.94857 17.26857
    ## 
    ## $d
    ##  [1] 101.05325 101.15465 100.50752 100.88771 100.68774 100.25539  98.60113
    ##  [8]  99.39742  99.94572  99.04335  99.12727  99.81228  99.82307  99.54351
    ## [15] 100.50874 101.35624  99.57199  98.91335 100.12653  98.38871

``` r
output = map(listcol_df[["norm"]],mean_and_sd)
```

can we add list columns and then what

``` r
listcol_df %>% 
  mutate(
    m_sd = map(norm, mean_and_sd) 
  )%>%
  select(-norm)
```

    ## # A tibble: 4 × 2
    ##   name  m_sd            
    ##   <chr> <named list>    
    ## 1 a     <tibble [1 × 2]>
    ## 2 b     <tibble [1 × 2]>
    ## 3 c     <tibble [1 × 2]>
    ## 4 d     <tibble [1 × 2]>

## what about something more realistic…

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2022-10-11 00:57:46 (8.406)

    ## file min/max dates: 1869-01-01 / 2022-09-30

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2022-10-11 00:57:55 (1.699)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2022-10-11 00:57:58 (0.952)

    ## file min/max dates: 1999-09-01 / 2022-10-31

let’s nest within weather stations…

``` r
weather_nest_df = 
weather_df%>%
  nest(data = date:tmin)
```

Really a list column!

``` r
weather_nest_df[["data"]]
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
weather_nest_df[["data"]][[1]]
```

    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows

``` r
lm(tmax~tmin, data = weather_nest_df[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
lm(tmax~tmin, data = weather_nest_df[["data"]][[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509

``` r
lm(tmax~tmin, data = weather_nest_df[["data"]][[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

let’s write a short lil ol function

``` r
weather_lm = function(df){
  lm(tmax~tmin, data = df)
}

weather_lm(weather_nest_df[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nest_df[["data"]],weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

can I do all this in a tidy way

``` r
weather_nest_df %>%
  mutate(
    model = map(data,weather_lm)
  )
```

    ## # A tibble: 3 × 4
    ##   name           id          data               model 
    ##   <chr>          <chr>       <list>             <list>
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>  
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>  
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

unnesting

``` r
weather_nest_df %>%
  unnest(data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

## Napoleon

Here’s my scraping function that works for single page

``` r
read_page_reviews = function(url){
  
  dynamite_html = read_html(url)

  review_titles = 
    dynamite_html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()

  review_stars = 
    dynamite_html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()

  review_text = 
    dynamite_html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim() %>%
    str_subset("The media could not be loaded.", negate = TRUE) %>% 
    str_subset("^$", negate = TRUE)
  

  reviews = 
    tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text)
}
```

what we did last time

``` r
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

vec_url = str_c(base_url,c(1,2,3,4,5))

dynamite_review = 
  bind_rows(
    read_page_reviews(vec_url[1]),
    read_page_reviews(vec_url[2]),
    read_page_reviews(vec_url[3]),
    read_page_reviews(vec_url[4]),
    read_page_reviews(vec_url[5])
)
dynamite_review
```

    ## # A tibble: 50 × 3
    ##    title                                      stars text                        
    ##    <chr>                                      <dbl> <chr>                       
    ##  1 Lol hey it’s Napoleon. What’s not to love…     5 Vote for Pedro              
    ##  2 Still the best                                 5 Completely stupid, absolute…
    ##  3 70’s and 80’s Schtick Comedy                   5 …especially funny if you ha…
    ##  4 Amazon Censorship                              5 I hope Amazon does not cens…
    ##  5 Watch to say you did                           3 I know it's supposed to be …
    ##  6 Best Movie Ever!                               5 We just love this movie and…
    ##  7 Quirky                                         5 Good family film            
    ##  8 Funny movie - can't play it !                  1 Sony 4k player won't even r…
    ##  9 A brilliant story about teenage life           5 Napoleon Dynamite delivers …
    ## 10 HUHYAH                                         5 Spicy                       
    ## # … with 40 more rows

``` r
map(vec_url,read_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
    ##    title                                      stars text                        
    ##    <chr>                                      <dbl> <chr>                       
    ##  1 Lol hey it’s Napoleon. What’s not to love…     5 Vote for Pedro              
    ##  2 Still the best                                 5 Completely stupid, absolute…
    ##  3 70’s and 80’s Schtick Comedy                   5 …especially funny if you ha…
    ##  4 Amazon Censorship                              5 I hope Amazon does not cens…
    ##  5 Watch to say you did                           3 I know it's supposed to be …
    ##  6 Best Movie Ever!                               5 We just love this movie and…
    ##  7 Quirky                                         5 Good family film            
    ##  8 Funny movie - can't play it !                  1 Sony 4k player won't even r…
    ##  9 A brilliant story about teenage life           5 Napoleon Dynamite delivers …
    ## 10 HUHYAH                                         5 Spicy                       
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                                         stars text                     
    ##    <chr>                                         <dbl> <chr>                    
    ##  1 Cult Classic                                      4 Takes a time or two to f…
    ##  2 Sweet                                             5 Timeless Movie. My Grand…
    ##  3 Cute                                              4 Fun                      
    ##  4 great collectible                                 5 one of the greatest movi…
    ##  5 Iconic, hilarious flick ! About friend ship .     5 Who doesn’t love this mo…
    ##  6 Funny                                             5 Me and my dad watched th…
    ##  7 Low budget but okay                               3 This has been a classic …
    ##  8 Disappointing                                     2 We tried to like this, b…
    ##  9 Favorite movie 🍿                                 5 This is one of my favori…
    ## 10 none                                              5 this movie was great Nap…
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 Great movie                                                           5 "Vot…
    ##  2 Get this to improve your nunchuck and bowstaff skills. Dancing i…     5 "Got…
    ##  3 Incredible Movie                                                      5 "Fun…
    ##  4 Always loved this movie!                                              5 "I h…
    ##  5 Great movie                                                           5 "Bou…
    ##  6 The case was damaged                                                  3 "It …
    ##  7 It’s classic                                                          5 "Cle…
    ##  8 Irreverent comedy                                                     5 "If …
    ##  9 Great classic!                                                        5 "Fun…
    ## 10 Most Awesomsomest Movie EVER!!!                                       5 "Thi…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 Always a favorite                                                     5 "I r…
    ##  2 It’s not working the disc keeps showing error when I tried other…     1 "It’…
    ##  3 Gosh!                                                                 5 "Eve…
    ##  4 An Acquired Taste                                                     1 "Thi…
    ##  5 What is this ?                                                        4 "Nic…
    ##  6 Napoleon Dynamite                                                     2 "I w…
    ##  7 Great movie                                                           5 "Gre…
    ##  8 Good movie                                                            5 "Goo…
    ##  9 Came as Described                                                     5 "Cam…
    ## 10 Oddly on my list of keepers.                                          5 "Goo…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 Low budget fun                                                        5 "Odd…
    ##  2 On a scale of 1 to 10 this rates a minus                              1 "Thi…
    ##  3 I always wondered...                                                  5 "wha…
    ##  4 Audio/video not synced                                                1 "I t…
    ##  5 Kind of feels like only a bully would actually laugh at this...       1 "...…
    ##  6 movie                                                                 5 "goo…
    ##  7 An Overdose of Comical Cringe                                         5 "Exc…
    ##  8 Glad I never wasted money on this                                     2 "I r…
    ##  9 A little disappointed                                                 3 "The…
    ## 10 An (almost) gem. Brought me back to the sweet awkwardness of hig…     5 "To …

``` r
napoleon_reviews = 
  tibble(
    page = 1:5,
    page_url= str_c(base_url,page)
  ) %>%
  mutate(
    review = map(page_url, read_page_reviews)
  )

napoleon_reviews %>%
  select(-page_url) %>%
  unnest(review)
```

    ## # A tibble: 50 × 4
    ##     page title                                      stars text                  
    ##    <int> <chr>                                      <dbl> <chr>                 
    ##  1     1 Lol hey it’s Napoleon. What’s not to love…     5 Vote for Pedro        
    ##  2     1 Still the best                                 5 Completely stupid, ab…
    ##  3     1 70’s and 80’s Schtick Comedy                   5 …especially funny if …
    ##  4     1 Amazon Censorship                              5 I hope Amazon does no…
    ##  5     1 Watch to say you did                           3 I know it's supposed …
    ##  6     1 Best Movie Ever!                               5 We just love this mov…
    ##  7     1 Quirky                                         5 Good family film      
    ##  8     1 Funny movie - can't play it !                  1 Sony 4k player won't …
    ##  9     1 A brilliant story about teenage life           5 Napoleon Dynamite del…
    ## 10     1 HUHYAH                                         5 Spicy                 
    ## # … with 40 more rows
