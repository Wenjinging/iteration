writing_functions
================
2022-11-05

## Z scores!!

Let’s compute the z-scores version of a list of numbers.

``` r
x_vec = rnorm(25,mean =7,sd=4)

(x_vec-mean(x_vec))/(sd(x_vec))
```

    ##  [1]  1.92388502 -0.42362201  1.73154529 -1.70337844 -0.60667220  0.27941681
    ##  [7] -0.06122938 -1.16380938  1.53107982 -0.55200013 -0.40681899 -1.59260152
    ## [13]  0.62295800 -0.05161559 -0.45294043  1.59862315  0.49192083  0.08310045
    ## [19]  0.35553190  0.04965151  0.07117714  0.71350743  0.01373814 -1.31502566
    ## [25] -1.13642176

Suppose you want to do this often.

``` r
z_scores = function(x){
  
  if(!is.numeric(x)){
    stop("Z scores only work for numbers")
  }
  
  if(length(x)<3){
    stop("Z scores really only work if you have thee or more numbers")
  }
  
  z = (x-mean(x))/(sd(x))
  
  z
}
```

``` r
z_scores(x_vec)

z_scores(x=1:10)
z_scores(x=rbinom(1000,1,.6))

#z_scores(x=3)
#z_scores("my name is Wenjing Yang")
```

## let’s have multiple outputs

let’s just get the mean and sd from the vector output.

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

``` r
mean_and_sd(x_vec)
mean_and_sd(1:10)
mean_and_sd(rbinom(1000,1,.5))
```

## let’s start with simulation…

``` r
x_vec = rnorm(25,mean =7,sd=4)

  
tibble(
    mean = mean(x_vec),
    sd = sd(x_vec)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  7.71  4.13

Can I do this using function? Yup

``` r
sim_mean_sd = function(n_obs, true_mean = 7, true_sd = 4){
  
  x = rnorm(n=n_obs,mean =true_mean,sd=true_sd)

  
tibble(
    mean = mean(x),
    sd = sd(x)
  )
}
```

Does it work?

``` r
sim_mean_sd(2500,10,3)
```

## Fixing bad stuff

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

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
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

let’s write a function to get reviews.

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

  reviews
}
```

let’s try with a URL

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=5"

read_page_reviews(url)
```

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

What good does this do?

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
