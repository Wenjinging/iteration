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
    ## 1  5.84  3.94

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.4  3.37

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.1 0.351

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  99.9 0.800

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
    ## 1  5.84  3.94
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.4  3.37
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.1 0.351
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  99.9 0.800

## can we map?

we can map!
