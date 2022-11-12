simulations
================
2022-11-06

# Simulations

Here is the function from before

``` r
sim_mean_sd = function(n_obs, mu = 7, sigma = 4){
  
  x = rnorm(n=n_obs,mean = mu, sd = sigma)

  
tibble(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
}
```

How did we use it before?

``` r
sim_mean_sd(n_obs = 30)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   8.19      4.29

How can we use this now

let’s start with a loop!

``` r
output = vector("list", length = 100)

for (i in 1:100){
  output[[i]] = sim_mean_sd(30)
}

bind_rows(output) %>% 
  view
```

let’s use list columns instead.

``` r
sim_results_df = 
  expand_grid(
    sample_size = 30,
    iteration = 1:100) %>%
  mutate(
    estimate_df = map(sample_size, sim_mean_sd)
  ) %>%
  unnest(estimate_df)
```

``` r
sim_results_df %>%
  ggplot(aes(x= mu_hat))+ 
  geom_density()
```

<img src="simulations_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

!!!!!!!!!!!!!
