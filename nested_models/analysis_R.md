analysis - R
================
Anas Muhammad
6/7/2020

## 1\. GPA Data

``` r
# read in GPA data
gpa_data <- read_csv("../data/gpa_data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   high_gpa = col_double(),
    ##   math_sat = col_double(),
    ##   verb_sat = col_double(),
    ##   univ_gpa = col_double()
    ## )

``` r
# pair plot
gpa_data %>% ggpairs(progress = FALSE)
```

![](analysis_R_files/figure-gfm/pair%20plot-1.png)<!-- -->

We are going to try and model `univ_gpa` with the predictors `high_gpa`,
`math_sat` and `verb_sat`, represented by the following linear equation:
  
![
univ\\\_gpa\_i=\\beta\_0+\\beta\_1 high\\\_gpa\_i+ \\beta\_2
math\\\_sat\_i+ \\beta\_3 verb\\\_sat\_i+ \\varepsilon\_i
](https://latex.codecogs.com/png.latex?%0Auniv%5C_gpa_i%3D%5Cbeta_0%2B%5Cbeta_1%20high%5C_gpa_i%2B%20%5Cbeta_2%20math%5C_sat_i%2B%20%5Cbeta_3%20verb%5C_sat_i%2B%20%5Cvarepsilon_i%0A
"
univ\\_gpa_i=\\beta_0+\\beta_1 high\\_gpa_i+ \\beta_2 math\\_sat_i+ \\beta_3 verb\\_sat_i+ \\varepsilon_i
")  

Lets try to fit the linear model

``` r
model_MLR <- lm(univ_gpa~math_sat + verb_sat + high_gpa, data = gpa_data)

tidy(model_MLR)
```

    ## # A tibble: 4 x 5
    ##   term        estimate std.error statistic       p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>         <dbl>
    ## 1 (Intercept) 0.579     0.342        1.69  0.0936       
    ## 2 math_sat    0.000489  0.00102      0.479 0.633        
    ## 3 verb_sat    0.00102   0.000812     1.26  0.212        
    ## 4 high_gpa    0.545     0.0850       6.41  0.00000000460

It seems that only the `high_gpa` is statistically significant and the
rest are not.
