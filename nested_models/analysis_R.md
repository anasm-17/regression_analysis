Comparing Nested Models - R
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

We are going to try and build 3 nested models to see if adding either
`high_sat` or `verb_sat` has an impact on the model.

1)  model\_1:   
    ![
    univ\\\_gpa\_i = \\beta\_0 + \\beta\_1 high\\\_gpa\_i +
    \\varepsilon\_i
    ](https://latex.codecogs.com/png.latex?%0Auniv%5C_gpa_i%20%3D%20%5Cbeta_0%20%2B%20%5Cbeta_1%20high%5C_gpa_i%20%2B%20%5Cvarepsilon_i%0A
    "
    univ\\_gpa_i = \\beta_0 + \\beta_1 high\\_gpa_i + \\varepsilon_i
    ")  

2)  model\_2:   
    ![
    univ\\\_gpa\_i = \\beta\_0 + \\beta\_1 high\\\_gpa\_i + \\beta\_2
    math\\\_sat\_i + \\varepsilon\_i
    ](https://latex.codecogs.com/png.latex?%0Auniv%5C_gpa_i%20%3D%20%5Cbeta_0%20%2B%20%5Cbeta_1%20high%5C_gpa_i%20%2B%20%5Cbeta_2%20math%5C_sat_i%20%2B%20%5Cvarepsilon_i%0A
    "
    univ\\_gpa_i = \\beta_0 + \\beta_1 high\\_gpa_i + \\beta_2 math\\_sat_i + \\varepsilon_i
    ")  

3)  model\_3:   
    ![
    univ\\\_gpa\_i=\\beta\_0+\\beta\_1 high\\\_gpa\_i+ \\beta\_2
    verb\\\_sat\_i+ \\varepsilon\_i
    ](https://latex.codecogs.com/png.latex?%0Auniv%5C_gpa_i%3D%5Cbeta_0%2B%5Cbeta_1%20high%5C_gpa_i%2B%20%5Cbeta_2%20verb%5C_sat_i%2B%20%5Cvarepsilon_i%0A
    "
    univ\\_gpa_i=\\beta_0+\\beta_1 high\\_gpa_i+ \\beta_2 verb\\_sat_i+ \\varepsilon_i
    ")  

<!-- end list -->

``` r
model_1 <- lm(univ_gpa~high_gpa, data = gpa_data)
model_2 <- lm(univ_gpa~high_gpa + math_sat, data = gpa_data)
model_3 <- lm(univ_gpa~high_gpa + verb_sat, data = gpa_data)
```

Lets look at the ANOVA for the nested models:

``` r
# ANOVA model 1 and model 2 
# adding `math_sat`
anova(model_1, model_2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: univ_gpa ~ high_gpa
    ## Model 2: univ_gpa ~ high_gpa + math_sat
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1    103 8.1587                           
    ## 2    102 7.9511  1   0.20759 2.6631 0.1058

``` r
# ANOVA model 1 and model 3
# adding `verb_sat`
anova(model_1, model_3)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: univ_gpa ~ high_gpa
    ## Model 2: univ_gpa ~ high_gpa + verb_sat
    ##   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
    ## 1    103 8.1587                              
    ## 2    102 7.8466  1    0.3121 4.0571 0.04662 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The results above show non-significant p-value of 0.105 when adding
`math_sat` predictor and a significant p-value of 0.0466 when adding
`verb_sat` to the linear model. Therefore, additional
![\\beta\_2](https://latex.codecogs.com/png.latex?%5Cbeta_2 "\\beta_2")
coefficient from the `verb_sat` predictor is significant, whereas for
`math_sat` it is not.

``` r
tidy(model_3)
```

    ## # A tibble: 3 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  0.684    0.263         2.60 1.06e- 2
    ## 2 high_gpa     0.563    0.0766        7.35 5.07e-11
    ## 3 verb_sat     0.00127  0.000628      2.01 4.66e- 2

It is worth noting here that the p-value of the model `univ_gpa` \~
`high_gpa` + `math_sat` is equal to the p-value from the ANOVA.

Similarly for `univ_gpa` \~ `high_gpa` + `verb_sat`, the same thing can
be observed.

``` r
anova(model_3)
```

    ## Analysis of Variance Table
    ## 
    ## Response: univ_gpa
    ##            Df  Sum Sq Mean Sq  F value  Pr(>F)    
    ## high_gpa    1 12.6394 12.6394 164.3026 < 2e-16 ***
    ## verb_sat    1  0.3121  0.3121   4.0571 0.04662 *  
    ## Residuals 102  7.8466  0.0769                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

However when adding more than one predictor to the base model, this is
not the case as seen here:

``` r
model_all <- lm(univ_gpa ~ high_gpa + verb_sat + math_sat, data=gpa_data)

tidy(model_all)
```

    ## # A tibble: 4 x 5
    ##   term        estimate std.error statistic       p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>         <dbl>
    ## 1 (Intercept) 0.579     0.342        1.69  0.0936       
    ## 2 high_gpa    0.545     0.0850       6.41  0.00000000460
    ## 3 verb_sat    0.00102   0.000812     1.26  0.212        
    ## 4 math_sat    0.000489  0.00102      0.479 0.633

``` r
anova(model_1, model_all)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: univ_gpa ~ high_gpa
    ## Model 2: univ_gpa ~ high_gpa + verb_sat + math_sat
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1    103 8.1587                           
    ## 2    101 7.8288  2   0.32988 2.1279 0.1244

In order to get the p-value as obtained from the `anova()` function, we
need to compute the general regression test or the extra sum of squares
test. Below equation taken from \[1\].

  
![
SS\_{R,extra} = SS\_{R, full} - SS\_{R, reduced}
](https://latex.codecogs.com/png.latex?%0ASS_%7BR%2Cextra%7D%20%3D%20SS_%7BR%2C%20full%7D%20-%20SS_%7BR%2C%20reduced%7D%0A
"
SS_{R,extra} = SS_{R, full} - SS_{R, reduced}
")  
And the resulting F would be from \[1\]:   
![
F\_{OBS} = \\frac{SS\_{R, extra}/r}{MS\_{E, full}} 
](https://latex.codecogs.com/png.latex?%0AF_%7BOBS%7D%20%3D%20%5Cfrac%7BSS_%7BR%2C%20extra%7D%2Fr%7D%7BMS_%7BE%2C%20full%7D%7D%20%20%0A
"
F_{OBS} = \\frac{SS_{R, extra}/r}{MS_{E, full}}  
")  

``` r
# Sum of squared residuals for reduced model
preds_model_1 <- fitted(model_1)
deviation_model_1 <- (gpa_data$univ_gpa - preds_model_1)^2
SS_R_reduced <- sum(deviation_model_1)

# Sum of squared residuals for full model
preds_model_all <- fitted(model_all)
deviation_model_all <- (gpa_data$univ_gpa - preds_model_all)^2
SS_R_full <- sum(deviation_model_all)

# Sum of squared residuals, extra
SS_R_extra <- SS_R_reduced - SS_R_full


F_obs <- (SS_R_extra/2)/mean(model_all$residuals^2)

df_num <- 2
df_den <- length(gpa_data$high_gpa) - 2 - 1
p_value <- 1-pf(F_obs, df1=df_num, df2=df_den)

print(paste("F:", round(F_obs, 4)))
```

    ## [1] "F: 2.2122"

``` r
print(paste("p-value:", round(p_value, 4)))
```

    ## [1] "p-value: 0.1147"

Almost the same as from the ANOVA.

# References

\[1\]
<https://www.stat.ncsu.edu/people/bloomfield/courses/st370/Slides/MandR-ch12-sec02-06.pdf>
