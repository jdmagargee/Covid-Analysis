Predictive Analytics Project
================
Zach Austin, James Brunette, Jack Magargee
4/19/2021

Load Packages, Clear Data

``` r
library(tidyverse)
library(ggplot2)
```

Load Data

``` r
# SET WORKING DIRECTORY HERE

setwd("~/Desktop/Covid-Analysis")

# loading in the one file you need, naming it master_data
master_data <- read_csv("master .csv")
```

Data cleaning and manipulation

``` r
master_data <- master_data %>% 
   mutate(pop_death_rate = deaths/total_population) %>% 
   select('state',  'county',   'deaths',   'cases',    'death_rate',   'total_population','pop_death_rate',    'lat',  'lon',  'population_density_per_sqmi',  'percent_fair_or_poor_health',  'percent_smokers',  'percent_adults_with_obesity',  'food_environment_index',   'percent_physically_inactive',  'percent_with_access_to_exercise_opportunities',    'percent_excessive_drinking',   'percent_uninsured',    'primary_care_physicians_rate',     'preventable_hospitalization_rate',     'percent_vaccinated',   'percent_children_in_poverty',  'percent_single_parent_households_CHR',     'injury_death_rate',    'overcrowding',     'inadequate_facilities',    'life_expectancy',  'percent_adults_with_diabetes',     'hiv_prevalence_rate',  'percent_food_insecure',    'percent_limited_access_to_healthy_foods',  'median_household_income',  'percent_less_than_18_years_of_age',    'percent_65_and_over',  'percent_rural',    'percent_below_poverty',    'percent_disabled') 
```

Correlation
matrix

``` r
write.csv(cor(master_data[,3:31],use = "pairwise.complete.obs"),"correlations.csv",row.names = F)
```

Select primary components and prep for forecasting

``` r
library(forecast)

modeling_data = master_data %>% 
   select(-c("lat","lon",'deaths','cases','state','county','pop_death_rate',
             'population_density_per_sqmi'))

# partition data into training and validation data

RNGkind(sample.kind = 'Rounding')
set.seed(1)
training_rows = sample(row.names(modeling_data), dim(modeling_data)[1]*.75)
training_df = modeling_data[training_rows,]
validation_rows = setdiff(row.names(modeling_data), training_rows)
valid_df = modeling_data[validation_rows,]

# MAKING A FULL QUASIBINOMIAL MODEL WITH ALL THE PREDICTORS

full.logit = glm(death_rate ~., data = training_df, family="quasibinomial")

summary(full.logit)
```

    ## 
    ## Call:
    ## glm(formula = death_rate ~ ., family = "quasibinomial", data = training_df)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -0.19718  -0.05596  -0.01561   0.03133   0.43471  
    ## 
    ## Coefficients:
    ##                                                 Estimate Std. Error t value
    ## (Intercept)                                   -1.010e+01  5.681e+00  -1.777
    ## total_population                               4.574e-08  3.045e-08   1.502
    ## percent_fair_or_poor_health                    1.901e-02  8.917e-03   2.132
    ## percent_smokers                               -5.589e-02  8.920e-03  -6.266
    ## percent_adults_with_obesity                   -6.938e-03  3.944e-03  -1.759
    ## food_environment_index                         1.024e+00  5.193e-01   1.972
    ## percent_physically_inactive                    1.089e-02  3.834e-03   2.841
    ## percent_with_access_to_exercise_opportunities -2.326e-03  1.047e-03  -2.222
    ## percent_excessive_drinking                     1.105e-02  7.970e-03   1.386
    ## percent_uninsured                              1.144e-02  3.999e-03   2.861
    ## primary_care_physicians_rate                   1.063e-04  6.635e-04   0.160
    ## preventable_hospitalization_rate               4.130e-05  1.039e-05   3.974
    ## percent_vaccinated                             3.283e-03  2.269e-03   1.447
    ## percent_children_in_poverty                    2.038e-02  4.751e-03   4.290
    ## percent_single_parent_households_CHR           9.349e-03  2.703e-03   3.459
    ## injury_death_rate                             -5.211e-03  1.023e-03  -5.095
    ## overcrowding                                  -1.029e-03  1.269e-02  -0.081
    ## inadequate_facilities                          2.454e-02  2.025e-02   1.212
    ## life_expectancy                               -8.010e-02  1.187e-02  -6.748
    ## percent_adults_with_diabetes                   7.137e-03  5.081e-03   1.404
    ## hiv_prevalence_rate                            4.440e-04  1.008e-04   4.405
    ## percent_food_insecure                          1.791e-01  9.779e-02   1.832
    ## percent_limited_access_to_healthy_foods        8.900e-02  4.612e-02   1.930
    ## median_household_income                        1.317e-05  2.315e-06   5.690
    ## percent_less_than_18_years_of_age             -8.621e-03  7.914e-03  -1.089
    ## percent_65_and_over                            4.054e-02  6.388e-03   6.346
    ## percent_rural                                  7.380e-04  8.955e-04   0.824
    ## percent_below_poverty                         -3.041e-03  6.387e-03  -0.476
    ## percent_disabled                              -1.279e-02  6.360e-03  -2.011
    ##                                               Pr(>|t|)    
    ## (Intercept)                                   0.075720 .  
    ## total_population                              0.133256    
    ## percent_fair_or_poor_health                   0.033184 *  
    ## percent_smokers                               4.76e-10 ***
    ## percent_adults_with_obesity                   0.078698 .  
    ## food_environment_index                        0.048807 *  
    ## percent_physically_inactive                   0.004558 ** 
    ## percent_with_access_to_exercise_opportunities 0.026437 *  
    ## percent_excessive_drinking                    0.165910    
    ## percent_uninsured                             0.004275 ** 
    ## primary_care_physicians_rate                  0.872700    
    ## preventable_hospitalization_rate              7.37e-05 ***
    ## percent_vaccinated                            0.148097    
    ## percent_children_in_poverty                   1.90e-05 ***
    ## percent_single_parent_households_CHR          0.000557 ***
    ## injury_death_rate                             3.91e-07 ***
    ## overcrowding                                  0.935371    
    ## inadequate_facilities                         0.225756    
    ## life_expectancy                               2.10e-11 ***
    ## percent_adults_with_diabetes                  0.160382    
    ## hiv_prevalence_rate                           1.13e-05 ***
    ## percent_food_insecure                         0.067160 .  
    ## percent_limited_access_to_healthy_foods       0.053807 .  
    ## median_household_income                       1.51e-08 ***
    ## percent_less_than_18_years_of_age             0.276140    
    ## percent_65_and_over                           2.87e-10 ***
    ## percent_rural                                 0.409950    
    ## percent_below_poverty                         0.634016    
    ## percent_disabled                              0.044543 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for quasibinomial family taken to be 0.006508162)
    ## 
    ##     Null deviance: 13.0677  on 1604  degrees of freedom
    ## Residual deviance:  9.2638  on 1576  degrees of freedom
    ##   (607 observations deleted due to missingness)
    ## AIC: NA
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
length(names(full.logit$coefficients))
```

    ## [1] 29

``` r
predictions = predict(full.logit, newdata=valid_df, type="response")

add_column(valid_df, predictions, .after="death_rate")
```

    ## # A tibble: 738 x 30
    ##    death_rate predictions total_population percent_fair_or… percent_smokers
    ##         <dbl>       <dbl>            <dbl>            <dbl>           <dbl>
    ##  1    0.0281       0.0585            10552             31.0            22.9
    ##  2    0.00885      0.0200            50991             19.4            17.4
    ##  3    0.0138       0.0258            54377             20.9            18.7
    ##  4    0.0205       0.0429            12697             28.6            21.1
    ##  5    0.00770      0.0279            17187             21.4            18.4
    ##  6    0.00643      0.0225            52608             23.2            20.0
    ##  7    0.00995      0.0175            90257             20.1            18.2
    ##  8    0.0362       0.0336            10565             30.4            22.6
    ##  9    0.0240       0.0219            30239             22.3            19.1
    ## 10    0.0129       0.0240            21975             30.0            23.3
    ## # … with 728 more rows, and 25 more variables:
    ## #   percent_adults_with_obesity <dbl>, food_environment_index <dbl>,
    ## #   percent_physically_inactive <dbl>,
    ## #   percent_with_access_to_exercise_opportunities <dbl>,
    ## #   percent_excessive_drinking <dbl>, percent_uninsured <dbl>,
    ## #   primary_care_physicians_rate <dbl>, preventable_hospitalization_rate <dbl>,
    ## #   percent_vaccinated <dbl>, percent_children_in_poverty <dbl>,
    ## #   percent_single_parent_households_CHR <dbl>, injury_death_rate <dbl>,
    ## #   overcrowding <dbl>, inadequate_facilities <dbl>, life_expectancy <dbl>,
    ## #   percent_adults_with_diabetes <dbl>, hiv_prevalence_rate <dbl>,
    ## #   percent_food_insecure <dbl>, percent_limited_access_to_healthy_foods <dbl>,
    ## #   median_household_income <dbl>, percent_less_than_18_years_of_age <dbl>,
    ## #   percent_65_and_over <dbl>, percent_rural <dbl>,
    ## #   percent_below_poverty <dbl>, percent_disabled <dbl>

``` r
accuracy(predictions, valid_df$death_rate)
```

    ##                    ME       RMSE         MAE  MPE MAPE
    ## Test set -0.001347278 0.01134669 0.008154835 -Inf  Inf

Build model

``` r
# EXHAUSTIVE MODEL

# RNGkind(sample.kind = 'Rounding')
# set.seed(1)
# 
# training_rows = sample(row.names(modeling_data), dim(modeling_data)[1]*.75)
# training_df = modeling_data[training_rows,]
# validation_rows = setdiff(row.names(modeling_data), training_rows)
# valid_df = modeling_data[validation_rows,]

library(leaps)
search <- regsubsets(death_rate ~ ., data = modeling_data, nbest = 1, 
                     nvmax = dim(modeling_data)[2],method = "exhaustive")
sum <- summary(search)
sum$which
```

    ##    (Intercept) total_population percent_fair_or_poor_health percent_smokers
    ## 1         TRUE            FALSE                       FALSE           FALSE
    ## 2         TRUE            FALSE                       FALSE           FALSE
    ## 3         TRUE            FALSE                       FALSE            TRUE
    ## 4         TRUE            FALSE                       FALSE            TRUE
    ## 5         TRUE            FALSE                       FALSE            TRUE
    ## 6         TRUE            FALSE                       FALSE            TRUE
    ## 7         TRUE            FALSE                       FALSE            TRUE
    ## 8         TRUE            FALSE                       FALSE            TRUE
    ## 9         TRUE            FALSE                       FALSE            TRUE
    ## 10        TRUE            FALSE                       FALSE            TRUE
    ## 11        TRUE            FALSE                       FALSE            TRUE
    ## 12        TRUE            FALSE                       FALSE            TRUE
    ## 13        TRUE            FALSE                       FALSE            TRUE
    ## 14        TRUE            FALSE                       FALSE            TRUE
    ## 15        TRUE            FALSE                       FALSE            TRUE
    ## 16        TRUE            FALSE                       FALSE            TRUE
    ## 17        TRUE            FALSE                       FALSE            TRUE
    ## 18        TRUE            FALSE                       FALSE            TRUE
    ## 19        TRUE             TRUE                       FALSE            TRUE
    ## 20        TRUE             TRUE                       FALSE            TRUE
    ## 21        TRUE            FALSE                        TRUE            TRUE
    ## 22        TRUE             TRUE                        TRUE            TRUE
    ## 23        TRUE             TRUE                        TRUE            TRUE
    ## 24        TRUE             TRUE                        TRUE            TRUE
    ## 25        TRUE             TRUE                        TRUE            TRUE
    ## 26        TRUE             TRUE                        TRUE            TRUE
    ## 27        TRUE             TRUE                        TRUE            TRUE
    ## 28        TRUE             TRUE                        TRUE            TRUE
    ##    percent_adults_with_obesity food_environment_index
    ## 1                        FALSE                  FALSE
    ## 2                        FALSE                  FALSE
    ## 3                        FALSE                  FALSE
    ## 4                        FALSE                  FALSE
    ## 5                        FALSE                  FALSE
    ## 6                        FALSE                  FALSE
    ## 7                        FALSE                  FALSE
    ## 8                        FALSE                  FALSE
    ## 9                        FALSE                  FALSE
    ## 10                       FALSE                  FALSE
    ## 11                       FALSE                  FALSE
    ## 12                       FALSE                  FALSE
    ## 13                       FALSE                  FALSE
    ## 14                       FALSE                  FALSE
    ## 15                       FALSE                  FALSE
    ## 16                       FALSE                  FALSE
    ## 17                        TRUE                  FALSE
    ## 18                        TRUE                  FALSE
    ## 19                        TRUE                  FALSE
    ## 20                        TRUE                  FALSE
    ## 21                        TRUE                  FALSE
    ## 22                        TRUE                  FALSE
    ## 23                        TRUE                   TRUE
    ## 24                        TRUE                   TRUE
    ## 25                        TRUE                   TRUE
    ## 26                        TRUE                   TRUE
    ## 27                        TRUE                   TRUE
    ## 28                        TRUE                   TRUE
    ##    percent_physically_inactive percent_with_access_to_exercise_opportunities
    ## 1                        FALSE                                         FALSE
    ## 2                        FALSE                                         FALSE
    ## 3                        FALSE                                         FALSE
    ## 4                         TRUE                                         FALSE
    ## 5                         TRUE                                         FALSE
    ## 6                        FALSE                                         FALSE
    ## 7                        FALSE                                         FALSE
    ## 8                        FALSE                                          TRUE
    ## 9                        FALSE                                          TRUE
    ## 10                       FALSE                                          TRUE
    ## 11                       FALSE                                          TRUE
    ## 12                       FALSE                                          TRUE
    ## 13                       FALSE                                          TRUE
    ## 14                        TRUE                                          TRUE
    ## 15                        TRUE                                          TRUE
    ## 16                        TRUE                                          TRUE
    ## 17                        TRUE                                          TRUE
    ## 18                        TRUE                                          TRUE
    ## 19                        TRUE                                          TRUE
    ## 20                        TRUE                                          TRUE
    ## 21                        TRUE                                          TRUE
    ## 22                        TRUE                                          TRUE
    ## 23                        TRUE                                          TRUE
    ## 24                        TRUE                                          TRUE
    ## 25                        TRUE                                          TRUE
    ## 26                        TRUE                                          TRUE
    ## 27                        TRUE                                          TRUE
    ## 28                        TRUE                                          TRUE
    ##    percent_excessive_drinking percent_uninsured primary_care_physicians_rate
    ## 1                       FALSE             FALSE                        FALSE
    ## 2                       FALSE             FALSE                        FALSE
    ## 3                       FALSE             FALSE                        FALSE
    ## 4                       FALSE             FALSE                        FALSE
    ## 5                       FALSE             FALSE                        FALSE
    ## 6                       FALSE             FALSE                        FALSE
    ## 7                       FALSE             FALSE                        FALSE
    ## 8                       FALSE             FALSE                        FALSE
    ## 9                       FALSE             FALSE                        FALSE
    ## 10                      FALSE              TRUE                        FALSE
    ## 11                      FALSE              TRUE                        FALSE
    ## 12                      FALSE              TRUE                        FALSE
    ## 13                      FALSE              TRUE                        FALSE
    ## 14                      FALSE              TRUE                        FALSE
    ## 15                      FALSE              TRUE                        FALSE
    ## 16                      FALSE              TRUE                        FALSE
    ## 17                      FALSE              TRUE                        FALSE
    ## 18                      FALSE              TRUE                        FALSE
    ## 19                      FALSE              TRUE                        FALSE
    ## 20                      FALSE              TRUE                        FALSE
    ## 21                       TRUE              TRUE                        FALSE
    ## 22                       TRUE              TRUE                        FALSE
    ## 23                       TRUE              TRUE                        FALSE
    ## 24                       TRUE              TRUE                        FALSE
    ## 25                       TRUE              TRUE                        FALSE
    ## 26                       TRUE              TRUE                        FALSE
    ## 27                       TRUE              TRUE                         TRUE
    ## 28                       TRUE              TRUE                         TRUE
    ##    preventable_hospitalization_rate percent_vaccinated
    ## 1                             FALSE              FALSE
    ## 2                             FALSE              FALSE
    ## 3                             FALSE              FALSE
    ## 4                             FALSE              FALSE
    ## 5                             FALSE              FALSE
    ## 6                             FALSE              FALSE
    ## 7                             FALSE              FALSE
    ## 8                             FALSE              FALSE
    ## 9                              TRUE              FALSE
    ## 10                            FALSE              FALSE
    ## 11                             TRUE              FALSE
    ## 12                             TRUE              FALSE
    ## 13                             TRUE              FALSE
    ## 14                             TRUE              FALSE
    ## 15                             TRUE              FALSE
    ## 16                             TRUE              FALSE
    ## 17                             TRUE              FALSE
    ## 18                             TRUE              FALSE
    ## 19                             TRUE              FALSE
    ## 20                             TRUE               TRUE
    ## 21                             TRUE               TRUE
    ## 22                             TRUE               TRUE
    ## 23                             TRUE               TRUE
    ## 24                             TRUE               TRUE
    ## 25                             TRUE               TRUE
    ## 26                             TRUE               TRUE
    ## 27                             TRUE               TRUE
    ## 28                             TRUE               TRUE
    ##    percent_children_in_poverty percent_single_parent_households_CHR
    ## 1                         TRUE                                FALSE
    ## 2                         TRUE                                FALSE
    ## 3                         TRUE                                FALSE
    ## 4                         TRUE                                FALSE
    ## 5                         TRUE                                FALSE
    ## 6                         TRUE                                FALSE
    ## 7                         TRUE                                FALSE
    ## 8                         TRUE                                FALSE
    ## 9                         TRUE                                FALSE
    ## 10                        TRUE                                 TRUE
    ## 11                        TRUE                                 TRUE
    ## 12                        TRUE                                 TRUE
    ## 13                        TRUE                                 TRUE
    ## 14                        TRUE                                 TRUE
    ## 15                        TRUE                                 TRUE
    ## 16                        TRUE                                 TRUE
    ## 17                        TRUE                                 TRUE
    ## 18                        TRUE                                 TRUE
    ## 19                        TRUE                                 TRUE
    ## 20                        TRUE                                 TRUE
    ## 21                        TRUE                                 TRUE
    ## 22                        TRUE                                 TRUE
    ## 23                        TRUE                                 TRUE
    ## 24                        TRUE                                 TRUE
    ## 25                        TRUE                                 TRUE
    ## 26                        TRUE                                 TRUE
    ## 27                        TRUE                                 TRUE
    ## 28                        TRUE                                 TRUE
    ##    injury_death_rate overcrowding inadequate_facilities life_expectancy
    ## 1              FALSE        FALSE                 FALSE           FALSE
    ## 2              FALSE        FALSE                 FALSE           FALSE
    ## 3              FALSE        FALSE                 FALSE           FALSE
    ## 4              FALSE        FALSE                 FALSE           FALSE
    ## 5              FALSE        FALSE                 FALSE           FALSE
    ## 6               TRUE        FALSE                 FALSE            TRUE
    ## 7               TRUE        FALSE                 FALSE            TRUE
    ## 8               TRUE        FALSE                 FALSE            TRUE
    ## 9               TRUE        FALSE                 FALSE            TRUE
    ## 10              TRUE        FALSE                 FALSE            TRUE
    ## 11              TRUE        FALSE                 FALSE            TRUE
    ## 12              TRUE        FALSE                 FALSE            TRUE
    ## 13              TRUE        FALSE                 FALSE            TRUE
    ## 14              TRUE        FALSE                 FALSE            TRUE
    ## 15              TRUE        FALSE                 FALSE            TRUE
    ## 16              TRUE        FALSE                  TRUE            TRUE
    ## 17              TRUE        FALSE                 FALSE            TRUE
    ## 18              TRUE        FALSE                  TRUE            TRUE
    ## 19              TRUE        FALSE                  TRUE            TRUE
    ## 20              TRUE        FALSE                  TRUE            TRUE
    ## 21              TRUE        FALSE                  TRUE            TRUE
    ## 22              TRUE        FALSE                  TRUE            TRUE
    ## 23              TRUE        FALSE                  TRUE            TRUE
    ## 24              TRUE        FALSE                  TRUE            TRUE
    ## 25              TRUE         TRUE                  TRUE            TRUE
    ## 26              TRUE         TRUE                  TRUE            TRUE
    ## 27              TRUE         TRUE                  TRUE            TRUE
    ## 28              TRUE         TRUE                  TRUE            TRUE
    ##    percent_adults_with_diabetes hiv_prevalence_rate percent_food_insecure
    ## 1                         FALSE               FALSE                 FALSE
    ## 2                         FALSE                TRUE                 FALSE
    ## 3                         FALSE                TRUE                 FALSE
    ## 4                         FALSE                TRUE                 FALSE
    ## 5                         FALSE                TRUE                 FALSE
    ## 6                         FALSE                TRUE                 FALSE
    ## 7                         FALSE                TRUE                 FALSE
    ## 8                         FALSE                TRUE                 FALSE
    ## 9                         FALSE                TRUE                 FALSE
    ## 10                        FALSE                TRUE                 FALSE
    ## 11                        FALSE                TRUE                 FALSE
    ## 12                        FALSE                TRUE                  TRUE
    ## 13                        FALSE                TRUE                  TRUE
    ## 14                        FALSE                TRUE                  TRUE
    ## 15                        FALSE                TRUE                  TRUE
    ## 16                        FALSE                TRUE                  TRUE
    ## 17                         TRUE                TRUE                  TRUE
    ## 18                         TRUE                TRUE                  TRUE
    ## 19                         TRUE                TRUE                  TRUE
    ## 20                         TRUE                TRUE                  TRUE
    ## 21                         TRUE                TRUE                  TRUE
    ## 22                         TRUE                TRUE                  TRUE
    ## 23                         TRUE                TRUE                 FALSE
    ## 24                         TRUE                TRUE                  TRUE
    ## 25                         TRUE                TRUE                  TRUE
    ## 26                         TRUE                TRUE                  TRUE
    ## 27                         TRUE                TRUE                  TRUE
    ## 28                         TRUE                TRUE                  TRUE
    ##    percent_limited_access_to_healthy_foods median_household_income
    ## 1                                    FALSE                   FALSE
    ## 2                                    FALSE                   FALSE
    ## 3                                    FALSE                   FALSE
    ## 4                                    FALSE                   FALSE
    ## 5                                    FALSE                   FALSE
    ## 6                                    FALSE                   FALSE
    ## 7                                    FALSE                    TRUE
    ## 8                                    FALSE                    TRUE
    ## 9                                    FALSE                    TRUE
    ## 10                                   FALSE                    TRUE
    ## 11                                   FALSE                    TRUE
    ## 12                                   FALSE                    TRUE
    ## 13                                   FALSE                    TRUE
    ## 14                                   FALSE                    TRUE
    ## 15                                   FALSE                    TRUE
    ## 16                                   FALSE                    TRUE
    ## 17                                   FALSE                    TRUE
    ## 18                                   FALSE                    TRUE
    ## 19                                   FALSE                    TRUE
    ## 20                                   FALSE                    TRUE
    ## 21                                   FALSE                    TRUE
    ## 22                                   FALSE                    TRUE
    ## 23                                    TRUE                    TRUE
    ## 24                                    TRUE                    TRUE
    ## 25                                    TRUE                    TRUE
    ## 26                                    TRUE                    TRUE
    ## 27                                    TRUE                    TRUE
    ## 28                                    TRUE                    TRUE
    ##    percent_less_than_18_years_of_age percent_65_and_over percent_rural
    ## 1                              FALSE               FALSE         FALSE
    ## 2                              FALSE               FALSE         FALSE
    ## 3                              FALSE               FALSE         FALSE
    ## 4                              FALSE               FALSE         FALSE
    ## 5                              FALSE                TRUE         FALSE
    ## 6                              FALSE                TRUE         FALSE
    ## 7                              FALSE                TRUE         FALSE
    ## 8                              FALSE                TRUE         FALSE
    ## 9                              FALSE                TRUE         FALSE
    ## 10                             FALSE                TRUE         FALSE
    ## 11                             FALSE                TRUE         FALSE
    ## 12                             FALSE                TRUE         FALSE
    ## 13                             FALSE                TRUE         FALSE
    ## 14                             FALSE                TRUE         FALSE
    ## 15                              TRUE                TRUE         FALSE
    ## 16                              TRUE                TRUE         FALSE
    ## 17                              TRUE                TRUE         FALSE
    ## 18                              TRUE                TRUE         FALSE
    ## 19                              TRUE                TRUE         FALSE
    ## 20                              TRUE                TRUE         FALSE
    ## 21                              TRUE                TRUE         FALSE
    ## 22                              TRUE                TRUE         FALSE
    ## 23                              TRUE                TRUE         FALSE
    ## 24                              TRUE                TRUE         FALSE
    ## 25                              TRUE                TRUE         FALSE
    ## 26                              TRUE                TRUE         FALSE
    ## 27                              TRUE                TRUE         FALSE
    ## 28                              TRUE                TRUE          TRUE
    ##    percent_below_poverty percent_disabled
    ## 1                  FALSE            FALSE
    ## 2                  FALSE            FALSE
    ## 3                  FALSE            FALSE
    ## 4                  FALSE            FALSE
    ## 5                  FALSE            FALSE
    ## 6                  FALSE            FALSE
    ## 7                  FALSE            FALSE
    ## 8                  FALSE            FALSE
    ## 9                  FALSE            FALSE
    ## 10                 FALSE            FALSE
    ## 11                 FALSE            FALSE
    ## 12                 FALSE            FALSE
    ## 13                 FALSE             TRUE
    ## 14                 FALSE             TRUE
    ## 15                 FALSE             TRUE
    ## 16                 FALSE             TRUE
    ## 17                 FALSE             TRUE
    ## 18                 FALSE             TRUE
    ## 19                 FALSE             TRUE
    ## 20                 FALSE             TRUE
    ## 21                 FALSE             TRUE
    ## 22                 FALSE             TRUE
    ## 23                 FALSE             TRUE
    ## 24                 FALSE             TRUE
    ## 25                 FALSE             TRUE
    ## 26                  TRUE             TRUE
    ## 27                  TRUE             TRUE
    ## 28                  TRUE             TRUE

``` r
sum$rsq
```

    ##  [1] 0.1004299 0.1337090 0.1567815 0.1733010 0.1849491 0.2040347 0.2185780
    ##  [8] 0.2267531 0.2310593 0.2356953 0.2403556 0.2426255 0.2443092 0.2461228
    ## [15] 0.2480269 0.2488836 0.2497081 0.2505763 0.2511735 0.2517187 0.2521418
    ## [22] 0.2527159 0.2530076 0.2535254 0.2536163 0.2536482 0.2536768 0.2536834

``` r
sum$adjr2
```

    ##  [1] 0.1000041 0.1328886 0.1555832 0.1717338 0.1830168 0.2017692 0.2159819
    ##  [8] 0.2238158 0.2277716 0.2320627 0.2363822 0.2383018 0.2396333 0.2410970
    ## [15] 0.2426531 0.2431553 0.2436257 0.2441404 0.2443823 0.2445718 0.2446382
    ## [22] 0.2448573 0.2447911 0.2449534 0.2446840 0.2443546 0.2440214 0.2436657

``` r
sum$Cp
```

    ## NULL

``` r
Adjr2=which.max(sum$adjr2) #find how many variables needed to get max adjusted r2
Adjr2
```

    ## [1] 24

``` r
# getting rid of the variables not needed in the exhaustive search model

exhaustive_data = master_data %>% 
  select(-c('primary_care_physicians_rate','overcrowding','percent_rural',
            'percent_below_poverty')) %>% 
  select(-c("lat","lon",'deaths','cases','state','county','pop_death_rate',
            'population_density_per_sqmi'))

# NOW FORMING THE LOGIT FROM THE VARIABLES SELECTED IN THE EXHAUSTIVE SEARCH

training_df1 = exhaustive_data[training_rows,]
valid_df1 = exhaustive_data[validation_rows,]

exhaustive.logit = glm(death_rate ~., data = training_df1, 
                       family="quasibinomial")

summary(exhaustive.logit)
```

    ## 
    ## Call:
    ## glm(formula = death_rate ~ ., family = "quasibinomial", data = training_df1)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -0.19636  -0.05625  -0.01585   0.03005   0.43827  
    ## 
    ## Coefficients:
    ##                                                 Estimate Std. Error t value
    ## (Intercept)                                   -1.019e+01  5.652e+00  -1.803
    ## total_population                               4.318e-08  2.952e-08   1.463
    ## percent_fair_or_poor_health                    1.178e-02  7.767e-03   1.517
    ## percent_smokers                               -5.200e-02  8.268e-03  -6.290
    ## percent_adults_with_obesity                   -5.112e-03  3.843e-03  -1.330
    ## food_environment_index                         1.019e+00  5.166e-01   1.972
    ## percent_physically_inactive                    9.170e-03  3.729e-03   2.459
    ## percent_with_access_to_exercise_opportunities -2.634e-03  8.277e-04  -3.182
    ## percent_excessive_drinking                     9.875e-03  7.780e-03   1.269
    ## percent_uninsured                              1.287e-02  3.861e-03   3.333
    ## preventable_hospitalization_rate               3.965e-05  1.029e-05   3.855
    ## percent_vaccinated                             2.345e-03  2.210e-03   1.061
    ## percent_children_in_poverty                    2.120e-02  4.386e-03   4.834
    ## percent_single_parent_households_CHR           9.262e-03  2.572e-03   3.601
    ## injury_death_rate                             -5.089e-03  9.971e-04  -5.104
    ## inadequate_facilities                          2.571e-02  1.798e-02   1.430
    ## life_expectancy                               -7.712e-02  1.123e-02  -6.866
    ## percent_adults_with_diabetes                   8.121e-03  4.920e-03   1.651
    ## hiv_prevalence_rate                            4.108e-04  9.910e-05   4.145
    ## percent_food_insecure                          1.789e-01  9.710e-02   1.843
    ## percent_limited_access_to_healthy_foods        8.848e-02  4.589e-02   1.928
    ## median_household_income                        1.339e-05  2.223e-06   6.022
    ## percent_less_than_18_years_of_age             -9.470e-03  7.382e-03  -1.283
    ## percent_65_and_over                            4.038e-02  5.981e-03   6.752
    ## percent_disabled                              -1.269e-02  6.135e-03  -2.068
    ##                                               Pr(>|t|)    
    ## (Intercept)                                   0.071604 .  
    ## total_population                              0.143688    
    ## percent_fair_or_poor_health                   0.129435    
    ## percent_smokers                               4.08e-10 ***
    ## percent_adults_with_obesity                   0.183725    
    ## food_environment_index                        0.048749 *  
    ## percent_physically_inactive                   0.014048 *  
    ## percent_with_access_to_exercise_opportunities 0.001488 ** 
    ## percent_excessive_drinking                    0.204483    
    ## percent_uninsured                             0.000878 ***
    ## preventable_hospitalization_rate              0.000120 ***
    ## percent_vaccinated                            0.289014    
    ## percent_children_in_poverty                   1.47e-06 ***
    ## percent_single_parent_households_CHR          0.000327 ***
    ## injury_death_rate                             3.71e-07 ***
    ## inadequate_facilities                         0.152934    
    ## life_expectancy                               9.35e-12 ***
    ## percent_adults_with_diabetes                  0.099009 .  
    ## hiv_prevalence_rate                           3.57e-05 ***
    ## percent_food_insecure                         0.065556 .  
    ## percent_limited_access_to_healthy_foods       0.053990 .  
    ## median_household_income                       2.13e-09 ***
    ## percent_less_than_18_years_of_age             0.199748    
    ## percent_65_and_over                           2.02e-11 ***
    ## percent_disabled                              0.038816 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for quasibinomial family taken to be 0.006644686)
    ## 
    ##     Null deviance: 13.5826  on 1648  degrees of freedom
    ## Residual deviance:  9.6777  on 1624  degrees of freedom
    ##   (563 observations deleted due to missingness)
    ## AIC: NA
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
length(names(exhaustive.logit$coefficients))
```

    ## [1] 25

``` r
exhaustive.predictions = predict(exhaustive.logit, newdata=valid_df1, 
                                 type="response")

add_column(valid_df1, predictions, .after="death_rate")
```

    ## # A tibble: 738 x 26
    ##    death_rate predictions total_population percent_fair_or… percent_smokers
    ##         <dbl>       <dbl>            <dbl>            <dbl>           <dbl>
    ##  1    0.0281       0.0585            10552             31.0            22.9
    ##  2    0.00885      0.0200            50991             19.4            17.4
    ##  3    0.0138       0.0258            54377             20.9            18.7
    ##  4    0.0205       0.0429            12697             28.6            21.1
    ##  5    0.00770      0.0279            17187             21.4            18.4
    ##  6    0.00643      0.0225            52608             23.2            20.0
    ##  7    0.00995      0.0175            90257             20.1            18.2
    ##  8    0.0362       0.0336            10565             30.4            22.6
    ##  9    0.0240       0.0219            30239             22.3            19.1
    ## 10    0.0129       0.0240            21975             30.0            23.3
    ## # … with 728 more rows, and 21 more variables:
    ## #   percent_adults_with_obesity <dbl>, food_environment_index <dbl>,
    ## #   percent_physically_inactive <dbl>,
    ## #   percent_with_access_to_exercise_opportunities <dbl>,
    ## #   percent_excessive_drinking <dbl>, percent_uninsured <dbl>,
    ## #   preventable_hospitalization_rate <dbl>, percent_vaccinated <dbl>,
    ## #   percent_children_in_poverty <dbl>,
    ## #   percent_single_parent_households_CHR <dbl>, injury_death_rate <dbl>,
    ## #   inadequate_facilities <dbl>, life_expectancy <dbl>,
    ## #   percent_adults_with_diabetes <dbl>, hiv_prevalence_rate <dbl>,
    ## #   percent_food_insecure <dbl>, percent_limited_access_to_healthy_foods <dbl>,
    ## #   median_household_income <dbl>, percent_less_than_18_years_of_age <dbl>,
    ## #   percent_65_and_over <dbl>, percent_disabled <dbl>

``` r
accuracy(exhaustive.predictions, valid_df1$death_rate)
```

    ##                    ME       RMSE         MAE  MPE MAPE
    ## Test set -0.001147919 0.01181165 0.008377988 -Inf  Inf

PCA Analysis

``` r
# PCA Analysis

pca_data = modeling_data %>% 
  select(-c(death_rate))
  

covid_pcs <- prcomp(na.omit(pca_data, scale. = T))

summary(covid_pcs)
```

    ## Importance of components:
    ##                              PC1       PC2      PC3   PC4   PC5  PC6   PC7
    ## Standard deviation     4.191e+05 1.426e+04 1.61e+03 179.1 32.47 22.9 18.25
    ## Proportion of Variance 9.988e-01 1.160e-03 1.00e-05   0.0  0.00  0.0  0.00
    ## Cumulative Proportion  9.988e-01 1.000e+00 1.00e+00   1.0  1.00  1.0  1.00
    ##                          PC8   PC9  PC10  PC11  PC12  PC13 PC14 PC15  PC16
    ## Standard deviation     13.48 8.299 7.703 5.327 5.248 4.785 4.41 3.78 3.257
    ## Proportion of Variance  0.00 0.000 0.000 0.000 0.000 0.000 0.00 0.00 0.000
    ## Cumulative Proportion   1.00 1.000 1.000 1.000 1.000 1.000 1.00 1.00 1.000
    ##                         PC17  PC18  PC19  PC20  PC21  PC22  PC23  PC24 PC25
    ## Standard deviation     2.735 2.622 2.435 2.199 2.106 1.928 1.576 1.455 1.18
    ## Proportion of Variance 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.00
    ## Cumulative Proportion  1.000 1.000 1.000 1.000 1.000 1.000 1.000 1.000 1.00
    ##                         PC26   PC27    PC28
    ## Standard deviation     1.089 0.6811 0.02801
    ## Proportion of Variance 0.000 0.0000 0.00000
    ## Cumulative Proportion  1.000 1.0000 1.00000

``` r
covid_pcs$center
```

    ##                              total_population 
    ##                                  1.348504e+05 
    ##                   percent_fair_or_poor_health 
    ##                                  1.816473e+01 
    ##                               percent_smokers 
    ##                                  1.748629e+01 
    ##                   percent_adults_with_obesity 
    ##                                  3.293782e+01 
    ##                        food_environment_index 
    ##                                  7.515954e+00 
    ##                   percent_physically_inactive 
    ##                                  2.730919e+01 
    ## percent_with_access_to_exercise_opportunities 
    ##                                  6.498740e+01 
    ##                    percent_excessive_drinking 
    ##                                  1.749666e+01 
    ##                             percent_uninsured 
    ##                                  1.131019e+01 
    ##                  primary_care_physicians_rate 
    ##                                  5.508495e+01 
    ##              preventable_hospitalization_rate 
    ##                                  4.897773e+03 
    ##                            percent_vaccinated 
    ##                                  4.323096e+01 
    ##                   percent_children_in_poverty 
    ##                                  2.108035e+01 
    ##          percent_single_parent_households_CHR 
    ##                                  3.348816e+01 
    ##                             injury_death_rate 
    ##                                  8.367624e+01 
    ##                                  overcrowding 
    ##                                  2.439032e+00 
    ##                         inadequate_facilities 
    ##                                  1.079688e+00 
    ##                               life_expectancy 
    ##                                  7.742285e+01 
    ##                  percent_adults_with_diabetes 
    ##                                  1.234083e+01 
    ##                           hiv_prevalence_rate 
    ##                                  1.817373e+02 
    ##                         percent_food_insecure 
    ##                                  1.342642e+01 
    ##       percent_limited_access_to_healthy_foods 
    ##                                  7.570174e+00 
    ##                       median_household_income 
    ##                                  5.385556e+04 
    ##             percent_less_than_18_years_of_age 
    ##                                  2.208888e+01 
    ##                           percent_65_and_over 
    ##                                  1.860992e+01 
    ##                                 percent_rural 
    ##                                  5.137462e+01 
    ##                         percent_below_poverty 
    ##                                  1.665683e+01 
    ##                              percent_disabled 
    ##                                  1.565923e+01

``` r
covid_pcs$rotation[,1]
```

    ##                              total_population 
    ##                                  9.999553e-01 
    ##                   percent_fair_or_poor_health 
    ##                                 -1.152265e-06 
    ##                               percent_smokers 
    ##                                 -1.852058e-06 
    ##                   percent_adults_with_obesity 
    ##                                 -3.242202e-06 
    ##                        food_environment_index 
    ##                                  2.620771e-07 
    ##                   percent_physically_inactive 
    ##                                 -3.224824e-06 
    ## percent_with_access_to_exercise_opportunities 
    ##                                  1.558309e-05 
    ##                    percent_excessive_drinking 
    ##                                  1.009385e-06 
    ##                             percent_uninsured 
    ##                                 -8.297141e-07 
    ##                  primary_care_physicians_rate 
    ##                                  1.639923e-05 
    ##              preventable_hospitalization_rate 
    ##                                 -3.031448e-04 
    ##                            percent_vaccinated 
    ##                                  2.054258e-06 
    ##                   percent_children_in_poverty 
    ##                                 -2.594285e-06 
    ##          percent_single_parent_households_CHR 
    ##                                 -3.867519e-07 
    ##                             injury_death_rate 
    ##                                 -1.188872e-05 
    ##                                  overcrowding 
    ##                                  9.100875e-07 
    ##                         inadequate_facilities 
    ##                                 -7.623130e-08 
    ##                               life_expectancy 
    ##                                  1.517640e-06 
    ##                  percent_adults_with_diabetes 
    ##                                 -1.680989e-06 
    ##                           hiv_prevalence_rate 
    ##                                  1.335563e-04 
    ##                         percent_food_insecure 
    ##                                 -7.789817e-07 
    ##       percent_limited_access_to_healthy_foods 
    ##                                 -1.308949e-06 
    ##                       median_household_income 
    ##                                  9.446312e-03 
    ##             percent_less_than_18_years_of_age 
    ##                                  2.867252e-07 
    ##                           percent_65_and_over 
    ##                                 -2.096481e-06 
    ##                                 percent_rural 
    ##                                 -2.645030e-05 
    ##                         percent_below_poverty 
    ##                                 -1.224171e-06 
    ##                              percent_disabled 
    ##                                 -2.566921e-06

Forward / Backward / Stepwise Selection

    ## 
    ## Call:
    ## glm(formula = death_rate ~ ., family = "quasibinomial", data = train.data)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -0.19718  -0.05596  -0.01561   0.03133   0.43471  
    ## 
    ## Coefficients:
    ##                                                 Estimate Std. Error t value
    ## (Intercept)                                   -1.010e+01  5.681e+00  -1.777
    ## total_population                               4.574e-08  3.045e-08   1.502
    ## percent_fair_or_poor_health                    1.901e-02  8.917e-03   2.132
    ## percent_smokers                               -5.589e-02  8.920e-03  -6.266
    ## percent_adults_with_obesity                   -6.938e-03  3.944e-03  -1.759
    ## food_environment_index                         1.024e+00  5.193e-01   1.972
    ## percent_physically_inactive                    1.089e-02  3.834e-03   2.841
    ## percent_with_access_to_exercise_opportunities -2.326e-03  1.047e-03  -2.222
    ## percent_excessive_drinking                     1.105e-02  7.970e-03   1.386
    ## percent_uninsured                              1.144e-02  3.999e-03   2.861
    ## primary_care_physicians_rate                   1.063e-04  6.635e-04   0.160
    ## preventable_hospitalization_rate               4.130e-05  1.039e-05   3.974
    ## percent_vaccinated                             3.283e-03  2.269e-03   1.447
    ## percent_children_in_poverty                    2.038e-02  4.751e-03   4.290
    ## percent_single_parent_households_CHR           9.349e-03  2.703e-03   3.459
    ## injury_death_rate                             -5.211e-03  1.023e-03  -5.095
    ## overcrowding                                  -1.029e-03  1.269e-02  -0.081
    ## inadequate_facilities                          2.454e-02  2.025e-02   1.212
    ## life_expectancy                               -8.010e-02  1.187e-02  -6.748
    ## percent_adults_with_diabetes                   7.137e-03  5.081e-03   1.404
    ## hiv_prevalence_rate                            4.440e-04  1.008e-04   4.405
    ## percent_food_insecure                          1.791e-01  9.779e-02   1.832
    ## percent_limited_access_to_healthy_foods        8.900e-02  4.612e-02   1.930
    ## median_household_income                        1.317e-05  2.315e-06   5.690
    ## percent_less_than_18_years_of_age             -8.621e-03  7.914e-03  -1.089
    ## percent_65_and_over                            4.054e-02  6.388e-03   6.346
    ## percent_rural                                  7.380e-04  8.955e-04   0.824
    ## percent_below_poverty                         -3.041e-03  6.387e-03  -0.476
    ## percent_disabled                              -1.279e-02  6.360e-03  -2.011
    ##                                               Pr(>|t|)    
    ## (Intercept)                                   0.075720 .  
    ## total_population                              0.133256    
    ## percent_fair_or_poor_health                   0.033184 *  
    ## percent_smokers                               4.76e-10 ***
    ## percent_adults_with_obesity                   0.078698 .  
    ## food_environment_index                        0.048807 *  
    ## percent_physically_inactive                   0.004558 ** 
    ## percent_with_access_to_exercise_opportunities 0.026437 *  
    ## percent_excessive_drinking                    0.165910    
    ## percent_uninsured                             0.004275 ** 
    ## primary_care_physicians_rate                  0.872700    
    ## preventable_hospitalization_rate              7.37e-05 ***
    ## percent_vaccinated                            0.148097    
    ## percent_children_in_poverty                   1.90e-05 ***
    ## percent_single_parent_households_CHR          0.000557 ***
    ## injury_death_rate                             3.91e-07 ***
    ## overcrowding                                  0.935371    
    ## inadequate_facilities                         0.225756    
    ## life_expectancy                               2.10e-11 ***
    ## percent_adults_with_diabetes                  0.160382    
    ## hiv_prevalence_rate                           1.13e-05 ***
    ## percent_food_insecure                         0.067160 .  
    ## percent_limited_access_to_healthy_foods       0.053807 .  
    ## median_household_income                       1.51e-08 ***
    ## percent_less_than_18_years_of_age             0.276140    
    ## percent_65_and_over                           2.87e-10 ***
    ## percent_rural                                 0.409950    
    ## percent_below_poverty                         0.634016    
    ## percent_disabled                              0.044543 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for quasibinomial family taken to be 0.006508162)
    ## 
    ##     Null deviance: 13.0677  on 1604  degrees of freedom
    ## Residual deviance:  9.2638  on 1576  degrees of freedom
    ##   (607 observations deleted due to missingness)
    ## AIC: NA
    ## 
    ## Number of Fisher Scoring iterations: 7

    ## [1] 29

    ##                     ME       RMSE         MAE  MPE MAPE
    ## Test set -3.732668e-15 0.01132187 0.007929608 -Inf  Inf

    ##                    ME       RMSE        MAE  MPE MAPE
    ## Test set -0.001169602 0.01085734 0.00802686 -Inf  Inf

    ##                    ME      RMSE        MAE  MPE MAPE
    ## Test set -0.002046955 0.0125367 0.00975246 -Inf  Inf

    ## Start:  AIC=-13897.93
    ## death_rate ~ 1
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## + percent_children_in_poverty                    1 0.0279738 0.25020 -14066
    ## + percent_single_parent_households_CHR           1 0.0248271 0.25334 -14046
    ## + hiv_prevalence_rate                            1 0.0208632 0.25731 -14021
    ## + percent_fair_or_poor_health                    1 0.0160271 0.26215 -13991
    ## + percent_food_insecure                          1 0.0153312 0.26284 -13987
    ## + percent_uninsured                              1 0.0146295 0.26354 -13983
    ## + percent_adults_with_diabetes                   1 0.0140561 0.26412 -13979
    ## + percent_physically_inactive                    1 0.0137562 0.26442 -13977
    ## + percent_excessive_drinking                     1 0.0132641 0.26491 -13974
    ## + percent_below_poverty                          1 0.0122633 0.26591 -13968
    ## + life_expectancy                                1 0.0115271 0.26665 -13964
    ## + food_environment_index                         1 0.0104080 0.26776 -13957
    ## + preventable_hospitalization_rate               1 0.0101286 0.26804 -13956
    ## + percent_with_access_to_exercise_opportunities  1 0.0085294 0.26964 -13946
    ## + percent_disabled                               1 0.0078235 0.27035 -13942
    ## + median_household_income                        1 0.0074834 0.27069 -13940
    ## + percent_65_and_over                            1 0.0059373 0.27224 -13931
    ## + percent_vaccinated                             1 0.0046913 0.27348 -13923
    ## + primary_care_physicians_rate                   1 0.0026864 0.27549 -13912
    ## + percent_adults_with_obesity                    1 0.0026193 0.27555 -13911
    ## + percent_rural                                  1 0.0021170 0.27606 -13908
    ## + overcrowding                                   1 0.0015764 0.27660 -13905
    ## + percent_less_than_18_years_of_age              1 0.0015139 0.27666 -13905
    ## + percent_smokers                                1 0.0009332 0.27724 -13901
    ## + percent_limited_access_to_healthy_foods        1 0.0007261 0.27745 -13900
    ## + total_population                               1 0.0006473 0.27753 -13900
    ## <none>                                                       0.27817 -13898
    ## + injury_death_rate                              1 0.0002262 0.27795 -13897
    ## + inadequate_facilities                          1 0.0001692 0.27800 -13897
    ## 
    ## Step:  AIC=-14066.04
    ## death_rate ~ percent_children_in_poverty
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## + percent_smokers                                1 0.0088909 0.24131 -14122
    ## + hiv_prevalence_rate                            1 0.0083460 0.24185 -14118
    ## + median_household_income                        1 0.0071566 0.24304 -14111
    ## + percent_below_poverty                          1 0.0044272 0.24577 -14093
    ## + injury_death_rate                              1 0.0032827 0.24692 -14085
    ## + percent_65_and_over                            1 0.0026604 0.24754 -14081
    ## + percent_uninsured                              1 0.0024147 0.24778 -14080
    ## + percent_single_parent_households_CHR           1 0.0023808 0.24782 -14079
    ## + total_population                               1 0.0018596 0.24834 -14076
    ## + percent_less_than_18_years_of_age              1 0.0016923 0.24851 -14075
    ## + preventable_hospitalization_rate               1 0.0013630 0.24884 -14073
    ## + percent_adults_with_diabetes                   1 0.0011653 0.24903 -14072
    ## + percent_limited_access_to_healthy_foods        1 0.0010157 0.24918 -14071
    ## + percent_physically_inactive                    1 0.0008816 0.24932 -14070
    ## + food_environment_index                         1 0.0008473 0.24935 -14070
    ## + percent_adults_with_obesity                    1 0.0006889 0.24951 -14068
    ## + percent_disabled                               1 0.0005275 0.24967 -14067
    ## + inadequate_facilities                          1 0.0004852 0.24971 -14067
    ## + percent_fair_or_poor_health                    1 0.0004427 0.24976 -14067
    ## + percent_vaccinated                             1 0.0004390 0.24976 -14067
    ## <none>                                                       0.25020 -14066
    ## + percent_with_access_to_exercise_opportunities  1 0.0002732 0.24993 -14066
    ## + percent_excessive_drinking                     1 0.0002214 0.24998 -14066
    ## + percent_food_insecure                          1 0.0001110 0.25009 -14065
    ## + life_expectancy                                1 0.0000565 0.25014 -14064
    ## + overcrowding                                   1 0.0000364 0.25016 -14064
    ## + percent_rural                                  1 0.0000017 0.25020 -14064
    ## + primary_care_physicians_rate                   1 0.0000001 0.25020 -14064
    ## 
    ## Step:  AIC=-14122.11
    ## death_rate ~ percent_children_in_poverty + percent_smokers
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## + hiv_prevalence_rate                            1 0.0068360 0.23447 -14166
    ## + percent_physically_inactive                    1 0.0049268 0.23638 -14153
    ## + preventable_hospitalization_rate               1 0.0046171 0.23669 -14151
    ## + percent_adults_with_diabetes                   1 0.0041179 0.23719 -14148
    ## + percent_single_parent_households_CHR           1 0.0036931 0.23761 -14145
    ## + median_household_income                        1 0.0033193 0.23799 -14142
    ## + life_expectancy                                1 0.0024122 0.23890 -14136
    ## + percent_below_poverty                          1 0.0018751 0.23943 -14133
    ## + percent_less_than_18_years_of_age              1 0.0015601 0.23975 -14130
    ## + percent_with_access_to_exercise_opportunities  1 0.0015268 0.23978 -14130
    ## + percent_limited_access_to_healthy_foods        1 0.0015143 0.23979 -14130
    ## + percent_65_and_over                            1 0.0015101 0.23980 -14130
    ## + injury_death_rate                              1 0.0012277 0.24008 -14128
    ## + percent_vaccinated                             1 0.0011460 0.24016 -14128
    ## + percent_excessive_drinking                     1 0.0010507 0.24026 -14127
    ## + percent_uninsured                              1 0.0009572 0.24035 -14126
    ## + overcrowding                                   1 0.0009571 0.24035 -14126
    ## + inadequate_facilities                          1 0.0009001 0.24041 -14126
    ## + total_population                               1 0.0007031 0.24061 -14125
    ## + percent_fair_or_poor_health                    1 0.0005293 0.24078 -14124
    ## + food_environment_index                         1 0.0004969 0.24081 -14123
    ## <none>                                                       0.24131 -14122
    ## + primary_care_physicians_rate                   1 0.0002958 0.24101 -14122
    ## + percent_rural                                  1 0.0002064 0.24110 -14122
    ## + percent_adults_with_obesity                    1 0.0001197 0.24119 -14121
    ## + percent_food_insecure                          1 0.0000523 0.24125 -14120
    ## + percent_disabled                               1 0.0000050 0.24130 -14120
    ## 
    ## Step:  AIC=-14166.23
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## + percent_physically_inactive                    1 0.0053965 0.22908 -14202
    ## + percent_adults_with_diabetes                   1 0.0046107 0.22986 -14196
    ## + percent_65_and_over                            1 0.0045941 0.22988 -14196
    ## + preventable_hospitalization_rate               1 0.0040695 0.23040 -14192
    ## + life_expectancy                                1 0.0031204 0.23135 -14186
    ## + percent_with_access_to_exercise_opportunities  1 0.0029912 0.23148 -14185
    ## + percent_below_poverty                          1 0.0025600 0.23191 -14182
    ## + percent_rural                                  1 0.0022277 0.23224 -14180
    ## + percent_limited_access_to_healthy_foods        1 0.0014657 0.23301 -14174
    ## + percent_excessive_drinking                     1 0.0014654 0.23301 -14174
    ## + food_environment_index                         1 0.0013564 0.23311 -14174
    ## + median_household_income                        1 0.0012554 0.23322 -14173
    ## + overcrowding                                   1 0.0010718 0.23340 -14172
    ## + percent_less_than_18_years_of_age              1 0.0010593 0.23341 -14172
    ## + percent_uninsured                              1 0.0010240 0.23345 -14171
    ## + percent_single_parent_households_CHR           1 0.0009277 0.23354 -14171
    ## + primary_care_physicians_rate                   1 0.0008445 0.23363 -14170
    ## + percent_disabled                               1 0.0005804 0.23389 -14168
    ## + percent_vaccinated                             1 0.0004926 0.23398 -14168
    ## + inadequate_facilities                          1 0.0003019 0.23417 -14166
    ## <none>                                                       0.23447 -14166
    ## + percent_food_insecure                          1 0.0002456 0.23423 -14166
    ## + percent_adults_with_obesity                    1 0.0002448 0.23423 -14166
    ## + injury_death_rate                              1 0.0002133 0.23426 -14166
    ## + percent_fair_or_poor_health                    1 0.0001626 0.23431 -14165
    ## + total_population                               1 0.0000104 0.23446 -14164
    ## 
    ## Step:  AIC=-14201.61
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## + percent_65_and_over                            1 0.0038197 0.22526 -14227
    ## + median_household_income                        1 0.0023733 0.22670 -14216
    ## + preventable_hospitalization_rate               1 0.0022093 0.22687 -14215
    ## + percent_adults_with_diabetes                   1 0.0017821 0.22729 -14212
    ## + percent_below_poverty                          1 0.0014760 0.22760 -14210
    ## + percent_less_than_18_years_of_age              1 0.0014756 0.22760 -14210
    ## + percent_single_parent_households_CHR           1 0.0014326 0.22764 -14210
    ## + percent_with_access_to_exercise_opportunities  1 0.0013190 0.22776 -14209
    ## + food_environment_index                         1 0.0012438 0.22783 -14208
    ## + percent_limited_access_to_healthy_foods        1 0.0011834 0.22789 -14208
    ## + life_expectancy                                1 0.0011685 0.22791 -14208
    ## + percent_rural                                  1 0.0009538 0.22812 -14206
    ## + percent_vaccinated                             1 0.0007133 0.22836 -14205
    ## + overcrowding                                   1 0.0005835 0.22849 -14204
    ## + percent_excessive_drinking                     1 0.0004461 0.22863 -14203
    ## + percent_food_insecure                          1 0.0003016 0.22877 -14202
    ## <none>                                                       0.22908 -14202
    ## + injury_death_rate                              1 0.0002783 0.22880 -14202
    ## + percent_uninsured                              1 0.0002409 0.22883 -14201
    ## + percent_adults_with_obesity                    1 0.0001662 0.22891 -14201
    ## + inadequate_facilities                          1 0.0001070 0.22897 -14200
    ## + percent_disabled                               1 0.0000696 0.22901 -14200
    ## + total_population                               1 0.0000256 0.22905 -14200
    ## + primary_care_physicians_rate                   1 0.0000241 0.22905 -14200
    ## + percent_fair_or_poor_health                    1 0.0000079 0.22907 -14200
    ## 
    ## Step:  AIC=-14226.59
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive + percent_65_and_over
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## + median_household_income                        1 0.0045028 0.22075 -14257
    ## + preventable_hospitalization_rate               1 0.0031338 0.22212 -14247
    ## + injury_death_rate                              1 0.0019482 0.22331 -14238
    ## + life_expectancy                                1 0.0015329 0.22372 -14236
    ## + percent_single_parent_households_CHR           1 0.0013295 0.22393 -14234
    ## + percent_adults_with_diabetes                   1 0.0011182 0.22414 -14233
    ## + percent_with_access_to_exercise_opportunities  1 0.0010795 0.22418 -14232
    ## + percent_limited_access_to_healthy_foods        1 0.0009764 0.22428 -14232
    ## + percent_vaccinated                             1 0.0008498 0.22441 -14231
    ## + food_environment_index                         1 0.0008212 0.22443 -14230
    ## + percent_fair_or_poor_health                    1 0.0007839 0.22447 -14230
    ## + percent_uninsured                              1 0.0006465 0.22461 -14229
    ## + percent_disabled                               1 0.0006342 0.22462 -14229
    ## + percent_below_poverty                          1 0.0004115 0.22484 -14228
    ## <none>                                                       0.22526 -14227
    ## + percent_excessive_drinking                     1 0.0001760 0.22508 -14226
    ## + total_population                               1 0.0001325 0.22512 -14226
    ## + inadequate_facilities                          1 0.0001056 0.22515 -14225
    ## + percent_food_insecure                          1 0.0001038 0.22515 -14225
    ## + primary_care_physicians_rate                   1 0.0000709 0.22518 -14225
    ## + percent_rural                                  1 0.0000382 0.22522 -14225
    ## + percent_less_than_18_years_of_age              1 0.0000263 0.22523 -14225
    ## + percent_adults_with_obesity                    1 0.0000187 0.22524 -14225
    ## + overcrowding                                   1 0.0000031 0.22525 -14225
    ## 
    ## Step:  AIC=-14257
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive + percent_65_and_over + 
    ##     median_household_income
    ## 
    ##                                                 Df  Sum of Sq     RSS    AIC
    ## + preventable_hospitalization_rate               1 0.00287826 0.21788 -14276
    ## + life_expectancy                                1 0.00265568 0.21810 -14274
    ## + percent_with_access_to_exercise_opportunities  1 0.00149402 0.21926 -14266
    ## + injury_death_rate                              1 0.00147696 0.21928 -14266
    ## + percent_single_parent_households_CHR           1 0.00122683 0.21953 -14264
    ## + percent_adults_with_diabetes                   1 0.00108488 0.21967 -14263
    ## + percent_uninsured                              1 0.00099960 0.21975 -14262
    ## + percent_fair_or_poor_health                    1 0.00084967 0.21990 -14261
    ## + percent_limited_access_to_healthy_foods        1 0.00068016 0.22007 -14260
    ## + percent_vaccinated                             1 0.00054996 0.22020 -14259
    ## + food_environment_index                         1 0.00034092 0.22041 -14258
    ## <none>                                                        0.22075 -14257
    ## + percent_rural                                  1 0.00021496 0.22054 -14257
    ## + primary_care_physicians_rate                   1 0.00020366 0.22055 -14256
    ## + percent_disabled                               1 0.00012909 0.22062 -14256
    ## + inadequate_facilities                          1 0.00005824 0.22070 -14255
    ## + percent_excessive_drinking                     1 0.00004248 0.22071 -14255
    ## + total_population                               1 0.00002242 0.22073 -14255
    ## + percent_adults_with_obesity                    1 0.00002188 0.22073 -14255
    ## + percent_less_than_18_years_of_age              1 0.00000541 0.22075 -14255
    ## + overcrowding                                   1 0.00000086 0.22075 -14255
    ## + percent_below_poverty                          1 0.00000036 0.22075 -14255
    ## + percent_food_insecure                          1 0.00000032 0.22075 -14255
    ## 
    ## Step:  AIC=-14276.07
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive + percent_65_and_over + 
    ##     median_household_income + preventable_hospitalization_rate
    ## 
    ##                                                 Df  Sum of Sq     RSS    AIC
    ## + life_expectancy                                1 0.00208445 0.21579 -14290
    ## + percent_single_parent_households_CHR           1 0.00138365 0.21649 -14284
    ## + injury_death_rate                              1 0.00133215 0.21654 -14284
    ## + percent_with_access_to_exercise_opportunities  1 0.00128914 0.21658 -14284
    ## + percent_uninsured                              1 0.00101650 0.21686 -14282
    ## + percent_adults_with_diabetes                   1 0.00084536 0.21703 -14280
    ## + percent_fair_or_poor_health                    1 0.00076972 0.21710 -14280
    ## + percent_vaccinated                             1 0.00064219 0.21723 -14279
    ## + percent_limited_access_to_healthy_foods        1 0.00047116 0.21740 -14278
    ## <none>                                                        0.21788 -14276
    ## + percent_disabled                               1 0.00021227 0.21766 -14276
    ## + percent_rural                                  1 0.00018704 0.21769 -14275
    ## + primary_care_physicians_rate                   1 0.00017190 0.21770 -14275
    ## + food_environment_index                         1 0.00016289 0.21771 -14275
    ## + percent_excessive_drinking                     1 0.00006252 0.21781 -14274
    ## + percent_less_than_18_years_of_age              1 0.00003872 0.21784 -14274
    ## + percent_food_insecure                          1 0.00002354 0.21785 -14274
    ## + percent_below_poverty                          1 0.00001151 0.21786 -14274
    ## + total_population                               1 0.00001119 0.21786 -14274
    ## + overcrowding                                   1 0.00000485 0.21787 -14274
    ## + inadequate_facilities                          1 0.00000478 0.21787 -14274
    ## + percent_adults_with_obesity                    1 0.00000136 0.21787 -14274
    ## 
    ## Step:  AIC=-14289.5
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive + percent_65_and_over + 
    ##     median_household_income + preventable_hospitalization_rate + 
    ##     life_expectancy
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## + injury_death_rate                              1 0.0041551 0.21163 -14319
    ## + percent_single_parent_households_CHR           1 0.0011902 0.21460 -14296
    ## + percent_with_access_to_exercise_opportunities  1 0.0011348 0.21466 -14296
    ## + percent_uninsured                              1 0.0010573 0.21473 -14295
    ## + percent_disabled                               1 0.0009793 0.21481 -14295
    ## + percent_fair_or_poor_health                    1 0.0008279 0.21496 -14294
    ## + percent_vaccinated                             1 0.0005900 0.21520 -14292
    ## + percent_adults_with_diabetes                   1 0.0004661 0.21532 -14291
    ## + percent_limited_access_to_healthy_foods        1 0.0002809 0.21551 -14290
    ## <none>                                                       0.21579 -14290
    ## + percent_rural                                  1 0.0001885 0.21560 -14289
    ## + food_environment_index                         1 0.0001762 0.21561 -14289
    ## + percent_less_than_18_years_of_age              1 0.0001671 0.21562 -14289
    ## + primary_care_physicians_rate                   1 0.0001583 0.21563 -14289
    ## + percent_below_poverty                          1 0.0000572 0.21573 -14288
    ## + overcrowding                                   1 0.0000521 0.21574 -14288
    ## + percent_adults_with_obesity                    1 0.0000291 0.21576 -14288
    ## + total_population                               1 0.0000267 0.21576 -14288
    ## + inadequate_facilities                          1 0.0000039 0.21579 -14288
    ## + percent_excessive_drinking                     1 0.0000028 0.21579 -14288
    ## + percent_food_insecure                          1 0.0000021 0.21579 -14288
    ## 
    ## Step:  AIC=-14318.7
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive + percent_65_and_over + 
    ##     median_household_income + preventable_hospitalization_rate + 
    ##     life_expectancy + injury_death_rate
    ## 
    ##                                                 Df  Sum of Sq     RSS    AIC
    ## + percent_uninsured                              1 0.00156119 0.21007 -14329
    ## + percent_with_access_to_exercise_opportunities  1 0.00123117 0.21040 -14326
    ## + percent_single_parent_households_CHR           1 0.00091123 0.21072 -14324
    ## + percent_fair_or_poor_health                    1 0.00073497 0.21090 -14322
    ## + percent_disabled                               1 0.00071712 0.21092 -14322
    ## + percent_rural                                  1 0.00032700 0.21131 -14319
    ## + overcrowding                                   1 0.00028769 0.21135 -14319
    ## <none>                                                        0.21163 -14319
    ## + percent_adults_with_obesity                    1 0.00020945 0.21143 -14318
    ## + percent_adults_with_diabetes                   1 0.00020905 0.21143 -14318
    ## + percent_vaccinated                             1 0.00017979 0.21146 -14318
    ## + primary_care_physicians_rate                   1 0.00011575 0.21152 -14318
    ## + inadequate_facilities                          1 0.00010721 0.21153 -14318
    ## + food_environment_index                         1 0.00006868 0.21157 -14317
    ## + percent_limited_access_to_healthy_foods        1 0.00006558 0.21157 -14317
    ## + percent_below_poverty                          1 0.00005185 0.21158 -14317
    ## + total_population                               1 0.00003887 0.21160 -14317
    ## + percent_less_than_18_years_of_age              1 0.00003236 0.21160 -14317
    ## + percent_food_insecure                          1 0.00000838 0.21163 -14317
    ## + percent_excessive_drinking                     1 0.00000376 0.21163 -14317
    ## 
    ## Step:  AIC=-14328.59
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive + percent_65_and_over + 
    ##     median_household_income + preventable_hospitalization_rate + 
    ##     life_expectancy + injury_death_rate + percent_uninsured
    ## 
    ##                                                 Df  Sum of Sq     RSS    AIC
    ## + percent_single_parent_households_CHR           1 0.00155965 0.20851 -14338
    ## + percent_with_access_to_exercise_opportunities  1 0.00085145 0.20922 -14333
    ## + percent_disabled                               1 0.00076708 0.20931 -14332
    ## + percent_vaccinated                             1 0.00034471 0.20973 -14329
    ## <none>                                                        0.21007 -14329
    ## + percent_rural                                  1 0.00022642 0.20985 -14328
    ## + percent_adults_with_diabetes                   1 0.00022331 0.20985 -14328
    ## + percent_less_than_18_years_of_age              1 0.00021117 0.20986 -14328
    ## + percent_fair_or_poor_health                    1 0.00020261 0.20987 -14328
    ## + percent_limited_access_to_healthy_foods        1 0.00013981 0.20993 -14328
    ## + food_environment_index                         1 0.00013792 0.20994 -14328
    ## + percent_adults_with_obesity                    1 0.00007840 0.20999 -14327
    ## + inadequate_facilities                          1 0.00007788 0.21000 -14327
    ## + total_population                               1 0.00006310 0.21001 -14327
    ## + percent_excessive_drinking                     1 0.00005522 0.21002 -14327
    ## + percent_below_poverty                          1 0.00004248 0.21003 -14327
    ## + overcrowding                                   1 0.00003148 0.21004 -14327
    ## + primary_care_physicians_rate                   1 0.00002906 0.21005 -14327
    ## + percent_food_insecure                          1 0.00001960 0.21005 -14327
    ## 
    ## Step:  AIC=-14338.55
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive + percent_65_and_over + 
    ##     median_household_income + preventable_hospitalization_rate + 
    ##     life_expectancy + injury_death_rate + percent_uninsured + 
    ##     percent_single_parent_households_CHR
    ## 
    ##                                                 Df  Sum of Sq     RSS    AIC
    ## + percent_with_access_to_exercise_opportunities  1 0.00108662 0.20743 -14345
    ## + percent_rural                                  1 0.00062004 0.20789 -14341
    ## + percent_disabled                               1 0.00041969 0.20810 -14340
    ## + food_environment_index                         1 0.00039255 0.20812 -14340
    ## <none>                                                        0.20851 -14338
    ## + percent_less_than_18_years_of_age              1 0.00023555 0.20828 -14338
    ## + percent_limited_access_to_healthy_foods        1 0.00020859 0.20831 -14338
    ## + percent_food_insecure                          1 0.00020009 0.20831 -14338
    ## + percent_vaccinated                             1 0.00017629 0.20834 -14338
    ## + percent_adults_with_diabetes                   1 0.00017576 0.20834 -14338
    ## + percent_adults_with_obesity                    1 0.00017521 0.20834 -14338
    ## + inadequate_facilities                          1 0.00010611 0.20841 -14337
    ## + percent_fair_or_poor_health                    1 0.00008996 0.20842 -14337
    ## + primary_care_physicians_rate                   1 0.00008549 0.20843 -14337
    ## + total_population                               1 0.00007536 0.20844 -14337
    ## + percent_excessive_drinking                     1 0.00007068 0.20844 -14337
    ## + overcrowding                                   1 0.00002656 0.20849 -14337
    ## + percent_below_poverty                          1 0.00000071 0.20851 -14337
    ## 
    ## Step:  AIC=-14344.93
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive + percent_65_and_over + 
    ##     median_household_income + preventable_hospitalization_rate + 
    ##     life_expectancy + injury_death_rate + percent_uninsured + 
    ##     percent_single_parent_households_CHR + percent_with_access_to_exercise_opportunities
    ## 
    ##                                           Df  Sum of Sq     RSS    AIC
    ## + percent_disabled                         1 0.00053377 0.20689 -14347
    ## + food_environment_index                   1 0.00036371 0.20706 -14346
    ## + percent_adults_with_obesity              1 0.00035281 0.20708 -14346
    ## + percent_vaccinated                       1 0.00026374 0.20716 -14345
    ## <none>                                                  0.20743 -14345
    ## + percent_food_insecure                    1 0.00021998 0.20721 -14345
    ## + percent_less_than_18_years_of_age        1 0.00021874 0.20721 -14345
    ## + total_population                         1 0.00019657 0.20723 -14344
    ## + percent_limited_access_to_healthy_foods  1 0.00016345 0.20726 -14344
    ## + percent_excessive_drinking               1 0.00015077 0.20728 -14344
    ## + percent_adults_with_diabetes             1 0.00010545 0.20732 -14344
    ## + inadequate_facilities                    1 0.00009955 0.20733 -14344
    ## + percent_fair_or_poor_health              1 0.00006190 0.20737 -14343
    ## + percent_rural                            1 0.00004480 0.20738 -14343
    ## + overcrowding                             1 0.00003198 0.20740 -14343
    ## + primary_care_physicians_rate             1 0.00000316 0.20742 -14343
    ## + percent_below_poverty                    1 0.00000099 0.20743 -14343
    ## 
    ## Step:  AIC=-14347.07
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive + percent_65_and_over + 
    ##     median_household_income + preventable_hospitalization_rate + 
    ##     life_expectancy + injury_death_rate + percent_uninsured + 
    ##     percent_single_parent_households_CHR + percent_with_access_to_exercise_opportunities + 
    ##     percent_disabled
    ## 
    ##                                           Df  Sum of Sq     RSS    AIC
    ## + food_environment_index                   1 0.00038988 0.20650 -14348
    ## + percent_adults_with_obesity              1 0.00037405 0.20652 -14348
    ## + percent_less_than_18_years_of_age        1 0.00035353 0.20654 -14348
    ## <none>                                                  0.20689 -14347
    ## + percent_limited_access_to_healthy_foods  1 0.00020840 0.20669 -14347
    ## + total_population                         1 0.00020376 0.20669 -14347
    ## + percent_food_insecure                    1 0.00019931 0.20669 -14347
    ## + percent_vaccinated                       1 0.00019300 0.20670 -14347
    ## + percent_adults_with_diabetes             1 0.00015178 0.20674 -14346
    ## + percent_fair_or_poor_health              1 0.00013664 0.20676 -14346
    ## + inadequate_facilities                    1 0.00011425 0.20678 -14346
    ## + percent_excessive_drinking               1 0.00009731 0.20680 -14346
    ## + percent_rural                            1 0.00006138 0.20683 -14346
    ## + overcrowding                             1 0.00003027 0.20686 -14345
    ## + primary_care_physicians_rate             1 0.00000019 0.20689 -14345
    ## + percent_below_poverty                    1 0.00000003 0.20689 -14345
    ## 
    ## Step:  AIC=-14348.1
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive + percent_65_and_over + 
    ##     median_household_income + preventable_hospitalization_rate + 
    ##     life_expectancy + injury_death_rate + percent_uninsured + 
    ##     percent_single_parent_households_CHR + percent_with_access_to_exercise_opportunities + 
    ##     percent_disabled + food_environment_index
    ## 
    ##                                           Df  Sum of Sq     RSS    AIC
    ## + percent_less_than_18_years_of_age        1 0.00040693 0.20610 -14349
    ## + percent_adults_with_obesity              1 0.00038591 0.20612 -14349
    ## <none>                                                  0.20650 -14348
    ## + percent_fair_or_poor_health              1 0.00019962 0.20630 -14348
    ## + inadequate_facilities                    1 0.00018195 0.20632 -14348
    ## + percent_vaccinated                       1 0.00016453 0.20634 -14347
    ## + total_population                         1 0.00016148 0.20634 -14347
    ## + percent_adults_with_diabetes             1 0.00013023 0.20637 -14347
    ## + percent_excessive_drinking               1 0.00006186 0.20644 -14347
    ## + percent_below_poverty                    1 0.00004100 0.20646 -14346
    ## + percent_rural                            1 0.00001719 0.20649 -14346
    ## + overcrowding                             1 0.00001243 0.20649 -14346
    ## + percent_limited_access_to_healthy_foods  1 0.00000269 0.20650 -14346
    ## + primary_care_physicians_rate             1 0.00000253 0.20650 -14346
    ## + percent_food_insecure                    1 0.00000016 0.20650 -14346
    ## 
    ## Step:  AIC=-14349.26
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive + percent_65_and_over + 
    ##     median_household_income + preventable_hospitalization_rate + 
    ##     life_expectancy + injury_death_rate + percent_uninsured + 
    ##     percent_single_parent_households_CHR + percent_with_access_to_exercise_opportunities + 
    ##     percent_disabled + food_environment_index + percent_less_than_18_years_of_age
    ## 
    ##                                           Df  Sum of Sq     RSS    AIC
    ## + percent_adults_with_obesity              1 2.7694e-04 0.20582 -14349
    ## + percent_fair_or_poor_health              1 2.7409e-04 0.20582 -14349
    ## <none>                                                  0.20610 -14349
    ## + inadequate_facilities                    1 2.1086e-04 0.20589 -14349
    ## + total_population                         1 1.7268e-04 0.20592 -14349
    ## + percent_adults_with_diabetes             1 1.4425e-04 0.20595 -14348
    ## + overcrowding                             1 1.0927e-04 0.20599 -14348
    ## + percent_vaccinated                       1 1.0187e-04 0.20600 -14348
    ## + percent_limited_access_to_healthy_foods  1 3.5293e-05 0.20606 -14348
    ## + percent_excessive_drinking               1 3.3292e-05 0.20606 -14348
    ## + percent_food_insecure                    1 1.5031e-05 0.20608 -14347
    ## + percent_below_poverty                    1 1.1992e-05 0.20608 -14347
    ## + percent_rural                            1 6.2320e-06 0.20609 -14347
    ## + primary_care_physicians_rate             1 4.9000e-08 0.20610 -14347
    ## 
    ## Step:  AIC=-14349.42
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive + percent_65_and_over + 
    ##     median_household_income + preventable_hospitalization_rate + 
    ##     life_expectancy + injury_death_rate + percent_uninsured + 
    ##     percent_single_parent_households_CHR + percent_with_access_to_exercise_opportunities + 
    ##     percent_disabled + food_environment_index + percent_less_than_18_years_of_age + 
    ##     percent_adults_with_obesity
    ## 
    ##                                           Df  Sum of Sq     RSS    AIC
    ## + percent_adults_with_diabetes             1 2.6825e-04 0.20555 -14350
    ## <none>                                                  0.20582 -14349
    ## + percent_fair_or_poor_health              1 2.4608e-04 0.20557 -14349
    ## + inadequate_facilities                    1 2.2310e-04 0.20560 -14349
    ## + total_population                         1 1.3307e-04 0.20569 -14348
    ## + percent_vaccinated                       1 1.1799e-04 0.20570 -14348
    ## + overcrowding                             1 8.7146e-05 0.20573 -14348
    ## + percent_limited_access_to_healthy_foods  1 3.7430e-05 0.20578 -14348
    ## + percent_excessive_drinking               1 3.7190e-05 0.20578 -14348
    ## + percent_rural                            1 1.7625e-05 0.20580 -14348
    ## + percent_food_insecure                    1 1.6383e-05 0.20580 -14348
    ## + primary_care_physicians_rate             1 5.0630e-06 0.20581 -14348
    ## + percent_below_poverty                    1 1.5160e-06 0.20582 -14347
    ## 
    ## Step:  AIC=-14349.51
    ## death_rate ~ percent_children_in_poverty + percent_smokers + 
    ##     hiv_prevalence_rate + percent_physically_inactive + percent_65_and_over + 
    ##     median_household_income + preventable_hospitalization_rate + 
    ##     life_expectancy + injury_death_rate + percent_uninsured + 
    ##     percent_single_parent_households_CHR + percent_with_access_to_exercise_opportunities + 
    ##     percent_disabled + food_environment_index + percent_less_than_18_years_of_age + 
    ##     percent_adults_with_obesity + percent_adults_with_diabetes
    ## 
    ##                                           Df  Sum of Sq     RSS    AIC
    ## <none>                                                  0.20555 -14350
    ## + inadequate_facilities                    1 2.1422e-04 0.20534 -14349
    ## + percent_fair_or_poor_health              1 2.1074e-04 0.20534 -14349
    ## + total_population                         1 1.2050e-04 0.20543 -14348
    ## + overcrowding                             1 9.3040e-05 0.20546 -14348
    ## + percent_vaccinated                       1 8.6970e-05 0.20547 -14348
    ## + percent_excessive_drinking               1 7.3677e-05 0.20548 -14348
    ## + percent_limited_access_to_healthy_foods  1 5.2066e-05 0.20550 -14348
    ## + percent_food_insecure                    1 2.5875e-05 0.20553 -14348
    ## + percent_rural                            1 1.6168e-05 0.20554 -14348
    ## + primary_care_physicians_rate             1 4.0220e-06 0.20555 -14348
    ## + percent_below_poverty                    1 7.4600e-07 0.20555 -14348

    ##                    ME       RMSE         MAE  MPE MAPE
    ## Test set -0.001005406 0.01142413 0.008321012 -Inf  Inf

    ## [1] 18

    ##                    ME       RMSE         MAE  MPE MAPE
    ## Test set -0.001179069 0.01178969 0.008409832 -Inf  Inf

    ## Start:  AIC=-14338.65
    ## death_rate ~ total_population + percent_fair_or_poor_health + 
    ##     percent_smokers + percent_adults_with_obesity + food_environment_index + 
    ##     percent_physically_inactive + percent_with_access_to_exercise_opportunities + 
    ##     percent_excessive_drinking + percent_uninsured + primary_care_physicians_rate + 
    ##     preventable_hospitalization_rate + percent_vaccinated + percent_children_in_poverty + 
    ##     percent_single_parent_households_CHR + injury_death_rate + 
    ##     overcrowding + inadequate_facilities + life_expectancy + 
    ##     percent_adults_with_diabetes + hiv_prevalence_rate + percent_food_insecure + 
    ##     percent_limited_access_to_healthy_foods + median_household_income + 
    ##     percent_less_than_18_years_of_age + percent_65_and_over + 
    ##     percent_rural + percent_below_poverty + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - primary_care_physicians_rate                   1 0.0000005 0.20413 -14341
    ## - overcrowding                                   1 0.0000026 0.20413 -14341
    ## - percent_below_poverty                          1 0.0000123 0.20414 -14341
    ## - percent_rural                                  1 0.0001224 0.20425 -14340
    ## - inadequate_facilities                          1 0.0001363 0.20427 -14340
    ## - total_population                               1 0.0001519 0.20428 -14340
    ## - percent_vaccinated                             1 0.0001691 0.20430 -14339
    ## - percent_excessive_drinking                     1 0.0001997 0.20433 -14339
    ## - percent_less_than_18_years_of_age              1 0.0002315 0.20436 -14339
    ## - percent_adults_with_diabetes                   1 0.0002355 0.20436 -14339
    ## <none>                                                       0.20413 -14339
    ## - percent_adults_with_obesity                    1 0.0003894 0.20452 -14338
    ## - percent_food_insecure                          1 0.0004078 0.20454 -14338
    ## - percent_limited_access_to_healthy_foods        1 0.0004243 0.20455 -14337
    ## - food_environment_index                         1 0.0004510 0.20458 -14337
    ## - percent_fair_or_poor_health                    1 0.0004598 0.20459 -14337
    ## - percent_with_access_to_exercise_opportunities  1 0.0005986 0.20473 -14336
    ## - percent_physically_inactive                    1 0.0006102 0.20474 -14336
    ## - percent_disabled                               1 0.0008091 0.20494 -14334
    ## - percent_uninsured                              1 0.0012538 0.20538 -14331
    ## - percent_single_parent_households_CHR           1 0.0015315 0.20566 -14329
    ## - preventable_hospitalization_rate               1 0.0016295 0.20576 -14328
    ## - hiv_prevalence_rate                            1 0.0021316 0.20626 -14324
    ## - percent_children_in_poverty                    1 0.0024325 0.20656 -14322
    ## - injury_death_rate                              1 0.0033770 0.20751 -14314
    ## - median_household_income                        1 0.0038359 0.20797 -14311
    ## - percent_65_and_over                            1 0.0049330 0.20906 -14302
    ## - life_expectancy                                1 0.0052401 0.20937 -14300
    ## - percent_smokers                                1 0.0053178 0.20945 -14299
    ## 
    ## Step:  AIC=-14340.65
    ## death_rate ~ total_population + percent_fair_or_poor_health + 
    ##     percent_smokers + percent_adults_with_obesity + food_environment_index + 
    ##     percent_physically_inactive + percent_with_access_to_exercise_opportunities + 
    ##     percent_excessive_drinking + percent_uninsured + preventable_hospitalization_rate + 
    ##     percent_vaccinated + percent_children_in_poverty + percent_single_parent_households_CHR + 
    ##     injury_death_rate + overcrowding + inadequate_facilities + 
    ##     life_expectancy + percent_adults_with_diabetes + hiv_prevalence_rate + 
    ##     percent_food_insecure + percent_limited_access_to_healthy_foods + 
    ##     median_household_income + percent_less_than_18_years_of_age + 
    ##     percent_65_and_over + percent_rural + percent_below_poverty + 
    ##     percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - overcrowding                                   1 0.0000028 0.20413 -14343
    ## - percent_below_poverty                          1 0.0000121 0.20414 -14343
    ## - percent_rural                                  1 0.0001237 0.20425 -14342
    ## - inadequate_facilities                          1 0.0001380 0.20427 -14342
    ## - total_population                               1 0.0001524 0.20428 -14342
    ## - percent_vaccinated                             1 0.0001705 0.20430 -14341
    ## - percent_excessive_drinking                     1 0.0001992 0.20433 -14341
    ## - percent_less_than_18_years_of_age              1 0.0002314 0.20436 -14341
    ## - percent_adults_with_diabetes                   1 0.0002351 0.20436 -14341
    ## <none>                                                       0.20413 -14341
    ## - percent_adults_with_obesity                    1 0.0003992 0.20453 -14340
    ## - percent_food_insecure                          1 0.0004076 0.20454 -14339
    ## - percent_limited_access_to_healthy_foods        1 0.0004240 0.20455 -14339
    ## - food_environment_index                         1 0.0004507 0.20458 -14339
    ## - percent_fair_or_poor_health                    1 0.0004600 0.20459 -14339
    ## - percent_with_access_to_exercise_opportunities  1 0.0006070 0.20474 -14338
    ## - percent_physically_inactive                    1 0.0006172 0.20475 -14338
    ## - percent_disabled                               1 0.0008186 0.20495 -14336
    ## - percent_uninsured                              1 0.0012547 0.20539 -14333
    ## - percent_single_parent_households_CHR           1 0.0015317 0.20566 -14331
    ## - preventable_hospitalization_rate               1 0.0016321 0.20576 -14330
    ## - hiv_prevalence_rate                            1 0.0021315 0.20626 -14326
    ## - percent_children_in_poverty                    1 0.0024378 0.20657 -14324
    ## - injury_death_rate                              1 0.0033835 0.20751 -14316
    ## - median_household_income                        1 0.0038397 0.20797 -14313
    ## - percent_65_and_over                            1 0.0049693 0.20910 -14304
    ## - life_expectancy                                1 0.0052402 0.20937 -14302
    ## - percent_smokers                                1 0.0053175 0.20945 -14301
    ## 
    ## Step:  AIC=-14342.63
    ## death_rate ~ total_population + percent_fair_or_poor_health + 
    ##     percent_smokers + percent_adults_with_obesity + food_environment_index + 
    ##     percent_physically_inactive + percent_with_access_to_exercise_opportunities + 
    ##     percent_excessive_drinking + percent_uninsured + preventable_hospitalization_rate + 
    ##     percent_vaccinated + percent_children_in_poverty + percent_single_parent_households_CHR + 
    ##     injury_death_rate + inadequate_facilities + life_expectancy + 
    ##     percent_adults_with_diabetes + hiv_prevalence_rate + percent_food_insecure + 
    ##     percent_limited_access_to_healthy_foods + median_household_income + 
    ##     percent_less_than_18_years_of_age + percent_65_and_over + 
    ##     percent_rural + percent_below_poverty + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - percent_below_poverty                          1 0.0000152 0.20415 -14344
    ## - percent_rural                                  1 0.0001232 0.20426 -14344
    ## - inadequate_facilities                          1 0.0001364 0.20427 -14344
    ## - total_population                               1 0.0001501 0.20428 -14343
    ## - percent_vaccinated                             1 0.0001717 0.20430 -14343
    ## - percent_excessive_drinking                     1 0.0001967 0.20433 -14343
    ## - percent_adults_with_diabetes                   1 0.0002368 0.20437 -14343
    ## <none>                                                       0.20413 -14343
    ## - percent_less_than_18_years_of_age              1 0.0002745 0.20441 -14342
    ## - percent_adults_with_obesity                    1 0.0003984 0.20453 -14342
    ## - percent_food_insecure                          1 0.0004108 0.20454 -14341
    ## - percent_limited_access_to_healthy_foods        1 0.0004259 0.20456 -14341
    ## - food_environment_index                         1 0.0004524 0.20459 -14341
    ## - percent_fair_or_poor_health                    1 0.0004829 0.20462 -14341
    ## - percent_with_access_to_exercise_opportunities  1 0.0006062 0.20474 -14340
    ## - percent_physically_inactive                    1 0.0006263 0.20476 -14340
    ## - percent_disabled                               1 0.0008179 0.20495 -14338
    ## - percent_uninsured                              1 0.0012720 0.20541 -14335
    ## - percent_single_parent_households_CHR           1 0.0015311 0.20566 -14333
    ## - preventable_hospitalization_rate               1 0.0016537 0.20579 -14332
    ## - hiv_prevalence_rate                            1 0.0021296 0.20626 -14328
    ## - percent_children_in_poverty                    1 0.0024353 0.20657 -14326
    ## - injury_death_rate                              1 0.0034248 0.20756 -14318
    ## - median_household_income                        1 0.0038407 0.20797 -14315
    ## - percent_65_and_over                            1 0.0049699 0.20910 -14306
    ## - life_expectancy                                1 0.0053107 0.20944 -14303
    ## - percent_smokers                                1 0.0055207 0.20965 -14302
    ## 
    ## Step:  AIC=-14344.51
    ## death_rate ~ total_population + percent_fair_or_poor_health + 
    ##     percent_smokers + percent_adults_with_obesity + food_environment_index + 
    ##     percent_physically_inactive + percent_with_access_to_exercise_opportunities + 
    ##     percent_excessive_drinking + percent_uninsured + preventable_hospitalization_rate + 
    ##     percent_vaccinated + percent_children_in_poverty + percent_single_parent_households_CHR + 
    ##     injury_death_rate + inadequate_facilities + life_expectancy + 
    ##     percent_adults_with_diabetes + hiv_prevalence_rate + percent_food_insecure + 
    ##     percent_limited_access_to_healthy_foods + median_household_income + 
    ##     percent_less_than_18_years_of_age + percent_65_and_over + 
    ##     percent_rural + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - percent_rural                                  1 0.0001177 0.20427 -14346
    ## - inadequate_facilities                          1 0.0001315 0.20428 -14346
    ## - total_population                               1 0.0001444 0.20429 -14345
    ## - percent_vaccinated                             1 0.0001666 0.20431 -14345
    ## - percent_excessive_drinking                     1 0.0001992 0.20435 -14345
    ## - percent_adults_with_diabetes                   1 0.0002402 0.20439 -14345
    ## <none>                                                       0.20415 -14344
    ## - percent_less_than_18_years_of_age              1 0.0002648 0.20441 -14344
    ## - percent_adults_with_obesity                    1 0.0003854 0.20453 -14344
    ## - percent_food_insecure                          1 0.0004048 0.20455 -14343
    ## - percent_limited_access_to_healthy_foods        1 0.0004241 0.20457 -14343
    ## - food_environment_index                         1 0.0004505 0.20460 -14343
    ## - percent_fair_or_poor_health                    1 0.0004693 0.20462 -14343
    ## - percent_with_access_to_exercise_opportunities  1 0.0006125 0.20476 -14342
    ## - percent_physically_inactive                    1 0.0006575 0.20481 -14341
    ## - percent_disabled                               1 0.0008140 0.20496 -14340
    ## - percent_uninsured                              1 0.0012943 0.20544 -14336
    ## - percent_single_parent_households_CHR           1 0.0015160 0.20566 -14335
    ## - preventable_hospitalization_rate               1 0.0016576 0.20581 -14334
    ## - hiv_prevalence_rate                            1 0.0022311 0.20638 -14329
    ## - percent_children_in_poverty                    1 0.0026158 0.20676 -14326
    ## - injury_death_rate                              1 0.0034284 0.20758 -14320
    ## - median_household_income                        1 0.0043444 0.20849 -14313
    ## - life_expectancy                                1 0.0054278 0.20958 -14304
    ## - percent_65_and_over                            1 0.0055241 0.20967 -14304
    ## - percent_smokers                                1 0.0055758 0.20972 -14303
    ## 
    ## Step:  AIC=-14345.58
    ## death_rate ~ total_population + percent_fair_or_poor_health + 
    ##     percent_smokers + percent_adults_with_obesity + food_environment_index + 
    ##     percent_physically_inactive + percent_with_access_to_exercise_opportunities + 
    ##     percent_excessive_drinking + percent_uninsured + preventable_hospitalization_rate + 
    ##     percent_vaccinated + percent_children_in_poverty + percent_single_parent_households_CHR + 
    ##     injury_death_rate + inadequate_facilities + life_expectancy + 
    ##     percent_adults_with_diabetes + hiv_prevalence_rate + percent_food_insecure + 
    ##     percent_limited_access_to_healthy_foods + median_household_income + 
    ##     percent_less_than_18_years_of_age + percent_65_and_over + 
    ##     percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - total_population                               1 0.0001126 0.20438 -14347
    ## - percent_vaccinated                             1 0.0001278 0.20439 -14347
    ## - inadequate_facilities                          1 0.0001474 0.20441 -14346
    ## - percent_excessive_drinking                     1 0.0001854 0.20445 -14346
    ## - percent_adults_with_diabetes                   1 0.0002512 0.20452 -14346
    ## <none>                                                       0.20427 -14346
    ## - percent_less_than_18_years_of_age              1 0.0002934 0.20456 -14345
    ## - percent_adults_with_obesity                    1 0.0003550 0.20462 -14345
    ## - percent_fair_or_poor_health                    1 0.0003953 0.20466 -14344
    ## - percent_food_insecure                          1 0.0004072 0.20467 -14344
    ## - percent_limited_access_to_healthy_foods        1 0.0004210 0.20469 -14344
    ## - food_environment_index                         1 0.0004542 0.20472 -14344
    ## - percent_physically_inactive                    1 0.0006919 0.20496 -14342
    ## - percent_disabled                               1 0.0007937 0.20506 -14341
    ## - percent_uninsured                              1 0.0013288 0.20559 -14337
    ## - percent_single_parent_households_CHR           1 0.0014189 0.20569 -14336
    ## - percent_with_access_to_exercise_opportunities  1 0.0014294 0.20569 -14336
    ## - preventable_hospitalization_rate               1 0.0016411 0.20591 -14335
    ## - hiv_prevalence_rate                            1 0.0021683 0.20643 -14331
    ## - percent_children_in_poverty                    1 0.0027630 0.20703 -14326
    ## - injury_death_rate                              1 0.0033643 0.20763 -14321
    ## - median_household_income                        1 0.0042554 0.20852 -14314
    ## - life_expectancy                                1 0.0053255 0.20959 -14306
    ## - percent_smokers                                1 0.0054581 0.20972 -14305
    ## - percent_65_and_over                            1 0.0059536 0.21022 -14302
    ## 
    ## Step:  AIC=-14346.7
    ## death_rate ~ percent_fair_or_poor_health + percent_smokers + 
    ##     percent_adults_with_obesity + food_environment_index + percent_physically_inactive + 
    ##     percent_with_access_to_exercise_opportunities + percent_excessive_drinking + 
    ##     percent_uninsured + preventable_hospitalization_rate + percent_vaccinated + 
    ##     percent_children_in_poverty + percent_single_parent_households_CHR + 
    ##     injury_death_rate + inadequate_facilities + life_expectancy + 
    ##     percent_adults_with_diabetes + hiv_prevalence_rate + percent_food_insecure + 
    ##     percent_limited_access_to_healthy_foods + median_household_income + 
    ##     percent_less_than_18_years_of_age + percent_65_and_over + 
    ##     percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - percent_vaccinated                             1 0.0001249 0.20450 -14348
    ## - inadequate_facilities                          1 0.0001581 0.20454 -14348
    ## - percent_excessive_drinking                     1 0.0001771 0.20456 -14347
    ## <none>                                                       0.20438 -14347
    ## - percent_adults_with_diabetes                   1 0.0002591 0.20464 -14347
    ## - percent_less_than_18_years_of_age              1 0.0002817 0.20466 -14346
    ## - percent_adults_with_obesity                    1 0.0003999 0.20478 -14346
    ## - percent_food_insecure                          1 0.0004052 0.20478 -14346
    ## - percent_fair_or_poor_health                    1 0.0004146 0.20479 -14345
    ## - percent_limited_access_to_healthy_foods        1 0.0004168 0.20480 -14345
    ## - food_environment_index                         1 0.0004527 0.20483 -14345
    ## - percent_physically_inactive                    1 0.0006813 0.20506 -14343
    ## - percent_disabled                               1 0.0007984 0.20518 -14342
    ## - percent_uninsured                              1 0.0012899 0.20567 -14339
    ## - percent_with_access_to_exercise_opportunities  1 0.0013414 0.20572 -14338
    ## - percent_single_parent_households_CHR           1 0.0014064 0.20578 -14338
    ## - preventable_hospitalization_rate               1 0.0016732 0.20605 -14336
    ## - hiv_prevalence_rate                            1 0.0026070 0.20699 -14328
    ## - percent_children_in_poverty                    1 0.0028044 0.20718 -14327
    ## - injury_death_rate                              1 0.0033492 0.20773 -14323
    ## - median_household_income                        1 0.0043200 0.20870 -14315
    ## - life_expectancy                                1 0.0052762 0.20966 -14308
    ## - percent_smokers                                1 0.0055944 0.20997 -14305
    ## - percent_65_and_over                            1 0.0058955 0.21027 -14303
    ## 
    ## Step:  AIC=-14347.72
    ## death_rate ~ percent_fair_or_poor_health + percent_smokers + 
    ##     percent_adults_with_obesity + food_environment_index + percent_physically_inactive + 
    ##     percent_with_access_to_exercise_opportunities + percent_excessive_drinking + 
    ##     percent_uninsured + preventable_hospitalization_rate + percent_children_in_poverty + 
    ##     percent_single_parent_households_CHR + injury_death_rate + 
    ##     inadequate_facilities + life_expectancy + percent_adults_with_diabetes + 
    ##     hiv_prevalence_rate + percent_food_insecure + percent_limited_access_to_healthy_foods + 
    ##     median_household_income + percent_less_than_18_years_of_age + 
    ##     percent_65_and_over + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - percent_excessive_drinking                     1 0.0001488 0.20465 -14349
    ## - inadequate_facilities                          1 0.0001704 0.20467 -14348
    ## <none>                                                       0.20450 -14348
    ## - percent_adults_with_diabetes                   1 0.0002941 0.20480 -14347
    ## - percent_less_than_18_years_of_age              1 0.0003463 0.20485 -14347
    ## - percent_fair_or_poor_health                    1 0.0003719 0.20488 -14347
    ## - percent_adults_with_obesity                    1 0.0003917 0.20489 -14347
    ## - percent_food_insecure                          1 0.0004085 0.20491 -14346
    ## - percent_limited_access_to_healthy_foods        1 0.0004209 0.20492 -14346
    ## - food_environment_index                         1 0.0004580 0.20496 -14346
    ## - percent_physically_inactive                    1 0.0006677 0.20517 -14344
    ## - percent_disabled                               1 0.0008987 0.20540 -14343
    ## - percent_uninsured                              1 0.0012718 0.20578 -14340
    ## - percent_with_access_to_exercise_opportunities  1 0.0012730 0.20578 -14340
    ## - percent_single_parent_households_CHR           1 0.0015338 0.20604 -14338
    ## - preventable_hospitalization_rate               1 0.0016474 0.20615 -14337
    ## - hiv_prevalence_rate                            1 0.0025802 0.20708 -14330
    ## - percent_children_in_poverty                    1 0.0026805 0.20718 -14329
    ## - injury_death_rate                              1 0.0035901 0.20809 -14322
    ## - median_household_income                        1 0.0043978 0.20890 -14316
    ## - percent_smokers                                1 0.0054712 0.20997 -14307
    ## - life_expectancy                                1 0.0054893 0.20999 -14307
    ## - percent_65_and_over                            1 0.0058090 0.21031 -14305
    ## 
    ## Step:  AIC=-14348.55
    ## death_rate ~ percent_fair_or_poor_health + percent_smokers + 
    ##     percent_adults_with_obesity + food_environment_index + percent_physically_inactive + 
    ##     percent_with_access_to_exercise_opportunities + percent_uninsured + 
    ##     preventable_hospitalization_rate + percent_children_in_poverty + 
    ##     percent_single_parent_households_CHR + injury_death_rate + 
    ##     inadequate_facilities + life_expectancy + percent_adults_with_diabetes + 
    ##     hiv_prevalence_rate + percent_food_insecure + percent_limited_access_to_healthy_foods + 
    ##     median_household_income + percent_less_than_18_years_of_age + 
    ##     percent_65_and_over + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - inadequate_facilities                          1 0.0002135 0.20487 -14349
    ## - percent_adults_with_diabetes                   1 0.0002484 0.20490 -14349
    ## <none>                                                       0.20465 -14349
    ## - percent_fair_or_poor_health                    1 0.0002585 0.20491 -14348
    ## - percent_adults_with_obesity                    1 0.0003791 0.20503 -14348
    ## - percent_food_insecure                          1 0.0003981 0.20505 -14347
    ## - percent_less_than_18_years_of_age              1 0.0004017 0.20505 -14347
    ## - percent_limited_access_to_healthy_foods        1 0.0004179 0.20507 -14347
    ## - food_environment_index                         1 0.0004534 0.20511 -14347
    ## - percent_physically_inactive                    1 0.0006345 0.20529 -14346
    ## - percent_disabled                               1 0.0009448 0.20560 -14343
    ## - percent_with_access_to_exercise_opportunities  1 0.0012041 0.20586 -14341
    ## - percent_uninsured                              1 0.0012231 0.20588 -14341
    ## - percent_single_parent_households_CHR           1 0.0015824 0.20624 -14338
    ## - preventable_hospitalization_rate               1 0.0017356 0.20639 -14337
    ## - hiv_prevalence_rate                            1 0.0025960 0.20725 -14330
    ## - percent_children_in_poverty                    1 0.0026961 0.20735 -14330
    ## - injury_death_rate                              1 0.0037408 0.20839 -14322
    ## - median_household_income                        1 0.0042492 0.20890 -14318
    ## - life_expectancy                                1 0.0053672 0.21002 -14309
    ## - percent_smokers                                1 0.0053869 0.21004 -14309
    ## - percent_65_and_over                            1 0.0056972 0.21035 -14306
    ## 
    ## Step:  AIC=-14348.88
    ## death_rate ~ percent_fair_or_poor_health + percent_smokers + 
    ##     percent_adults_with_obesity + food_environment_index + percent_physically_inactive + 
    ##     percent_with_access_to_exercise_opportunities + percent_uninsured + 
    ##     preventable_hospitalization_rate + percent_children_in_poverty + 
    ##     percent_single_parent_households_CHR + injury_death_rate + 
    ##     life_expectancy + percent_adults_with_diabetes + hiv_prevalence_rate + 
    ##     percent_food_insecure + percent_limited_access_to_healthy_foods + 
    ##     median_household_income + percent_less_than_18_years_of_age + 
    ##     percent_65_and_over + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - percent_fair_or_poor_health                    1 0.0002195 0.20508 -14349
    ## <none>                                                       0.20487 -14349
    ## - percent_adults_with_diabetes                   1 0.0002619 0.20513 -14349
    ## - percent_adults_with_obesity                    1 0.0003723 0.20524 -14348
    ## - percent_less_than_18_years_of_age              1 0.0003726 0.20524 -14348
    ## - percent_food_insecure                          1 0.0004370 0.20530 -14348
    ## - percent_limited_access_to_healthy_foods        1 0.0004593 0.20533 -14347
    ## - food_environment_index                         1 0.0004912 0.20536 -14347
    ## - percent_physically_inactive                    1 0.0005934 0.20546 -14346
    ## - percent_disabled                               1 0.0008973 0.20576 -14344
    ## - percent_with_access_to_exercise_opportunities  1 0.0012143 0.20608 -14341
    ## - percent_uninsured                              1 0.0012560 0.20612 -14341
    ## - percent_single_parent_households_CHR           1 0.0015368 0.20640 -14339
    ## - preventable_hospitalization_rate               1 0.0016532 0.20652 -14338
    ## - hiv_prevalence_rate                            1 0.0024885 0.20735 -14332
    ## - percent_children_in_poverty                    1 0.0029594 0.20783 -14328
    ## - injury_death_rate                              1 0.0035543 0.20842 -14323
    ## - median_household_income                        1 0.0042345 0.20910 -14318
    ## - life_expectancy                                1 0.0052088 0.21007 -14311
    ## - percent_smokers                                1 0.0054066 0.21027 -14309
    ## - percent_65_and_over                            1 0.0055808 0.21045 -14308
    ## 
    ## Step:  AIC=-14349.16
    ## death_rate ~ percent_smokers + percent_adults_with_obesity + 
    ##     food_environment_index + percent_physically_inactive + percent_with_access_to_exercise_opportunities + 
    ##     percent_uninsured + preventable_hospitalization_rate + percent_children_in_poverty + 
    ##     percent_single_parent_households_CHR + injury_death_rate + 
    ##     life_expectancy + percent_adults_with_diabetes + hiv_prevalence_rate + 
    ##     percent_food_insecure + percent_limited_access_to_healthy_foods + 
    ##     median_household_income + percent_less_than_18_years_of_age + 
    ##     percent_65_and_over + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## <none>                                                       0.20508 -14349
    ## - percent_adults_with_diabetes                   1 0.0003022 0.20539 -14349
    ## - percent_less_than_18_years_of_age              1 0.0003181 0.20540 -14349
    ## - percent_food_insecure                          1 0.0004146 0.20550 -14348
    ## - percent_adults_with_obesity                    1 0.0004162 0.20550 -14348
    ## - percent_limited_access_to_healthy_foods        1 0.0004408 0.20553 -14348
    ## - food_environment_index                         1 0.0004665 0.20555 -14348
    ## - percent_physically_inactive                    1 0.0006344 0.20572 -14346
    ## - percent_disabled                               1 0.0007712 0.20586 -14345
    ## - percent_with_access_to_exercise_opportunities  1 0.0012695 0.20636 -14341
    ## - preventable_hospitalization_rate               1 0.0016895 0.20677 -14338
    ## - percent_single_parent_households_CHR           1 0.0017461 0.20683 -14338
    ## - percent_uninsured                              1 0.0017875 0.20687 -14337
    ## - hiv_prevalence_rate                            1 0.0025131 0.20760 -14332
    ## - injury_death_rate                              1 0.0037703 0.20886 -14322
    ## - percent_children_in_poverty                    1 0.0040242 0.20911 -14320
    ## - median_household_income                        1 0.0042899 0.20938 -14318
    ## - life_expectancy                                1 0.0051305 0.21022 -14312
    ## - percent_65_and_over                            1 0.0053633 0.21045 -14310
    ## - percent_smokers                                1 0.0054466 0.21053 -14309

    ## [1] 20

    ##                    ME       RMSE         MAE  MPE MAPE
    ## Test set -0.001157879 0.01181613 0.008414405 -Inf  Inf

    ## Start:  AIC=-14338.65
    ## death_rate ~ total_population + percent_fair_or_poor_health + 
    ##     percent_smokers + percent_adults_with_obesity + food_environment_index + 
    ##     percent_physically_inactive + percent_with_access_to_exercise_opportunities + 
    ##     percent_excessive_drinking + percent_uninsured + primary_care_physicians_rate + 
    ##     preventable_hospitalization_rate + percent_vaccinated + percent_children_in_poverty + 
    ##     percent_single_parent_households_CHR + injury_death_rate + 
    ##     overcrowding + inadequate_facilities + life_expectancy + 
    ##     percent_adults_with_diabetes + hiv_prevalence_rate + percent_food_insecure + 
    ##     percent_limited_access_to_healthy_foods + median_household_income + 
    ##     percent_less_than_18_years_of_age + percent_65_and_over + 
    ##     percent_rural + percent_below_poverty + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - primary_care_physicians_rate                   1 0.0000005 0.20413 -14341
    ## - overcrowding                                   1 0.0000026 0.20413 -14341
    ## - percent_below_poverty                          1 0.0000123 0.20414 -14341
    ## - percent_rural                                  1 0.0001224 0.20425 -14340
    ## - inadequate_facilities                          1 0.0001363 0.20427 -14340
    ## - total_population                               1 0.0001519 0.20428 -14340
    ## - percent_vaccinated                             1 0.0001691 0.20430 -14339
    ## - percent_excessive_drinking                     1 0.0001997 0.20433 -14339
    ## - percent_less_than_18_years_of_age              1 0.0002315 0.20436 -14339
    ## - percent_adults_with_diabetes                   1 0.0002355 0.20436 -14339
    ## <none>                                                       0.20413 -14339
    ## - percent_adults_with_obesity                    1 0.0003894 0.20452 -14338
    ## - percent_food_insecure                          1 0.0004078 0.20454 -14338
    ## - percent_limited_access_to_healthy_foods        1 0.0004243 0.20455 -14337
    ## - food_environment_index                         1 0.0004510 0.20458 -14337
    ## - percent_fair_or_poor_health                    1 0.0004598 0.20459 -14337
    ## - percent_with_access_to_exercise_opportunities  1 0.0005986 0.20473 -14336
    ## - percent_physically_inactive                    1 0.0006102 0.20474 -14336
    ## - percent_disabled                               1 0.0008091 0.20494 -14334
    ## - percent_uninsured                              1 0.0012538 0.20538 -14331
    ## - percent_single_parent_households_CHR           1 0.0015315 0.20566 -14329
    ## - preventable_hospitalization_rate               1 0.0016295 0.20576 -14328
    ## - hiv_prevalence_rate                            1 0.0021316 0.20626 -14324
    ## - percent_children_in_poverty                    1 0.0024325 0.20656 -14322
    ## - injury_death_rate                              1 0.0033770 0.20751 -14314
    ## - median_household_income                        1 0.0038359 0.20797 -14311
    ## - percent_65_and_over                            1 0.0049330 0.20906 -14302
    ## - life_expectancy                                1 0.0052401 0.20937 -14300
    ## - percent_smokers                                1 0.0053178 0.20945 -14299
    ## 
    ## Step:  AIC=-14340.65
    ## death_rate ~ total_population + percent_fair_or_poor_health + 
    ##     percent_smokers + percent_adults_with_obesity + food_environment_index + 
    ##     percent_physically_inactive + percent_with_access_to_exercise_opportunities + 
    ##     percent_excessive_drinking + percent_uninsured + preventable_hospitalization_rate + 
    ##     percent_vaccinated + percent_children_in_poverty + percent_single_parent_households_CHR + 
    ##     injury_death_rate + overcrowding + inadequate_facilities + 
    ##     life_expectancy + percent_adults_with_diabetes + hiv_prevalence_rate + 
    ##     percent_food_insecure + percent_limited_access_to_healthy_foods + 
    ##     median_household_income + percent_less_than_18_years_of_age + 
    ##     percent_65_and_over + percent_rural + percent_below_poverty + 
    ##     percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - overcrowding                                   1 0.0000028 0.20413 -14343
    ## - percent_below_poverty                          1 0.0000121 0.20414 -14343
    ## - percent_rural                                  1 0.0001237 0.20425 -14342
    ## - inadequate_facilities                          1 0.0001380 0.20427 -14342
    ## - total_population                               1 0.0001524 0.20428 -14342
    ## - percent_vaccinated                             1 0.0001705 0.20430 -14341
    ## - percent_excessive_drinking                     1 0.0001992 0.20433 -14341
    ## - percent_less_than_18_years_of_age              1 0.0002314 0.20436 -14341
    ## - percent_adults_with_diabetes                   1 0.0002351 0.20436 -14341
    ## <none>                                                       0.20413 -14341
    ## - percent_adults_with_obesity                    1 0.0003992 0.20453 -14340
    ## - percent_food_insecure                          1 0.0004076 0.20454 -14339
    ## - percent_limited_access_to_healthy_foods        1 0.0004240 0.20455 -14339
    ## - food_environment_index                         1 0.0004507 0.20458 -14339
    ## - percent_fair_or_poor_health                    1 0.0004600 0.20459 -14339
    ## + primary_care_physicians_rate                   1 0.0000005 0.20413 -14339
    ## - percent_with_access_to_exercise_opportunities  1 0.0006070 0.20474 -14338
    ## - percent_physically_inactive                    1 0.0006172 0.20475 -14338
    ## - percent_disabled                               1 0.0008186 0.20495 -14336
    ## - percent_uninsured                              1 0.0012547 0.20539 -14333
    ## - percent_single_parent_households_CHR           1 0.0015317 0.20566 -14331
    ## - preventable_hospitalization_rate               1 0.0016321 0.20576 -14330
    ## - hiv_prevalence_rate                            1 0.0021315 0.20626 -14326
    ## - percent_children_in_poverty                    1 0.0024378 0.20657 -14324
    ## - injury_death_rate                              1 0.0033835 0.20751 -14316
    ## - median_household_income                        1 0.0038397 0.20797 -14313
    ## - percent_65_and_over                            1 0.0049693 0.20910 -14304
    ## - life_expectancy                                1 0.0052402 0.20937 -14302
    ## - percent_smokers                                1 0.0053175 0.20945 -14301
    ## 
    ## Step:  AIC=-14342.63
    ## death_rate ~ total_population + percent_fair_or_poor_health + 
    ##     percent_smokers + percent_adults_with_obesity + food_environment_index + 
    ##     percent_physically_inactive + percent_with_access_to_exercise_opportunities + 
    ##     percent_excessive_drinking + percent_uninsured + preventable_hospitalization_rate + 
    ##     percent_vaccinated + percent_children_in_poverty + percent_single_parent_households_CHR + 
    ##     injury_death_rate + inadequate_facilities + life_expectancy + 
    ##     percent_adults_with_diabetes + hiv_prevalence_rate + percent_food_insecure + 
    ##     percent_limited_access_to_healthy_foods + median_household_income + 
    ##     percent_less_than_18_years_of_age + percent_65_and_over + 
    ##     percent_rural + percent_below_poverty + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - percent_below_poverty                          1 0.0000152 0.20415 -14344
    ## - percent_rural                                  1 0.0001232 0.20426 -14344
    ## - inadequate_facilities                          1 0.0001364 0.20427 -14344
    ## - total_population                               1 0.0001501 0.20428 -14343
    ## - percent_vaccinated                             1 0.0001717 0.20430 -14343
    ## - percent_excessive_drinking                     1 0.0001967 0.20433 -14343
    ## - percent_adults_with_diabetes                   1 0.0002368 0.20437 -14343
    ## <none>                                                       0.20413 -14343
    ## - percent_less_than_18_years_of_age              1 0.0002745 0.20441 -14342
    ## - percent_adults_with_obesity                    1 0.0003984 0.20453 -14342
    ## - percent_food_insecure                          1 0.0004108 0.20454 -14341
    ## - percent_limited_access_to_healthy_foods        1 0.0004259 0.20456 -14341
    ## - food_environment_index                         1 0.0004524 0.20459 -14341
    ## - percent_fair_or_poor_health                    1 0.0004829 0.20462 -14341
    ## + overcrowding                                   1 0.0000028 0.20413 -14341
    ## + primary_care_physicians_rate                   1 0.0000008 0.20413 -14341
    ## - percent_with_access_to_exercise_opportunities  1 0.0006062 0.20474 -14340
    ## - percent_physically_inactive                    1 0.0006263 0.20476 -14340
    ## - percent_disabled                               1 0.0008179 0.20495 -14338
    ## - percent_uninsured                              1 0.0012720 0.20541 -14335
    ## - percent_single_parent_households_CHR           1 0.0015311 0.20566 -14333
    ## - preventable_hospitalization_rate               1 0.0016537 0.20579 -14332
    ## - hiv_prevalence_rate                            1 0.0021296 0.20626 -14328
    ## - percent_children_in_poverty                    1 0.0024353 0.20657 -14326
    ## - injury_death_rate                              1 0.0034248 0.20756 -14318
    ## - median_household_income                        1 0.0038407 0.20797 -14315
    ## - percent_65_and_over                            1 0.0049699 0.20910 -14306
    ## - life_expectancy                                1 0.0053107 0.20944 -14303
    ## - percent_smokers                                1 0.0055207 0.20965 -14302
    ## 
    ## Step:  AIC=-14344.51
    ## death_rate ~ total_population + percent_fair_or_poor_health + 
    ##     percent_smokers + percent_adults_with_obesity + food_environment_index + 
    ##     percent_physically_inactive + percent_with_access_to_exercise_opportunities + 
    ##     percent_excessive_drinking + percent_uninsured + preventable_hospitalization_rate + 
    ##     percent_vaccinated + percent_children_in_poverty + percent_single_parent_households_CHR + 
    ##     injury_death_rate + inadequate_facilities + life_expectancy + 
    ##     percent_adults_with_diabetes + hiv_prevalence_rate + percent_food_insecure + 
    ##     percent_limited_access_to_healthy_foods + median_household_income + 
    ##     percent_less_than_18_years_of_age + percent_65_and_over + 
    ##     percent_rural + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - percent_rural                                  1 0.0001177 0.20427 -14346
    ## - inadequate_facilities                          1 0.0001315 0.20428 -14346
    ## - total_population                               1 0.0001444 0.20429 -14345
    ## - percent_vaccinated                             1 0.0001666 0.20431 -14345
    ## - percent_excessive_drinking                     1 0.0001992 0.20435 -14345
    ## - percent_adults_with_diabetes                   1 0.0002402 0.20439 -14345
    ## <none>                                                       0.20415 -14344
    ## - percent_less_than_18_years_of_age              1 0.0002648 0.20441 -14344
    ## - percent_adults_with_obesity                    1 0.0003854 0.20453 -14344
    ## - percent_food_insecure                          1 0.0004048 0.20455 -14343
    ## - percent_limited_access_to_healthy_foods        1 0.0004241 0.20457 -14343
    ## - food_environment_index                         1 0.0004505 0.20460 -14343
    ## - percent_fair_or_poor_health                    1 0.0004693 0.20462 -14343
    ## + percent_below_poverty                          1 0.0000152 0.20413 -14343
    ## + overcrowding                                   1 0.0000059 0.20414 -14343
    ## + primary_care_physicians_rate                   1 0.0000006 0.20415 -14342
    ## - percent_with_access_to_exercise_opportunities  1 0.0006125 0.20476 -14342
    ## - percent_physically_inactive                    1 0.0006575 0.20481 -14341
    ## - percent_disabled                               1 0.0008140 0.20496 -14340
    ## - percent_uninsured                              1 0.0012943 0.20544 -14336
    ## - percent_single_parent_households_CHR           1 0.0015160 0.20566 -14335
    ## - preventable_hospitalization_rate               1 0.0016576 0.20581 -14334
    ## - hiv_prevalence_rate                            1 0.0022311 0.20638 -14329
    ## - percent_children_in_poverty                    1 0.0026158 0.20676 -14326
    ## - injury_death_rate                              1 0.0034284 0.20758 -14320
    ## - median_household_income                        1 0.0043444 0.20849 -14313
    ## - life_expectancy                                1 0.0054278 0.20958 -14304
    ## - percent_65_and_over                            1 0.0055241 0.20967 -14304
    ## - percent_smokers                                1 0.0055758 0.20972 -14303
    ## 
    ## Step:  AIC=-14345.58
    ## death_rate ~ total_population + percent_fair_or_poor_health + 
    ##     percent_smokers + percent_adults_with_obesity + food_environment_index + 
    ##     percent_physically_inactive + percent_with_access_to_exercise_opportunities + 
    ##     percent_excessive_drinking + percent_uninsured + preventable_hospitalization_rate + 
    ##     percent_vaccinated + percent_children_in_poverty + percent_single_parent_households_CHR + 
    ##     injury_death_rate + inadequate_facilities + life_expectancy + 
    ##     percent_adults_with_diabetes + hiv_prevalence_rate + percent_food_insecure + 
    ##     percent_limited_access_to_healthy_foods + median_household_income + 
    ##     percent_less_than_18_years_of_age + percent_65_and_over + 
    ##     percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - total_population                               1 0.0001126 0.20438 -14347
    ## - percent_vaccinated                             1 0.0001278 0.20439 -14347
    ## - inadequate_facilities                          1 0.0001474 0.20441 -14346
    ## - percent_excessive_drinking                     1 0.0001854 0.20445 -14346
    ## - percent_adults_with_diabetes                   1 0.0002512 0.20452 -14346
    ## <none>                                                       0.20427 -14346
    ## - percent_less_than_18_years_of_age              1 0.0002934 0.20456 -14345
    ## - percent_adults_with_obesity                    1 0.0003550 0.20462 -14345
    ## + percent_rural                                  1 0.0001177 0.20415 -14344
    ## - percent_fair_or_poor_health                    1 0.0003953 0.20466 -14344
    ## - percent_food_insecure                          1 0.0004072 0.20467 -14344
    ## - percent_limited_access_to_healthy_foods        1 0.0004210 0.20469 -14344
    ## - food_environment_index                         1 0.0004542 0.20472 -14344
    ## + percent_below_poverty                          1 0.0000097 0.20426 -14344
    ## + overcrowding                                   1 0.0000045 0.20426 -14344
    ## + primary_care_physicians_rate                   1 0.0000015 0.20427 -14344
    ## - percent_physically_inactive                    1 0.0006919 0.20496 -14342
    ## - percent_disabled                               1 0.0007937 0.20506 -14341
    ## - percent_uninsured                              1 0.0013288 0.20559 -14337
    ## - percent_single_parent_households_CHR           1 0.0014189 0.20569 -14336
    ## - percent_with_access_to_exercise_opportunities  1 0.0014294 0.20569 -14336
    ## - preventable_hospitalization_rate               1 0.0016411 0.20591 -14335
    ## - hiv_prevalence_rate                            1 0.0021683 0.20643 -14331
    ## - percent_children_in_poverty                    1 0.0027630 0.20703 -14326
    ## - injury_death_rate                              1 0.0033643 0.20763 -14321
    ## - median_household_income                        1 0.0042554 0.20852 -14314
    ## - life_expectancy                                1 0.0053255 0.20959 -14306
    ## - percent_smokers                                1 0.0054581 0.20972 -14305
    ## - percent_65_and_over                            1 0.0059536 0.21022 -14302
    ## 
    ## Step:  AIC=-14346.7
    ## death_rate ~ percent_fair_or_poor_health + percent_smokers + 
    ##     percent_adults_with_obesity + food_environment_index + percent_physically_inactive + 
    ##     percent_with_access_to_exercise_opportunities + percent_excessive_drinking + 
    ##     percent_uninsured + preventable_hospitalization_rate + percent_vaccinated + 
    ##     percent_children_in_poverty + percent_single_parent_households_CHR + 
    ##     injury_death_rate + inadequate_facilities + life_expectancy + 
    ##     percent_adults_with_diabetes + hiv_prevalence_rate + percent_food_insecure + 
    ##     percent_limited_access_to_healthy_foods + median_household_income + 
    ##     percent_less_than_18_years_of_age + percent_65_and_over + 
    ##     percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - percent_vaccinated                             1 0.0001249 0.20450 -14348
    ## - inadequate_facilities                          1 0.0001581 0.20454 -14348
    ## - percent_excessive_drinking                     1 0.0001771 0.20456 -14347
    ## <none>                                                       0.20438 -14347
    ## - percent_adults_with_diabetes                   1 0.0002591 0.20464 -14347
    ## - percent_less_than_18_years_of_age              1 0.0002817 0.20466 -14346
    ## + total_population                               1 0.0001126 0.20427 -14346
    ## - percent_adults_with_obesity                    1 0.0003999 0.20478 -14346
    ## - percent_food_insecure                          1 0.0004052 0.20478 -14346
    ## - percent_fair_or_poor_health                    1 0.0004146 0.20479 -14345
    ## - percent_limited_access_to_healthy_foods        1 0.0004168 0.20480 -14345
    ## + percent_rural                                  1 0.0000858 0.20429 -14345
    ## - food_environment_index                         1 0.0004527 0.20483 -14345
    ## + percent_below_poverty                          1 0.0000062 0.20437 -14345
    ## + primary_care_physicians_rate                   1 0.0000008 0.20438 -14345
    ## + overcrowding                                   1 0.0000000 0.20438 -14345
    ## - percent_physically_inactive                    1 0.0006813 0.20506 -14343
    ## - percent_disabled                               1 0.0007984 0.20518 -14342
    ## - percent_uninsured                              1 0.0012899 0.20567 -14339
    ## - percent_with_access_to_exercise_opportunities  1 0.0013414 0.20572 -14338
    ## - percent_single_parent_households_CHR           1 0.0014064 0.20578 -14338
    ## - preventable_hospitalization_rate               1 0.0016732 0.20605 -14336
    ## - hiv_prevalence_rate                            1 0.0026070 0.20699 -14328
    ## - percent_children_in_poverty                    1 0.0028044 0.20718 -14327
    ## - injury_death_rate                              1 0.0033492 0.20773 -14323
    ## - median_household_income                        1 0.0043200 0.20870 -14315
    ## - life_expectancy                                1 0.0052762 0.20966 -14308
    ## - percent_smokers                                1 0.0055944 0.20997 -14305
    ## - percent_65_and_over                            1 0.0058955 0.21027 -14303
    ## 
    ## Step:  AIC=-14347.72
    ## death_rate ~ percent_fair_or_poor_health + percent_smokers + 
    ##     percent_adults_with_obesity + food_environment_index + percent_physically_inactive + 
    ##     percent_with_access_to_exercise_opportunities + percent_excessive_drinking + 
    ##     percent_uninsured + preventable_hospitalization_rate + percent_children_in_poverty + 
    ##     percent_single_parent_households_CHR + injury_death_rate + 
    ##     inadequate_facilities + life_expectancy + percent_adults_with_diabetes + 
    ##     hiv_prevalence_rate + percent_food_insecure + percent_limited_access_to_healthy_foods + 
    ##     median_household_income + percent_less_than_18_years_of_age + 
    ##     percent_65_and_over + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - percent_excessive_drinking                     1 0.0001488 0.20465 -14349
    ## - inadequate_facilities                          1 0.0001704 0.20467 -14348
    ## <none>                                                       0.20450 -14348
    ## - percent_adults_with_diabetes                   1 0.0002941 0.20480 -14347
    ## - percent_less_than_18_years_of_age              1 0.0003463 0.20485 -14347
    ## - percent_fair_or_poor_health                    1 0.0003719 0.20488 -14347
    ## + percent_vaccinated                             1 0.0001249 0.20438 -14347
    ## - percent_adults_with_obesity                    1 0.0003917 0.20489 -14347
    ## + total_population                               1 0.0001096 0.20439 -14347
    ## - percent_food_insecure                          1 0.0004085 0.20491 -14346
    ## - percent_limited_access_to_healthy_foods        1 0.0004209 0.20492 -14346
    ## + percent_rural                                  1 0.0000545 0.20445 -14346
    ## - food_environment_index                         1 0.0004580 0.20496 -14346
    ## + percent_below_poverty                          1 0.0000040 0.20450 -14346
    ## + overcrowding                                   1 0.0000000 0.20450 -14346
    ## + primary_care_physicians_rate                   1 0.0000000 0.20450 -14346
    ## - percent_physically_inactive                    1 0.0006677 0.20517 -14344
    ## - percent_disabled                               1 0.0008987 0.20540 -14343
    ## - percent_uninsured                              1 0.0012718 0.20578 -14340
    ## - percent_with_access_to_exercise_opportunities  1 0.0012730 0.20578 -14340
    ## - percent_single_parent_households_CHR           1 0.0015338 0.20604 -14338
    ## - preventable_hospitalization_rate               1 0.0016474 0.20615 -14337
    ## - hiv_prevalence_rate                            1 0.0025802 0.20708 -14330
    ## - percent_children_in_poverty                    1 0.0026805 0.20718 -14329
    ## - injury_death_rate                              1 0.0035901 0.20809 -14322
    ## - median_household_income                        1 0.0043978 0.20890 -14316
    ## - percent_smokers                                1 0.0054712 0.20997 -14307
    ## - life_expectancy                                1 0.0054893 0.20999 -14307
    ## - percent_65_and_over                            1 0.0058090 0.21031 -14305
    ## 
    ## Step:  AIC=-14348.55
    ## death_rate ~ percent_fair_or_poor_health + percent_smokers + 
    ##     percent_adults_with_obesity + food_environment_index + percent_physically_inactive + 
    ##     percent_with_access_to_exercise_opportunities + percent_uninsured + 
    ##     preventable_hospitalization_rate + percent_children_in_poverty + 
    ##     percent_single_parent_households_CHR + injury_death_rate + 
    ##     inadequate_facilities + life_expectancy + percent_adults_with_diabetes + 
    ##     hiv_prevalence_rate + percent_food_insecure + percent_limited_access_to_healthy_foods + 
    ##     median_household_income + percent_less_than_18_years_of_age + 
    ##     percent_65_and_over + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - inadequate_facilities                          1 0.0002135 0.20487 -14349
    ## - percent_adults_with_diabetes                   1 0.0002484 0.20490 -14349
    ## <none>                                                       0.20465 -14349
    ## - percent_fair_or_poor_health                    1 0.0002585 0.20491 -14348
    ## + percent_excessive_drinking                     1 0.0001488 0.20450 -14348
    ## - percent_adults_with_obesity                    1 0.0003791 0.20503 -14348
    ## - percent_food_insecure                          1 0.0003981 0.20505 -14347
    ## - percent_less_than_18_years_of_age              1 0.0004017 0.20505 -14347
    ## + total_population                               1 0.0001023 0.20455 -14347
    ## + percent_vaccinated                             1 0.0000966 0.20456 -14347
    ## - percent_limited_access_to_healthy_foods        1 0.0004179 0.20507 -14347
    ## - food_environment_index                         1 0.0004534 0.20511 -14347
    ## + percent_rural                                  1 0.0000499 0.20460 -14347
    ## + percent_below_poverty                          1 0.0000057 0.20465 -14347
    ## + overcrowding                                   1 0.0000026 0.20465 -14347
    ## + primary_care_physicians_rate                   1 0.0000008 0.20465 -14347
    ## - percent_physically_inactive                    1 0.0006345 0.20529 -14346
    ## - percent_disabled                               1 0.0009448 0.20560 -14343
    ## - percent_with_access_to_exercise_opportunities  1 0.0012041 0.20586 -14341
    ## - percent_uninsured                              1 0.0012231 0.20588 -14341
    ## - percent_single_parent_households_CHR           1 0.0015824 0.20624 -14338
    ## - preventable_hospitalization_rate               1 0.0017356 0.20639 -14337
    ## - hiv_prevalence_rate                            1 0.0025960 0.20725 -14330
    ## - percent_children_in_poverty                    1 0.0026961 0.20735 -14330
    ## - injury_death_rate                              1 0.0037408 0.20839 -14322
    ## - median_household_income                        1 0.0042492 0.20890 -14318
    ## - life_expectancy                                1 0.0053672 0.21002 -14309
    ## - percent_smokers                                1 0.0053869 0.21004 -14309
    ## - percent_65_and_over                            1 0.0056972 0.21035 -14306
    ## 
    ## Step:  AIC=-14348.88
    ## death_rate ~ percent_fair_or_poor_health + percent_smokers + 
    ##     percent_adults_with_obesity + food_environment_index + percent_physically_inactive + 
    ##     percent_with_access_to_exercise_opportunities + percent_uninsured + 
    ##     preventable_hospitalization_rate + percent_children_in_poverty + 
    ##     percent_single_parent_households_CHR + injury_death_rate + 
    ##     life_expectancy + percent_adults_with_diabetes + hiv_prevalence_rate + 
    ##     percent_food_insecure + percent_limited_access_to_healthy_foods + 
    ##     median_household_income + percent_less_than_18_years_of_age + 
    ##     percent_65_and_over + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## - percent_fair_or_poor_health                    1 0.0002195 0.20508 -14349
    ## <none>                                                       0.20487 -14349
    ## - percent_adults_with_diabetes                   1 0.0002619 0.20513 -14349
    ## + inadequate_facilities                          1 0.0002135 0.20465 -14349
    ## + percent_excessive_drinking                     1 0.0001918 0.20467 -14348
    ## - percent_adults_with_obesity                    1 0.0003723 0.20524 -14348
    ## - percent_less_than_18_years_of_age              1 0.0003726 0.20524 -14348
    ## + total_population                               1 0.0001132 0.20475 -14348
    ## + percent_vaccinated                             1 0.0001051 0.20476 -14348
    ## - percent_food_insecure                          1 0.0004370 0.20530 -14348
    ## + percent_rural                                  1 0.0000594 0.20481 -14347
    ## - percent_limited_access_to_healthy_foods        1 0.0004593 0.20533 -14347
    ## + overcrowding                                   1 0.0000291 0.20484 -14347
    ## - food_environment_index                         1 0.0004912 0.20536 -14347
    ## + percent_below_poverty                          1 0.0000020 0.20486 -14347
    ## + primary_care_physicians_rate                   1 0.0000002 0.20486 -14347
    ## - percent_physically_inactive                    1 0.0005934 0.20546 -14346
    ## - percent_disabled                               1 0.0008973 0.20576 -14344
    ## - percent_with_access_to_exercise_opportunities  1 0.0012143 0.20608 -14341
    ## - percent_uninsured                              1 0.0012560 0.20612 -14341
    ## - percent_single_parent_households_CHR           1 0.0015368 0.20640 -14339
    ## - preventable_hospitalization_rate               1 0.0016532 0.20652 -14338
    ## - hiv_prevalence_rate                            1 0.0024885 0.20735 -14332
    ## - percent_children_in_poverty                    1 0.0029594 0.20783 -14328
    ## - injury_death_rate                              1 0.0035543 0.20842 -14323
    ## - median_household_income                        1 0.0042345 0.20910 -14318
    ## - life_expectancy                                1 0.0052088 0.21007 -14311
    ## - percent_smokers                                1 0.0054066 0.21027 -14309
    ## - percent_65_and_over                            1 0.0055808 0.21045 -14308
    ## 
    ## Step:  AIC=-14349.16
    ## death_rate ~ percent_smokers + percent_adults_with_obesity + 
    ##     food_environment_index + percent_physically_inactive + percent_with_access_to_exercise_opportunities + 
    ##     percent_uninsured + preventable_hospitalization_rate + percent_children_in_poverty + 
    ##     percent_single_parent_households_CHR + injury_death_rate + 
    ##     life_expectancy + percent_adults_with_diabetes + hiv_prevalence_rate + 
    ##     percent_food_insecure + percent_limited_access_to_healthy_foods + 
    ##     median_household_income + percent_less_than_18_years_of_age + 
    ##     percent_65_and_over + percent_disabled
    ## 
    ##                                                 Df Sum of Sq     RSS    AIC
    ## <none>                                                       0.20508 -14349
    ## + percent_fair_or_poor_health                    1 0.0002195 0.20487 -14349
    ## - percent_adults_with_diabetes                   1 0.0003022 0.20539 -14349
    ## - percent_less_than_18_years_of_age              1 0.0003181 0.20540 -14349
    ## + inadequate_facilities                          1 0.0001744 0.20491 -14348
    ## + total_population                               1 0.0001309 0.20495 -14348
    ## - percent_food_insecure                          1 0.0004146 0.20550 -14348
    ## - percent_adults_with_obesity                    1 0.0004162 0.20550 -14348
    ## + overcrowding                                   1 0.0000910 0.20499 -14348
    ## + percent_vaccinated                             1 0.0000824 0.20500 -14348
    ## - percent_limited_access_to_healthy_foods        1 0.0004408 0.20553 -14348
    ## + percent_excessive_drinking                     1 0.0000605 0.20503 -14348
    ## - food_environment_index                         1 0.0004665 0.20555 -14348
    ## + percent_rural                                  1 0.0000217 0.20506 -14347
    ## + percent_below_poverty                          1 0.0000047 0.20508 -14347
    ## + primary_care_physicians_rate                   1 0.0000017 0.20508 -14347
    ## - percent_physically_inactive                    1 0.0006344 0.20572 -14346
    ## - percent_disabled                               1 0.0007712 0.20586 -14345
    ## - percent_with_access_to_exercise_opportunities  1 0.0012695 0.20636 -14341
    ## - preventable_hospitalization_rate               1 0.0016895 0.20677 -14338
    ## - percent_single_parent_households_CHR           1 0.0017461 0.20683 -14338
    ## - percent_uninsured                              1 0.0017875 0.20687 -14337
    ## - hiv_prevalence_rate                            1 0.0025131 0.20760 -14332
    ## - injury_death_rate                              1 0.0037703 0.20886 -14322
    ## - percent_children_in_poverty                    1 0.0040242 0.20911 -14320
    ## - median_household_income                        1 0.0042899 0.20938 -14318
    ## - life_expectancy                                1 0.0051305 0.21022 -14312
    ## - percent_65_and_over                            1 0.0053633 0.21045 -14310
    ## - percent_smokers                                1 0.0054466 0.21053 -14309

    ## 
    ## Call:
    ## lm(formula = death_rate ~ percent_smokers + percent_adults_with_obesity + 
    ##     food_environment_index + percent_physically_inactive + percent_with_access_to_exercise_opportunities + 
    ##     percent_uninsured + preventable_hospitalization_rate + percent_children_in_poverty + 
    ##     percent_single_parent_households_CHR + injury_death_rate + 
    ##     life_expectancy + percent_adults_with_diabetes + hiv_prevalence_rate + 
    ##     percent_food_insecure + percent_limited_access_to_healthy_foods + 
    ##     median_household_income + percent_less_than_18_years_of_age + 
    ##     percent_65_and_over + percent_disabled, data = na.omit(train.data))
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.025446 -0.006534 -0.001914  0.004308  0.085235 
    ## 
    ## Coefficients:
    ##                                                 Estimate Std. Error t value
    ## (Intercept)                                   -9.106e-02  1.077e-01  -0.846
    ## percent_smokers                               -9.384e-04  1.446e-04  -6.488
    ## percent_adults_with_obesity                   -1.371e-04  7.644e-05  -1.794
    ## food_environment_index                         1.879e-02  9.895e-03   1.899
    ## percent_physically_inactive                    1.686e-04  7.615e-05   2.214
    ## percent_with_access_to_exercise_opportunities -5.296e-05  1.691e-05  -3.132
    ## percent_uninsured                              2.739e-04  7.369e-05   3.717
    ## preventable_hospitalization_rate               7.402e-07  2.048e-07   3.614
    ## percent_children_in_poverty                    4.581e-04  8.215e-05   5.577
    ## percent_single_parent_households_CHR           1.899e-04  5.170e-05   3.673
    ## injury_death_rate                             -1.009e-04  1.870e-05  -5.398
    ## life_expectancy                               -1.334e-03  2.118e-04  -6.297
    ## percent_adults_with_diabetes                   1.571e-04  1.028e-04   1.528
    ## hiv_prevalence_rate                            9.260e-06  2.101e-06   4.407
    ## percent_food_insecure                          3.331e-03  1.861e-03   1.790
    ## percent_limited_access_to_healthy_foods        1.619e-03  8.772e-04   1.846
    ## median_household_income                        2.404e-07  4.174e-08   5.758
    ## percent_less_than_18_years_of_age             -2.136e-04  1.362e-04  -1.568
    ## percent_65_and_over                            7.323e-04  1.138e-04   6.438
    ## percent_disabled                              -3.049e-04  1.249e-04  -2.441
    ##                                               Pr(>|t|)    
    ## (Intercept)                                   0.397852    
    ## percent_smokers                               1.16e-10 ***
    ## percent_adults_with_obesity                   0.073074 .  
    ## food_environment_index                        0.057786 .  
    ## percent_physically_inactive                   0.026956 *  
    ## percent_with_access_to_exercise_opportunities 0.001766 ** 
    ## percent_uninsured                             0.000209 ***
    ## preventable_hospitalization_rate              0.000311 ***
    ## percent_children_in_poverty                   2.88e-08 ***
    ## percent_single_parent_households_CHR          0.000247 ***
    ## injury_death_rate                             7.76e-08 ***
    ## life_expectancy                               3.92e-10 ***
    ## percent_adults_with_diabetes                  0.126634    
    ## hiv_prevalence_rate                           1.12e-05 ***
    ## percent_food_insecure                         0.073650 .  
    ## percent_limited_access_to_healthy_foods       0.065131 .  
    ## median_household_income                       1.02e-08 ***
    ## percent_less_than_18_years_of_age             0.117078    
    ## percent_65_and_over                           1.60e-10 ***
    ## percent_disabled                              0.014741 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.01138 on 1585 degrees of freedom
    ## Multiple R-squared:  0.2627, Adjusted R-squared:  0.2539 
    ## F-statistic: 29.73 on 19 and 1585 DF,  p-value: < 2.2e-16

    ## [1] 20

    ##                     ME       RMSE         MAE  MPE MAPE
    ## Test set -0.0009712068 0.01143146 0.008329115 -Inf  Inf

    ##                    ME       RMSE         MAE  MPE MAPE
    ## Test set -0.001005406 0.01142413 0.008321012 -Inf  Inf

    ##                    ME       RMSE         MAE  MPE MAPE
    ## Test set -0.001179069 0.01178969 0.008409832 -Inf  Inf

    ##                     ME       RMSE         MAE  MPE MAPE
    ## Test set -0.0009712068 0.01143146 0.008329115 -Inf  Inf

    ##                    ME       RMSE         MAE  MPE MAPE
    ## Test set -0.001157879 0.01181613 0.008414405 -Inf  Inf

    ##                     ME       RMSE         MAE  MPE MAPE
    ## Test set -0.0009712068 0.01143146 0.008329115 -Inf  Inf

    ##                    ME       RMSE         MAE  MPE MAPE
    ## Test set -0.001157879 0.01181613 0.008414405 -Inf  Inf

    ##                    ME       RMSE         MAE  MPE MAPE
    ## Test set -0.001347278 0.01134669 0.008154835 -Inf  Inf

    ##  [1] "(Intercept)"                                  
    ##  [2] "percent_children_in_poverty"                  
    ##  [3] "percent_smokers"                              
    ##  [4] "hiv_prevalence_rate"                          
    ##  [5] "percent_physically_inactive"                  
    ##  [6] "percent_65_and_over"                          
    ##  [7] "median_household_income"                      
    ##  [8] "preventable_hospitalization_rate"             
    ##  [9] "life_expectancy"                              
    ## [10] "injury_death_rate"                            
    ## [11] "percent_uninsured"                            
    ## [12] "percent_single_parent_households_CHR"         
    ## [13] "percent_with_access_to_exercise_opportunities"
    ## [14] "percent_disabled"                             
    ## [15] "food_environment_index"                       
    ## [16] "percent_less_than_18_years_of_age"            
    ## [17] "percent_adults_with_obesity"                  
    ## [18] "percent_adults_with_diabetes"

Validation Modeling using Selected model

``` r
reg.selected <- forward.logit

predicted <- predict(reg.selected,valid.data,type = "response")

table <- add_column(valid.data, predicted, .after="death_rate") %>% 
  select("death_rate","predicted") 

validation_counties <- data.frame(master_data[validation_rows,1:2],table)

validation_counties <- validation_counties %>% 
  mutate(abs_res = abs(death_rate - predicted)) %>% 
  arrange(abs_res) %>% 
  filter(!is.na(predicted))

worst_predictions <- tail(validation_counties)
best_predictions <- head(validation_counties)

high_predictions <- head(validation_counties %>% 
  arrange(desc(predicted)),10)

high_predictions_and_real <- head(validation_counties %>% 
                                    arrange(desc(predicted+death_rate)),10)

high_predictions_and_real
```

    ##                   state               county death_rate  predicted     abs_res
    ## 1                 Texas               Marion 0.09139785 0.03011729 0.061280563
    ## 2              New York              Orleans 0.08630952 0.02303882 0.063270700
    ## 3               Florida               Sumter 0.02771679 0.08026658 0.052549784
    ## 4               Florida                Union 0.04883907 0.05497523 0.006136156
    ## 5                 Texas               Brooks 0.06024096 0.03722733 0.023013639
    ## 6               Georgia              Jenkins 0.06772908 0.02803763 0.039691452
    ## 7              Virginia            Middlesex 0.06111111 0.03420776 0.026903351
    ## 8  District of Columbia District of Columbia 0.03082740 0.06372821 0.032900806
    ## 9               Georgia                Baker 0.04687500 0.04467334 0.002201659
    ## 10              Georgia               Sumter 0.05378788 0.03632195 0.017465933

Re-devise model with all data as training

``` r
forward.logit.all.counties <- glm(death_rate~.,data = 
                       modeling_data[,c(names(dr.lm.forward$coefficients)[-1],
                                     "death_rate")],
                     family = "quasibinomial")

predicted <- predict(forward.logit.all.counties,modeling_data,type = "response")

table <- add_column(modeling_data, predicted, .after="death_rate") %>% 
  select("death_rate","predicted") 

all_counties <- data.frame(master_data[,1:2],table)

all_counties <- all_counties %>% 
  mutate(abs_res = abs(death_rate - predicted)) %>% 
  mutate(residual = death_rate - predicted) %>% 
  arrange(abs_res) %>% 
  filter(!is.na(predicted))

worst_predictions <- tail(all_counties)
best_predictions <- head(all_counties)

high_predictions <- head(all_counties %>% 
  arrange(desc(predicted)),10)

high_predictions_and_real <- head(all_counties %>% 
                                    arrange(desc(predicted+death_rate)),10)

high_predictions_and_real
```

    ##            state      county death_rate  predicted    abs_res   residual
    ## 1        Georgia    Randolph 0.08378378 0.04598234 0.03780144 0.03780144
    ## 2        Georgia     Hancock 0.08768657 0.04172357 0.04596300 0.04596300
    ## 3       Virginia Northampton 0.08806818 0.03720007 0.05086811 0.05086811
    ## 4  Massachusetts    Franklin 0.10614525 0.01832656 0.08781869 0.08781869
    ## 5          Texas      Crosby 0.08791209 0.03452598 0.05338611 0.05338611
    ## 6          Texas      Marion 0.09139785 0.03005437 0.06134348 0.06134348
    ## 7        Georgia     Terrell 0.07875895 0.04138542 0.03737353 0.03737353
    ## 8          Texas       Garza 0.09090909 0.02666733 0.06424176 0.06424176
    ## 9          Texas       Floyd 0.08585859 0.02530172 0.06055687 0.06055687
    ## 10      New York     Orleans 0.08630952 0.02266725 0.06364227 0.06364227

``` r
tail(all_counties %>% arrange(desc(predicted+death_rate)),10)
```

    ##              state        county  death_rate   predicted      abs_res
    ## 2164 West Virginia    Monongalia 0.002292264 0.009538447 0.0072461839
    ## 2165        Nevada      Pershing 0.000000000 0.011760190 0.0117601903
    ## 2166          Utah         Cache 0.001901141 0.009787582 0.0078864412
    ## 2167       Wyoming        Albany 0.002830189 0.007824461 0.0049942719
    ## 2168       Wyoming         Teton 0.001160093 0.009362321 0.0082022282
    ## 2169    California          Mono 0.005454545 0.004885862 0.0005686833
    ## 2170       Georgia Chattahoochee 0.001958864 0.008292752 0.0063338879
    ## 2171         Idaho         Latah 0.001648352 0.008270075 0.0066217237
    ## 2172       Wyoming      Niobrara 0.000000000 0.007829806 0.0078298063
    ## 2173      Colorado        Summit 0.002430134 0.004606068 0.0021759342
    ##           residual
    ## 2164 -0.0072461839
    ## 2165 -0.0117601903
    ## 2166 -0.0078864412
    ## 2167 -0.0049942719
    ## 2168 -0.0082022282
    ## 2169  0.0005686833
    ## 2170 -0.0063338879
    ## 2171 -0.0066217237
    ## 2172 -0.0078298063
    ## 2173 -0.0021759342

Mapping

    ## [1] "state"      "county"     "death_rate" "predicted"  "abs_res"   
    ## [6] "residual"

    ## [1] "state"      "county"     "death_rate" "lat"        "lon"       
    ## [6] "predicted"  "residual"   "res_sign"   "abs_res"

![](README_figs/README-unnamed-chunk-13-1.png)<!-- -->![](README_figs/README-unnamed-chunk-13-2.png)<!-- -->![](README_figs/README-unnamed-chunk-13-3.png)<!-- -->

``` r
library(tidyverse)
county.geom <- read_csv("us_county_geometry.csv")
county.geom$state <- str_to_title(county.geom$state)

all_counties_map$state <- as.character(all_counties_map$state)
all_counties_map$county <- as.character(all_counties_map$county)

all_counties2 <- inner_join(all_counties_map, county.geom)
```

``` r
#install.packages("usmap")
library(usmap)
library(usdata)
library(ggimage) # Load ggimage
library(reshape2)
library(ggpubr)

#all_counties2$fips <- paste(fips(all_counties2$state),all_counties2$fips,sep = "")

all_counties2$abbr <- state2abbr(all_counties2$state)
all_counties2$county = paste(all_counties2$county, "County")

all_counties2 <- all_counties2 %>% 
  select('fips','abbr','county','death_rate', "predicted") 


a <-plot_usmap(data = all_counties2, values = "death_rate", color = NA) +
  scale_fill_continuous(low = "white", high = "red", name = "Death Rate by County", label = scales::comma) +
  theme(legend.position = "bottom") +
  ggtitle("Actual")

b <-plot_usmap(data = all_counties2, values = "predicted", color = NA) +
  scale_fill_continuous(low = "white", high = "red", name = "Death Rate by County", label = scales::comma) +
  theme(legend.position = "bottom") +
  ggtitle("Predicted")

ggarrange(a,b)
```

![](README_figs/README-unnamed-chunk-15-1.png)<!-- -->
