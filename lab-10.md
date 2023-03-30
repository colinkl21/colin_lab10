Lab 10 - Grading the professor, Pt. 2
================
Colin Li
3/30/2023

### Load packages and data

``` r
library(tidyverse) 
```

    ## Warning: package 'tidyverse' was built under R version 4.2.3

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## Warning: package 'tibble' was built under R version 4.2.3

    ## Warning: package 'tidyr' was built under R version 4.2.3

    ## Warning: package 'readr' was built under R version 4.2.3

    ## Warning: package 'purrr' was built under R version 4.2.3

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## Warning: package 'forcats' was built under R version 4.2.3

    ## Warning: package 'lubridate' was built under R version 4.2.3

``` r
library(tidymodels)
```

    ## Warning: package 'tidymodels' was built under R version 4.2.3

    ## Warning: package 'broom' was built under R version 4.2.3

    ## Warning: package 'dials' was built under R version 4.2.3

    ## Warning: package 'infer' was built under R version 4.2.3

    ## Warning: package 'modeldata' was built under R version 4.2.3

    ## Warning: package 'parsnip' was built under R version 4.2.3

    ## Warning: package 'recipes' was built under R version 4.2.3

    ## Warning: package 'rsample' was built under R version 4.2.3

    ## Warning: package 'tune' was built under R version 4.2.3

    ## Warning: package 'workflows' was built under R version 4.2.3

    ## Warning: package 'workflowsets' was built under R version 4.2.3

    ## Warning: package 'yardstick' was built under R version 4.2.3

``` r
library(openintro)
```

    ## Warning: package 'openintro' was built under R version 4.2.3

    ## Warning: package 'airports' was built under R version 4.2.3

    ## Warning: package 'cherryblossom' was built under R version 4.2.3

    ## Warning: package 'usdata' was built under R version 4.2.3

### Part 1

``` r
m_bty <- lm(score~bty_avg, data = evals)
summary(m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

R2 = 0.03502, Adjusted R2 = 0.03293

### Part 2

``` r
m_bty_gen <- lm(score~bty_avg + gender, data = evals)
summary(m_bty_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

``` r
m_bty_rank <- lm(score~bty_avg + rank, data = evals)
summary(m_bty_rank)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + rank, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

Intercept: 3.747 the predicted evaluation score for female professors
when female professors’ attractiveness score is 0. the model explains
5.9% of variance in score. The male line means predicted male
professors’ evaluation score while controlling for attractiveness. when
they receive the same beauty score, male professors have higher
evaluation score. It varies because gender is a moderator. The adjusted
R2 increased about 2%, this means gender explains an additional unique
2% of variance in score. Adding gender to the model increased bty_avg
estimate a little bit. The new model, intercept means the predicted
evaluation score for teaching faculty when their attractiveness rating
is 0. Slope bty means 1 unit increase in attractiveness rating is
associated with .068 increase in evaluation score when controlling for
professors’ rank. Slope tenure track means if professors are tenured
tracked, their predicted evaluation score is 3.98155-0.16070, when
controlling for other ranks and attractiveness rating. Slope tenured
means if professors are tenured, their predicted evaluation score is
3.98155-0.12623, when controlling for other ranks and attractiveness.

### Part 3

``` r
m <- lm(score~rank + ethnicity + gender + language + age + cls_perc_eval + cls_did_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)

summary(m)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_did_eval + cls_students + cls_level + 
    ##     cls_profs + cls_credits + bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.83665 -0.31377  0.08559  0.35655  1.08091 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.6035360  0.2615008  13.780  < 2e-16 ***
    ## ranktenure track      -0.1022542  0.0823357  -1.242 0.214915    
    ## ranktenured           -0.0444115  0.0652594  -0.681 0.496514    
    ## ethnicitynot minority  0.1838073  0.0776989   2.366 0.018423 *  
    ## gendermale             0.1813064  0.0516980   3.507 0.000499 ***
    ## languagenon-english   -0.1297849  0.1081723  -1.200 0.230850    
    ## age                   -0.0065680  0.0030868  -2.128 0.033900 *  
    ## cls_perc_eval          0.0046764  0.0021063   2.220 0.026904 *  
    ## cls_did_eval           0.0022369  0.0031124   0.719 0.472698    
    ## cls_students          -0.0009486  0.0019726  -0.481 0.630832    
    ## cls_levelupper         0.0103812  0.0568080   0.183 0.855084    
    ## cls_profssingle       -0.0050013  0.0516204  -0.097 0.922860    
    ## cls_creditsone credit  0.5063406  0.1171236   4.323  1.9e-05 ***
    ## bty_avg                0.0609228  0.0166912   3.650 0.000293 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5043 on 449 degrees of freedom
    ## Multiple R-squared:  0.1645, Adjusted R-squared:  0.1403 
    ## F-statistic: 6.799 on 13 and 449 DF,  p-value: 5.372e-12

``` r
m2 <- lm(score~rank + ethnicity + gender + language + age + cls_perc_eval + cls_did_eval + cls_level + cls_profs + cls_credits + bty_avg, data = evals)

summary(m2)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_did_eval + cls_level + cls_profs + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84177 -0.31178  0.08699  0.35816  1.09685 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.5508754  0.2372645  14.966  < 2e-16 ***
    ## ranktenure track      -0.1057146  0.0819506  -1.290 0.197720    
    ## ranktenured           -0.0457961  0.0651401  -0.703 0.482395    
    ## ethnicitynot minority  0.1856665  0.0775364   2.395 0.017048 *  
    ## gendermale             0.1791494  0.0514590   3.481 0.000547 ***
    ## languagenon-english   -0.1274950  0.1079751  -1.181 0.238313    
    ## age                   -0.0066084  0.0030830  -2.143 0.032609 *  
    ## cls_perc_eval          0.0053957  0.0014816   3.642 0.000302 ***
    ## cls_did_eval           0.0007651  0.0005654   1.353 0.176659    
    ## cls_levelupper         0.0169653  0.0550862   0.308 0.758241    
    ## cls_profssingle       -0.0070681  0.0513972  -0.138 0.890682    
    ## cls_creditsone credit  0.5087096  0.1169200   4.351 1.68e-05 ***
    ## bty_avg                0.0609051  0.0166769   3.652 0.000291 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5038 on 450 degrees of freedom
    ## Multiple R-squared:  0.1641, Adjusted R-squared:  0.1418 
    ## F-statistic: 7.359 on 12 and 450 DF,  p-value: 2.119e-12

``` r
mfinal <- lm(score~rank + ethnicity + gender + age + cls_perc_eval + cls_credits + bty_avg, data = evals)

summary(mfinal)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + age + cls_perc_eval + 
    ##     cls_credits + bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84113 -0.31628  0.07816  0.35598  1.07330 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.597655   0.234496  15.342  < 2e-16 ***
    ## ranktenure track      -0.124279   0.079805  -1.557 0.120100    
    ## ranktenured           -0.033075   0.064058  -0.516 0.605875    
    ## ethnicitynot minority  0.219760   0.072453   3.033 0.002559 ** 
    ## gendermale             0.179925   0.050919   3.534 0.000452 ***
    ## age                   -0.007378   0.003033  -2.432 0.015388 *  
    ## cls_perc_eval          0.005008   0.001441   3.475 0.000560 ***
    ## cls_creditsone credit  0.512099   0.111203   4.605 5.36e-06 ***
    ## bty_avg                0.064487   0.016397   3.933 9.70e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5035 on 454 degrees of freedom
    ## Multiple R-squared:  0.1577, Adjusted R-squared:  0.1429 
    ## F-statistic: 10.63 on 8 and 454 DF,  p-value: 1.005e-13

I thought cls_credits (i.e., number of credits) wouldn’t matter. It
turns out I was wrong. I decided not to include cls_students. The final
model’s adjusted R2 is .1429, compared to the model that I included all
potential predictors (.1403), it is higher.

Slope bty_avg means 1 unit increase in attractiveness rating is
associated with a .064 increase in evaluation score, when controlling
for rank, ethnicity, gender, age, course credit, and percent of students
wbo completed evaluation.  
Slope ethnicity means professors who are not ethnic minority are rated
.22 higher in course evals than ethnic minority professors, when
controlling for rank, attractiveness, gender, age, course credit, and
percent of students wbo completed evaluation.

A high evaluation score is associated with ethnicity, gender, age, and
attractiveness.

I won’t generalize because I imagine the student demographics at UT
Austin can be unique. E.g., if there are more White students there,
their ratings may show such biases toward ethnic minority faculty, but
this may not be the case for universities with a more diverse student
body.
