Poisson log-linear model for response white blood cell count
================

***Fit a Poisson log-linear model for response white blood cell count
(WCC).***

> The Poisson distribution models the rate of occurrence of events,
> which aligns with the concept of WCC representing the rate of white
> blood cell occurrence within a given volume of blood.

**We will use the stepwise method both directions with AIC
criterion(excluding the intercept) to select the best subset of
covariates.**

- **Model Selection**

So the full model is as follows:

``` r
D$WCC<-round(D$WCC)
a<-glm(WCC~.,data=D, family=poisson( link=log))
summary(a)
```

    ## 
    ## Call:
    ## glm(formula = WCC ~ ., family = poisson(link = log), data = D)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.47433  -0.44125  -0.05201   0.34299   1.57771  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)   0.0708195  4.3275131   0.016    0.987
    ## Sexmale      -0.0591059  0.1482799  -0.399    0.690
    ## SportField    0.0625812  0.1549214   0.404    0.686
    ## SportGym     -0.0863994  0.2942741  -0.294    0.769
    ## SportNetball  0.1853015  0.1244059   1.489    0.136
    ## SportRow     -0.0295013  0.1080459  -0.273    0.785
    ## SportSwim    -0.1422184  0.1311113  -1.085    0.278
    ## SportT400m   -0.1065287  0.1387679  -0.768    0.443
    ## SportTennis  -0.0950113  0.1635737  -0.581    0.561
    ## SportTSprnt  -0.0563758  0.1644622  -0.343    0.732
    ## SportWPolo    0.2053296  0.1275326   1.610    0.107
    ## RCC          -0.0397049  0.1755573  -0.226    0.821
    ## Hc            0.0199885  0.0303591   0.658    0.510
    ## Hg            0.0139084  0.0703530   0.198    0.843
    ## Ferr          0.0003504  0.0006735   0.520    0.603
    ## BMI           0.0313390  0.0972490   0.322    0.747
    ## SSF          -0.0010025  0.0043946  -0.228    0.820
    ## X.Bfat       -0.0115733  0.0407503  -0.284    0.776
    ## LBM          -0.0222985  0.0477649  -0.467    0.641
    ## Ht            0.0068940  0.0239624   0.288    0.774
    ## Wt            0.0096477  0.0495536   0.195    0.846
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 93.927  on 201  degrees of freedom
    ## Residual deviance: 72.647  on 181  degrees of freedom
    ## AIC: 880.48
    ## 
    ## Number of Fisher Scoring iterations: 4

**Log(λ)= 0.19SportNetball +0.23SportWPolo + 0.02Hc +0.03BMI -0.01LBM
+0.01Ht** **So λ = exp{ 0.19SportNetball +0.23SportWPolo + 0.02Hc
+0.03BMI -0.01LBM +0.01Ht}**

- **Exclude the constant from the model**

``` r
X=model.matrix(a)
data=data.frame(D$WCC,X[,-1])
head(data)
```

    ##   D.WCC Sexmale SportField SportGym SportNetball SportRow SportSwim SportT400m
    ## 1     8       0          0        0            0        0         0          0
    ## 2     8       0          0        0            0        0         0          0
    ## 3     5       0          0        0            0        0         0          0
    ## 4     5       0          0        0            0        0         0          0
    ## 5     7       0          0        0            0        0         0          0
    ## 6     4       0          0        0            0        0         0          0
    ##   SportTennis SportTSprnt SportWPolo  RCC   Hc   Hg Ferr   BMI   SSF X.Bfat
    ## 1           0           0          0 3.96 37.5 12.3   60 20.56 109.1  19.75
    ## 2           0           0          0 4.41 38.2 12.7   68 20.67 102.8  21.30
    ## 3           0           0          0 4.14 36.4 11.6   21 21.86 104.6  19.88
    ## 4           0           0          0 4.11 37.3 12.6   69 21.88 126.4  23.66
    ## 5           0           0          0 4.45 41.5 14.0   29 18.96  80.3  17.64
    ## 6           0           0          0 4.10 37.4 12.5   42 21.04  75.2  15.58
    ##     LBM    Ht   Wt
    ## 1 63.32 195.9 78.9
    ## 2 58.55 189.7 74.4
    ## 3 55.36 177.8 69.1
    ## 4 57.18 185.0 74.9
    ## 5 53.20 184.6 64.6
    ## 6 53.77 174.0 63.7

- **Fit the model without the constant**

``` r
a1<-glm(data$D.WCC~.-1,data=data, family=poisson)
a2<-step(a1,direction="both")
```

    ## Start:  AIC=878.48
    ## data$D.WCC ~ (Sexmale + SportField + SportGym + SportNetball + 
    ##     SportRow + SportSwim + SportT400m + SportTennis + SportTSprnt + 
    ##     SportWPolo + RCC + Hc + Hg + Ferr + BMI + SSF + X.Bfat + 
    ##     LBM + Ht + Wt) - 1
    ## 
    ##                Df Deviance    AIC
    ## - Hg            1   72.686 876.52
    ## - RCC           1   72.698 876.53
    ## - SSF           1   72.700 876.53
    ## - Wt            1   72.701 876.54
    ## - SportRow      1   72.722 876.56
    ## - X.Bfat        1   72.727 876.56
    ## - SportGym      1   72.762 876.60
    ## - SportTSprnt   1   72.766 876.60
    ## - SportField    1   72.810 876.65
    ## - Sexmale       1   72.812 876.65
    ## - LBM           1   72.865 876.70
    ## - Ferr          1   72.919 876.75
    ## - SportTennis   1   72.988 876.82
    ## - Hc            1   73.081 876.92
    ## - SportT400m    1   73.236 877.07
    ## - SportSwim     1   73.830 877.67
    ## - BMI           1   74.147 877.98
    ## <none>              72.647 878.48
    ## - SportNetball  1   74.871 878.71
    ## - SportWPolo    1   75.236 879.07
    ## - Ht            1   76.123 879.96
    ## 
    ## Step:  AIC=876.52
    ## data$D.WCC ~ Sexmale + SportField + SportGym + SportNetball + 
    ##     SportRow + SportSwim + SportT400m + SportTennis + SportTSprnt + 
    ##     SportWPolo + RCC + Hc + Ferr + BMI + SSF + X.Bfat + LBM + 
    ##     Ht + Wt - 1
    ## 
    ##                Df Deviance    AIC
    ## - RCC           1   72.732 874.57
    ## - SSF           1   72.735 874.57
    ## - Wt            1   72.738 874.57
    ## - SportRow      1   72.759 874.59
    ## - X.Bfat        1   72.770 874.61
    ## - SportGym      1   72.792 874.63
    ## - SportTSprnt   1   72.811 874.65
    ## - Sexmale       1   72.832 874.67
    ## - SportField    1   72.850 874.69
    ## - LBM           1   72.904 874.74
    ## - Ferr          1   72.977 874.81
    ## - SportTennis   1   73.034 874.87
    ## - SportT400m    1   73.278 875.11
    ## - Hc            1   73.850 875.69
    ## - SportSwim     1   73.897 875.73
    ## - BMI           1   74.370 876.20
    ## <none>              72.686 876.52
    ## - SportNetball  1   74.874 876.71
    ## - SportWPolo    1   75.237 877.07
    ## - Ht            1   76.180 878.01
    ## + Hg            1   72.647 878.48
    ## 
    ## Step:  AIC=874.57
    ## data$D.WCC ~ Sexmale + SportField + SportGym + SportNetball + 
    ##     SportRow + SportSwim + SportT400m + SportTennis + SportTSprnt + 
    ##     SportWPolo + Hc + Ferr + BMI + SSF + X.Bfat + LBM + Ht + 
    ##     Wt - 1
    ## 
    ##                Df Deviance    AIC
    ## - Wt            1   72.786 872.62
    ## - SSF           1   72.792 872.63
    ## - SportRow      1   72.812 872.65
    ## - X.Bfat        1   72.814 872.65
    ## - SportGym      1   72.845 872.68
    ## - SportField    1   72.864 872.70
    ## - SportTSprnt   1   72.903 872.74
    ## - Sexmale       1   72.907 872.74
    ## - LBM           1   72.953 872.79
    ## - Ferr          1   73.051 872.89
    ## - SportTennis   1   73.163 873.00
    ## - SportT400m    1   73.309 873.14
    ## - SportSwim     1   73.946 873.78
    ## - BMI           1   74.525 874.36
    ## <none>              72.732 874.57
    ## - SportNetball  1   74.876 874.71
    ## - SportWPolo    1   75.267 875.10
    ## - Hc            1   75.743 875.58
    ## - Ht            1   76.191 876.03
    ## + RCC           1   72.686 876.52
    ## + Hg            1   72.698 876.53
    ## 
    ## Step:  AIC=872.62
    ## data$D.WCC ~ Sexmale + SportField + SportGym + SportNetball + 
    ##     SportRow + SportSwim + SportT400m + SportTennis + SportTSprnt + 
    ##     SportWPolo + Hc + Ferr + BMI + SSF + X.Bfat + LBM + Ht - 
    ##     1
    ## 
    ##                Df Deviance    AIC
    ## - X.Bfat        1   72.815 870.65
    ## - SSF           1   72.820 870.65
    ## - SportRow      1   72.867 870.70
    ## - SportGym      1   72.895 870.73
    ## - SportTSprnt   1   72.933 870.77
    ## - Sexmale       1   72.946 870.78
    ## - SportField    1   72.956 870.79
    ## - Ferr          1   73.104 870.94
    ## - SportTennis   1   73.219 871.05
    ## - SportT400m    1   73.313 871.15
    ## - SportSwim     1   73.988 871.82
    ## - BMI           1   74.536 872.37
    ## <none>              72.786 872.62
    ## - LBM           1   74.844 872.68
    ## - SportNetball  1   74.886 872.72
    ## - SportWPolo    1   75.431 873.27
    ## - Hc            1   75.744 873.58
    ## + Wt            1   72.732 874.57
    ## + RCC           1   72.738 874.57
    ## + Hg            1   72.753 874.59
    ## - Ht            1   77.217 875.05
    ## 
    ## Step:  AIC=870.65
    ## data$D.WCC ~ Sexmale + SportField + SportGym + SportNetball + 
    ##     SportRow + SportSwim + SportT400m + SportTennis + SportTSprnt + 
    ##     SportWPolo + Hc + Ferr + BMI + SSF + LBM + Ht - 1
    ## 
    ##                Df Deviance    AIC
    ## - SportRow      1   72.904 868.74
    ## - SportGym      1   72.907 868.74
    ## - Sexmale       1   72.946 868.78
    ## - SportTSprnt   1   72.949 868.78
    ## - SportField    1   72.995 868.83
    ## - Ferr          1   73.129 868.96
    ## - SportTennis   1   73.224 869.06
    ## - SportT400m    1   73.314 869.15
    ## - SSF           1   73.441 869.28
    ## - SportSwim     1   73.990 869.82
    ## - BMI           1   74.574 870.41
    ## <none>              72.815 870.65
    ## - LBM           1   74.972 870.81
    ## - SportNetball  1   75.013 870.85
    ## - SportWPolo    1   75.463 871.30
    ## - Hc            1   75.789 871.62
    ## + RCC           1   72.771 872.61
    ## + Hg            1   72.777 872.61
    ## + X.Bfat        1   72.786 872.62
    ## + Wt            1   72.814 872.65
    ## - Ht            1   77.483 873.32
    ## 
    ## Step:  AIC=868.74
    ## data$D.WCC ~ Sexmale + SportField + SportGym + SportNetball + 
    ##     SportSwim + SportT400m + SportTennis + SportTSprnt + SportWPolo + 
    ##     Hc + Ferr + BMI + SSF + LBM + Ht - 1
    ## 
    ##                Df Deviance    AIC
    ## - SportGym      1   72.948 866.78
    ## - SportTSprnt   1   72.962 866.80
    ## - Sexmale       1   73.017 866.85
    ## - Ferr          1   73.183 867.02
    ## - SportTennis   1   73.224 867.06
    ## - SportT400m    1   73.330 867.16
    ## - SSF           1   73.461 867.30
    ## - SportField    1   73.476 867.31
    ## - SportSwim     1   74.171 868.01
    ## - BMI           1   74.576 868.41
    ## <none>              72.904 868.74
    ## - LBM           1   74.981 868.82
    ## - Hc            1   75.789 869.62
    ## - SportNetball  1   76.534 870.37
    ## + SportRow      1   72.815 870.65
    ## + RCC           1   72.853 870.69
    ## + X.Bfat        1   72.867 870.70
    ## + Hg            1   72.868 870.70
    ## + Wt            1   72.903 870.74
    ## - SportWPolo    1   77.473 871.31
    ## - Ht            1   77.740 871.57
    ## 
    ## Step:  AIC=866.78
    ## data$D.WCC ~ Sexmale + SportField + SportNetball + SportSwim + 
    ##     SportT400m + SportTennis + SportTSprnt + SportWPolo + Hc + 
    ##     Ferr + BMI + SSF + LBM + Ht - 1
    ## 
    ##                Df Deviance    AIC
    ## - SportTSprnt   1   72.985 864.82
    ## - Sexmale       1   73.075 864.91
    ## - Ferr          1   73.228 865.06
    ## - SportTennis   1   73.233 865.07
    ## - SportT400m    1   73.330 865.16
    ## - SSF           1   73.461 865.30
    ## - SportField    1   73.569 865.40
    ## - SportSwim     1   74.171 866.01
    ## - BMI           1   74.592 866.43
    ## <none>              72.948 866.78
    ## - LBM           1   75.066 866.90
    ## - Hc            1   75.810 867.64
    ## - SportNetball  1   76.733 868.57
    ## + RCC           1   72.894 868.73
    ## + SportGym      1   72.904 868.74
    ## + SportRow      1   72.907 868.74
    ## + Hg            1   72.920 868.75
    ## + X.Bfat        1   72.930 868.76
    ## + Wt            1   72.947 868.78
    ## - SportWPolo    1   77.630 869.47
    ## - Ht            1   77.740 869.57
    ## 
    ## Step:  AIC=864.82
    ## data$D.WCC ~ Sexmale + SportField + SportNetball + SportSwim + 
    ##     SportT400m + SportTennis + SportWPolo + Hc + Ferr + BMI + 
    ##     SSF + LBM + Ht - 1
    ## 
    ##                Df Deviance    AIC
    ## - Sexmale       1   73.118 862.95
    ## - SportTennis   1   73.238 863.07
    ## - Ferr          1   73.256 863.09
    ## - SportT400m    1   73.333 863.17
    ## - SSF           1   73.461 863.30
    ## - SportField    1   73.771 863.61
    ## - SportSwim     1   74.201 864.04
    ## - BMI           1   74.601 864.44
    ## <none>              72.985 864.82
    ## - LBM           1   75.071 864.91
    ## - Hc            1   75.832 865.67
    ## - SportNetball  1   76.793 866.63
    ## + RCC           1   72.911 866.75
    ## + SportTSprnt   1   72.948 866.78
    ## + Hg            1   72.953 866.79
    ## + SportGym      1   72.962 866.80
    ## + X.Bfat        1   72.973 866.81
    ## + SportRow      1   72.978 866.81
    ## + Wt            1   72.984 866.82
    ## - Ht            1   78.034 867.87
    ## - SportWPolo    1   78.239 868.07
    ## 
    ## Step:  AIC=862.95
    ## data$D.WCC ~ SportField + SportNetball + SportSwim + SportT400m + 
    ##     SportTennis + SportWPolo + Hc + Ferr + BMI + SSF + LBM + 
    ##     Ht - 1
    ## 
    ##                Df Deviance    AIC
    ## - Ferr          1   73.296 861.13
    ## - SportTennis   1   73.383 861.22
    ## - SSF           1   73.553 861.39
    ## - SportT400m    1   73.623 861.46
    ## - SportField    1   73.968 861.80
    ## - SportSwim     1   74.330 862.16
    ## <none>              73.118 862.95
    ## - BMI           1   75.403 863.24
    ## - Hc            1   75.930 863.77
    ## - SportNetball  1   76.801 864.64
    ## + Sexmale       1   72.985 864.82
    ## + RCC           1   73.004 864.84
    ## + SportTSprnt   1   73.075 864.91
    ## + SportGym      1   73.086 864.92
    ## + Wt            1   73.096 864.93
    ## + Hg            1   73.109 864.94
    ## + X.Bfat        1   73.114 864.95
    ## + SportRow      1   73.117 864.95
    ## - SportWPolo    1   78.239 866.07
    ## - LBM           1   79.337 867.17
    ## - Ht            1   81.066 868.90
    ## 
    ## Step:  AIC=861.13
    ## data$D.WCC ~ SportField + SportNetball + SportSwim + SportT400m + 
    ##     SportTennis + SportWPolo + Hc + BMI + SSF + LBM + Ht - 1
    ## 
    ##                Df Deviance    AIC
    ## - SportTennis   1   73.495 859.33
    ## - SSF           1   73.816 859.65
    ## - SportT400m    1   73.836 859.67
    ## - SportField    1   74.168 860.00
    ## - SportSwim     1   74.460 860.29
    ## <none>              73.296 861.13
    ## - BMI           1   75.874 861.71
    ## - Hc            1   76.179 862.01
    ## + Ferr          1   73.118 862.95
    ## - SportNetball  1   77.174 863.01
    ## + RCC           1   73.176 863.01
    ## + Sexmale       1   73.256 863.09
    ## + SportTSprnt   1   73.264 863.10
    ## + SportGym      1   73.266 863.10
    ## + Hg            1   73.270 863.11
    ## + Wt            1   73.284 863.12
    ## + SportRow      1   73.296 863.13
    ## + X.Bfat        1   73.296 863.13
    ## - SportWPolo    1   79.103 864.94
    ## - LBM           1   79.471 865.31
    ## - Ht            1   81.087 866.92
    ## 
    ## Step:  AIC=859.33
    ## data$D.WCC ~ SportField + SportNetball + SportSwim + SportT400m + 
    ##     SportWPolo + Hc + BMI + SSF + LBM + Ht - 1
    ## 
    ##                Df Deviance    AIC
    ## - SportT400m    1   73.922 857.76
    ## - SSF           1   73.974 857.81
    ## - SportField    1   74.440 858.27
    ## - SportSwim     1   74.541 858.38
    ## <none>              73.495 859.33
    ## - BMI           1   76.020 859.85
    ## - Hc            1   76.318 860.15
    ## + SportTennis   1   73.296 861.13
    ## + RCC           1   73.315 861.15
    ## + Ferr          1   73.383 861.22
    ## + Sexmale       1   73.436 861.27
    ## + Wt            1   73.461 861.30
    ## + Hg            1   73.471 861.31
    ## + SportGym      1   73.483 861.32
    ## + SportRow      1   73.484 861.32
    ## + X.Bfat        1   73.488 861.32
    ## + SportTSprnt   1   73.489 861.32
    ## - SportNetball  1   77.714 861.55
    ## - LBM           1   79.473 863.31
    ## - SportWPolo    1   79.574 863.41
    ## - Ht            1   81.138 864.97
    ## 
    ## Step:  AIC=857.76
    ## data$D.WCC ~ SportField + SportNetball + SportSwim + SportWPolo + 
    ##     Hc + BMI + SSF + LBM + Ht - 1
    ## 
    ##                Df Deviance    AIC
    ## - SSF           1   74.199 856.03
    ## - SportSwim     1   74.730 856.56
    ## - SportField    1   74.987 856.82
    ## <none>              73.922 857.76
    ## - BMI           1   76.234 858.07
    ## - Hc            1   76.677 858.51
    ## + SportT400m    1   73.495 859.33
    ## + Ferr          1   73.766 859.60
    ## + Sexmale       1   73.779 859.61
    ## + SportTennis   1   73.836 859.67
    ## + SportRow      1   73.840 859.68
    ## + RCC           1   73.846 859.68
    ## + X.Bfat        1   73.866 859.70
    ## + Wt            1   73.881 859.72
    ## + SportTSprnt   1   73.894 859.73
    ## + Hg            1   73.903 859.74
    ## + SportGym      1   73.922 859.76
    ## - SportNetball  1   78.334 860.17
    ## - LBM           1   79.474 861.31
    ## - SportWPolo    1   80.449 862.28
    ## - Ht            1   81.212 863.05
    ## 
    ## Step:  AIC=856.03
    ## data$D.WCC ~ SportField + SportNetball + SportSwim + SportWPolo + 
    ##     Hc + BMI + LBM + Ht - 1
    ## 
    ##                Df Deviance    AIC
    ## - SportSwim     1   74.928 854.76
    ## - SportField    1   75.299 855.13
    ## <none>              74.199 856.03
    ## - BMI           1   77.052 856.89
    ## + SSF           1   73.922 857.76
    ## + SportT400m    1   73.974 857.81
    ## + Ferr          1   73.993 857.83
    ## + Wt            1   74.034 857.87
    ## + X.Bfat        1   74.050 857.89
    ## + RCC           1   74.095 857.93
    ## + SportTennis   1   74.114 857.95
    ## + Sexmale       1   74.129 857.96
    ## + SportTSprnt   1   74.131 857.97
    ## + Hg            1   74.158 857.99
    ## + SportRow      1   74.162 858.00
    ## + SportGym      1   74.189 858.02
    ## - SportNetball  1   78.339 858.17
    ## - Hc            1   79.819 859.65
    ## - SportWPolo    1   80.450 860.28
    ## - LBM           1   81.405 861.24
    ## - Ht            1   81.710 861.54
    ## 
    ## Step:  AIC=854.76
    ## data$D.WCC ~ SportField + SportNetball + SportWPolo + Hc + BMI + 
    ##     LBM + Ht - 1
    ## 
    ##                Df Deviance    AIC
    ## - SportField    1   76.418 854.25
    ## <none>              74.928 854.76
    ## - BMI           1   77.684 855.52
    ## + SportSwim     1   74.199 856.03
    ## + SSF           1   74.730 856.56
    ## + SportRow      1   74.747 856.58
    ## + SportTSprnt   1   74.762 856.60
    ## + Ferr          1   74.769 856.60
    ## + SportT400m    1   74.837 856.67
    ## + Wt            1   74.848 856.68
    ## + X.Bfat        1   74.853 856.69
    ## + Hg            1   74.866 856.70
    ## + Sexmale       1   74.870 856.70
    ## + RCC           1   74.871 856.71
    ## + SportTennis   1   74.887 856.72
    ## + SportGym      1   74.910 856.75
    ## - SportNetball  1   79.516 857.35
    ## - Hc            1   80.472 858.31
    ## - SportWPolo    1   82.179 860.01
    ## - LBM           1   82.557 860.39
    ## - Ht            1   82.639 860.47
    ## 
    ## Step:  AIC=854.25
    ## data$D.WCC ~ SportNetball + SportWPolo + Hc + BMI + LBM + Ht - 
    ##     1
    ## 
    ##                Df Deviance    AIC
    ## <none>              76.418 854.25
    ## + SportField    1   74.928 854.76
    ## + SportSwim     1   75.299 855.13
    ## + SSF           1   76.204 856.04
    ## + Ferr          1   76.242 856.08
    ## + SportT400m    1   76.295 856.13
    ## + Sexmale       1   76.298 856.13
    ## + Hg            1   76.338 856.17
    ## + X.Bfat        1   76.342 856.18
    ## + SportTennis   1   76.353 856.19
    ## + Wt            1   76.362 856.20
    ## + SportTSprnt   1   76.392 856.23
    ## + SportGym      1   76.402 856.24
    ## + SportRow      1   76.414 856.25
    ## + RCC           1   76.418 856.25
    ## - SportNetball  1   80.469 856.30
    ## - Hc            1   82.017 857.85
    ## - BMI           1   82.388 858.22
    ## - SportWPolo    1   82.656 858.49
    ## - Ht            1   82.707 858.54
    ## - LBM           1   83.553 859.39

``` r
summary(a2)
```

    ## 
    ## Call:
    ## glm(formula = data$D.WCC ~ SportNetball + SportWPolo + Hc + BMI + 
    ##     LBM + Ht - 1, family = poisson, data = data)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.44000  -0.45326  -0.03746   0.40398   1.68700  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)   
    ## SportNetball  0.192024   0.094421   2.034  0.04198 * 
    ## SportWPolo    0.229419   0.089607   2.560  0.01046 * 
    ## Hc            0.019760   0.008288   2.384  0.01712 * 
    ## BMI           0.029626   0.012024   2.464  0.01374 * 
    ## LBM          -0.008958   0.003359  -2.667  0.00766 **
    ## Ht            0.005349   0.002144   2.495  0.01258 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 3258.921  on 202  degrees of freedom
    ## Residual deviance:   76.418  on 196  degrees of freedom
    ## AIC: 854.25
    ## 
    ## Number of Fisher Scoring iterations: 4

So the fitted model is as follows:

**Log(λ)= 0.19SportNetball +0.23SportWPolo + 0.02Hc +0.03BMI -0.01LBM
+0.01Ht** **So λ = exp{ 0.19SportNetball +0.23SportWPolo + 0.02Hc
+0.03BMI -0.01LBM +0.01Ht}**

- **Checking for overdispersion**

``` r
dispersiontest(a2, alternative="greater")
```

    ## 
    ##  Overdispersion test
    ## 
    ## data:  a2
    ## z = -17.001, p-value = 1
    ## alternative hypothesis: true dispersion is greater than 1
    ## sample estimates:
    ## dispersion 
    ##  0.3810914

> So we do not reject the null hypothesis(p-value=0.3810914\>0.05) that
> the model is not overdispersed.

> **The Poisson model fits well the data.**

- **Parameter Interpretation**

> exp(0.19) = 1.21, The WCC will be increased by 21% if the sport is
> Netball compared to BBall(sport), if the other covariates are
> constant.

> exp(0.23) = 1.26, The WCC will be increased by 26% if the sport is
> WPolo compared to BBall(sport), if the other covariates are constant.

> exp(0.02) = 1.02, The WCC will be increased by 2% if the Hc increases
> by 1% and Wt is constant, for athletes of BBall(sport).

> exp(0.03) = 1.03, The WCC will be increased by 3% if the BMI increases
> by one unit, for athletes of BBall(sport).

> exp(-0.01) = 0.99, The WCC will be decreased by 1% if the LBM
> increases by 1kg, for athletes of BBall(sport).

> exp(0.01) = 1.01, The WCC will be decreased by 1% if the Ht increases
> by 1cm and Wt is constant, for athletes of BBall(sport).

- **Compare Models**

``` r
mnull<-glm(data$D.WCC~1,data=data, family=poisson)
anova(a2,mnull,test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: data$D.WCC ~ SportNetball + SportWPolo + Hc + BMI + LBM + Ht - 
    ##     1
    ## Model 2: data$D.WCC ~ 1
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
    ## 1       196     76.418                        
    ## 2       201     93.927 -5  -17.509 0.003629 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

> The final model differs from constant model(p-value=0.003629\< 0.05)

- **Predict the white cells of a female and Gym athlete**

``` r
xnew<-data.frame(Sex=c('female'),
                 SportNetball=c(0),
                 SportWPolo=c(0),
                 Hc=c(mean(D$Wt)),
                 BMI=c(mean(D$BMI)),
                 LBM=c(mean(D$LBM)),
                 Ht=c(mean(D$Ht)))

pro<-exp(predict(a2,newdata=xnew))
pro
```

    ##        1 
    ## 12.73754

``` r
pro<-exp(predict(mnull,newdata=xnew))
pro
```

    ##        1 
    ## 7.108911

> Prediction with final model : 12.73754

> Prediction with constant model : 7.108911
