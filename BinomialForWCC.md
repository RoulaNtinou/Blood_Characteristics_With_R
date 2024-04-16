Proportion of white blood cells with Binomial Log Regression
================

- ***Fit a Binomial logistic regression model for the proportion of
  white blood cells***

**We will use the stepwise method both directions with AIC
criterion(excluding the intercept) to select the best subset of
covariates.**

**We will use the sum of white and red cells as the total number of
trials**

``` r
D$RCC<-round(D$RCC)
D$WCC<-round(D$WCC)
Total<-D$RCC+D$WCC
```

- **Model Selection**

``` r
m0 <-glm( (WCC/Total)~.-1-RCC, weights=Total, data=D, family=binomial )
m1 <-step(m0,direction='both')
```

    ## Start:  AIC=655.9
    ## (WCC/Total) ~ (Sex + Sport + RCC + Hc + Hg + Ferr + BMI + SSF + 
    ##     X.Bfat + LBM + Ht + Wt) - 1 - RCC
    ## 
    ##          Df Deviance    AIC
    ## - Sport   9   35.871 642.28
    ## - Sex     2   31.654 652.07
    ## - LBM     1   31.486 653.90
    ## - Hg      1   31.488 653.90
    ## - Wt      1   31.491 653.90
    ## - Ht      1   31.494 653.91
    ## - Hc      1   31.500 653.91
    ## - X.Bfat  1   31.504 653.92
    ## - BMI     1   31.521 653.93
    ## - SSF     1   31.543 653.96
    ## - Ferr    1   31.693 654.11
    ## <none>        31.486 655.90
    ## 
    ## Step:  AIC=642.28
    ## (WCC/Total) ~ Sex + Hc + Hg + Ferr + BMI + SSF + X.Bfat + LBM + 
    ##     Ht + Wt - 1
    ## 
    ##          Df Deviance    AIC
    ## - Sex     2   35.873 638.29
    ## - Hc      1   35.871 640.28
    ## - Ht      1   35.872 640.28
    ## - SSF     1   35.872 640.29
    ## - BMI     1   35.875 640.29
    ## - Wt      1   35.886 640.30
    ## - LBM     1   35.886 640.30
    ## - Hg      1   35.921 640.33
    ## - X.Bfat  1   35.986 640.40
    ## - Ferr    1   36.211 640.62
    ## <none>        35.871 642.28
    ## + Sport   9   31.486 655.90
    ## 
    ## Step:  AIC=638.29
    ## (WCC/Total) ~ Hc + Hg + Ferr + BMI + SSF + X.Bfat + LBM + Ht + 
    ##     Wt - 1
    ## 
    ##          Df Deviance    AIC
    ## - Hc      1   35.873 636.29
    ## - SSF     1   35.874 636.29
    ## - LBM     1   35.888 636.30
    ## - Wt      1   35.904 636.32
    ## - Hg      1   35.923 636.34
    ## - X.Bfat  1   36.006 636.42
    ## - Ht      1   36.008 636.42
    ## - BMI     1   36.011 636.42
    ## - Ferr    1   36.258 636.67
    ## <none>        35.873 638.29
    ## + Sex     2   35.871 642.28
    ## + Sport   9   31.654 652.07
    ## 
    ## Step:  AIC=636.29
    ## (WCC/Total) ~ Hg + Ferr + BMI + SSF + X.Bfat + LBM + Ht + Wt - 
    ##     1
    ## 
    ##          Df Deviance    AIC
    ## - SSF     1   35.874 634.29
    ## - LBM     1   35.889 634.30
    ## - Wt      1   35.904 634.32
    ## - X.Bfat  1   36.007 634.42
    ## - BMI     1   36.011 634.42
    ## - Ht      1   36.020 634.43
    ## - Hg      1   36.134 634.55
    ## - Ferr    1   36.260 634.67
    ## <none>        35.873 636.29
    ## + Hc      1   35.873 638.29
    ## + Sex     2   35.871 640.28
    ## + Sport   9   31.669 650.08
    ## 
    ## Step:  AIC=634.29
    ## (WCC/Total) ~ Hg + Ferr + BMI + X.Bfat + LBM + Ht + Wt - 1
    ## 
    ##          Df Deviance    AIC
    ## - LBM     1   35.894 632.31
    ## - Wt      1   35.914 632.33
    ## - BMI     1   36.013 632.43
    ## - X.Bfat  1   36.014 632.43
    ## - Ht      1   36.021 632.43
    ## - Hg      1   36.135 632.55
    ## - Ferr    1   36.260 632.67
    ## <none>        35.874 634.29
    ## + SSF     1   35.873 636.29
    ## + Hc      1   35.874 636.29
    ## + Sex     2   35.873 638.29
    ## + Sport   9   31.815 648.23
    ## 
    ## Step:  AIC=632.31
    ## (WCC/Total) ~ Hg + Ferr + BMI + X.Bfat + Ht + Wt - 1
    ## 
    ##          Df Deviance    AIC
    ## - BMI     1   36.081 630.49
    ## - Wt      1   36.148 630.56
    ## - Hg      1   36.168 630.58
    ## - Ferr    1   36.276 630.69
    ## - Ht      1   36.496 630.91
    ## - X.Bfat  1   37.487 631.90
    ## <none>        35.894 632.31
    ## + LBM     1   35.874 634.29
    ## + SSF     1   35.889 634.30
    ## + Hc      1   35.894 634.31
    ## + Sex     2   35.891 636.30
    ## + Sport   9   31.825 646.24
    ## 
    ## Step:  AIC=630.49
    ## (WCC/Total) ~ Hg + Ferr + X.Bfat + Ht + Wt - 1
    ## 
    ##          Df Deviance    AIC
    ## - Wt      1   36.151 628.56
    ## - Hg      1   36.187 628.60
    ## - Ht      1   36.555 628.97
    ## - Ferr    1   36.556 628.97
    ## <none>        36.081 630.49
    ## + BMI     1   35.894 632.31
    ## + LBM     1   36.013 632.43
    ## + SSF     1   36.071 632.48
    ## + Hc      1   36.081 632.49
    ## - X.Bfat  1   40.438 632.85
    ## + Sex     2   35.895 634.31
    ## + Sport   9   32.334 644.75
    ## 
    ## Step:  AIC=628.56
    ## (WCC/Total) ~ Hg + Ferr + X.Bfat + Ht - 1
    ## 
    ##          Df Deviance    AIC
    ## - Hg      1   36.265 626.68
    ## - Ht      1   36.555 626.97
    ## - Ferr    1   36.566 626.98
    ## <none>        36.151 628.56
    ## + Wt      1   36.081 630.49
    ## + LBM     1   36.091 630.50
    ## + SSF     1   36.098 630.51
    ## + Hc      1   36.144 630.56
    ## + BMI     1   36.148 630.56
    ## - X.Bfat  1   40.458 630.87
    ## + Sex     2   35.917 632.33
    ## + Sport   9   32.683 643.10
    ## 
    ## Step:  AIC=626.68
    ## (WCC/Total) ~ Ferr + X.Bfat + Ht - 1
    ## 
    ##          Df Deviance    AIC
    ## - Ferr    1   36.606 625.02
    ## - Ht      1   38.014 626.43
    ## <none>        36.265 626.68
    ## + Hg      1   36.151 628.56
    ## + Hc      1   36.183 628.60
    ## + Wt      1   36.187 628.60
    ## + LBM     1   36.196 628.61
    ## + SSF     1   36.213 628.63
    ## + BMI     1   36.254 628.67
    ## - X.Bfat  1   41.794 630.21
    ## + Sex     2   36.126 630.54
    ## + Sport   9   32.692 641.10
    ## 
    ## Step:  AIC=625.02
    ## (WCC/Total) ~ X.Bfat + Ht - 1
    ## 
    ##          Df Deviance    AIC
    ## <none>        36.606 625.02
    ## + Ferr    1   36.265 626.68
    ## + Hg      1   36.566 626.98
    ## + Hc      1   36.570 626.98
    ## + Wt      1   36.592 627.00
    ## + BMI     1   36.596 627.01
    ## + LBM     1   36.596 627.01
    ## + SSF     1   36.598 627.01
    ## - Ht      1   41.533 627.95
    ## - X.Bfat  1   41.848 628.26
    ## + Sex     2   36.510 628.92
    ## + Sport   9   32.784 639.20

``` r
summary(m1)
```

    ## 
    ## Call:
    ## glm(formula = (WCC/Total) ~ X.Bfat + Ht - 1, family = binomial, 
    ##     data = D, weights = Total)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.05357  -0.28177  -0.03498   0.22953   1.34558  
    ## 
    ## Coefficients:
    ##         Estimate Std. Error z value Pr(>|z|)  
    ## X.Bfat 0.0152387  0.0066833   2.280   0.0226 *
    ## Ht     0.0012057  0.0005434   2.219   0.0265 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 144.905  on 202  degrees of freedom
    ## Residual deviance:  36.606  on 200  degrees of freedom
    ## AIC: 625.02
    ## 
    ## Number of Fisher Scoring iterations: 3

**The final model after excluding the constant and the RCC, using
stepwise both direction method with the AIC criterion, for the
proportion(percentage) of white blood cell is the following**:

**Log(p/(1-p))= 0.01X.Bfat + 0.001Ht**

**Y ~ B(p,Total)**

- **Compare model against the constant model**

``` r
mnull <-glm( (WCC/Total)~1, weights=Total, data=D, family=binomial )
anova(mnull,m1,test='Chisq')
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: (WCC/Total) ~ 1
    ## Model 2: (WCC/Total) ~ X.Bfat + Ht - 1
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
    ## 1       201     41.065                       
    ## 2       200     36.606  1   4.4587  0.03472 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

> The Deviance of the Model=36.606 \< 41.065= Deviance of the Constant
> Model. So the models are statistically different. So we do not reject
> the null hypothesis that the final model differs from the
> constant(p-value= 0.03472\> 0.01).

- **Predict the white cells of a female and Gym athlete**

``` r
x=mean(D$X.Bfat)
y=mean(D$Ht)
pa=exp(0.0152387*x +0.0012057*y)
pfita=pa/(1+pa)
pfita
```

    ## [1] 0.6041978

***The expected amount of white blood cell is going to be (0.6041978 \*
Total(white + red))***
