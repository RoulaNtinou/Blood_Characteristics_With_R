WeightHeightRegression
================

> Since the R-squared=0.60, that is not very high percentage of
> variability explained(only 60%).

> In addition β0=-126.19 that should be the expected value of Wt when
> Ht=0, which does not have direct interpretation, and this value(Ht=0)
> is not possible and outside the observed range.

> So we are going to remove the constant from the model.

- **Exclude the intercept from the model**

``` r
final<-lm(Wt~Ht-1,data=D)
summary(final)
```

    ## 
    ## Call:
    ## lm(formula = Wt ~ Ht - 1, data = D)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -24.515  -7.572  -1.502   6.119  44.019 
    ## 
    ## Coefficients:
    ##    Estimate Std. Error t value Pr(>|t|)    
    ## Ht  0.41850    0.00431   97.11   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.05 on 201 degrees of freedom
    ## Multiple R-squared:  0.9791, Adjusted R-squared:  0.979 
    ## F-statistic:  9430 on 1 and 201 DF,  p-value: < 2.2e-16

> **The full model is:** Wt = 0.42Ht + ε , ε ~ N(0,11.05²)

> **Interpretation of parameters** For β1=0.42.
>
> First of all the p_value for testing whether the parameter is zero, is
> less than 0.05, so there is a significant effect of Height on
> Weight.  
> So each extra cm of Height increases the expected value of Weight by
> 0.42kg.
>
> R-squared = 0.98, So 98% of the variability is explained only using
> the Height as covariate.  
> Also the predictions are going to be very good.
>
> Residual standard deviation: σ = 11.05.It measures the precision of
> the model predictions.  
> It means that the accuracy of the prediction is 11.05 Kg.  
> Fitted value ±11.05kg will include 66% of the cases. Fitted value
> ±22.1kg will include 95% of the cases.

- **Testing for normality, the original variables(Weight and Height)**

``` r
lillie.test(D$Wt)
```

    ## 
    ##  Lilliefors (Kolmogorov-Smirnov) normality test
    ## 
    ## data:  D$Wt
    ## D = 0.057308, p-value = 0.1086

``` r
shapiro.test(D$Wt)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  D$Wt
    ## W = 0.99299, p-value = 0.4514

``` r
lillie.test(D$Ht)
```

    ## 
    ##  Lilliefors (Kolmogorov-Smirnov) normality test
    ## 
    ## data:  D$Ht
    ## D = 0.045569, p-value = 0.3874

``` r
shapiro.test(D$Ht)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  D$Ht
    ## W = 0.9906, p-value = 0.2121

> Using the normality tests Lilliefors(K-S) and Shapiro-Wilk, we do not
> reject the null hypothesis of normality.

- **Monitoring correlation**

``` r
cor.test(D$Wt,D$Ht)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  D$Wt and D$Ht
    ## t = 17.68, df = 200, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.7205322 0.8295299
    ## sample estimates:
    ##       cor 
    ## 0.7809063

> So there is strong linear dependence between Weight and Height.

- **Summarizing the Linear Regression model for Wt and Ht**

``` r
a<-lm(Wt~Ht,data=D)
summary(a)
```

    ## 
    ## Call:
    ## lm(formula = Wt ~ Ht, data = D)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -16.372  -5.296  -1.197   4.378  38.030 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -126.18901   11.39656  -11.07   <2e-16 ***
    ## Ht             1.11712    0.06319   17.68   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.72 on 200 degrees of freedom
    ## Multiple R-squared:  0.6098, Adjusted R-squared:  0.6079 
    ## F-statistic: 312.6 on 1 and 200 DF,  p-value: < 2.2e-16
