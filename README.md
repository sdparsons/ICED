ICED
================

``` r
library("lavaan")
```

    ## This is lavaan 0.6-8
    ## lavaan is FREE software! Please report any bugs.

``` r
source("ICED_syntax.R")
source("ICED_run.R")
source("ICED_boot.R")
source("str2cov.R")
source("sim_ICED.R")
```

<img src="inst/hexlogo/ICED_hexlogo.png" height="300"/>

## ICED: IntraClass Effect Decomposition

This script is intended to highlight progress and functionality with the
ICED package

## generate syntax

The `ICED_syntax()` function takes a dataframe and generates the lavaan
syntax to run the model (the first variable must be time). Here we save
it to the `syn` object. The model will also be printed to the console.
We are aiming to recreate the model from Brandmaier et al. (2018),
Figure 4

<figure>
<img src="elife-35718-fig4-v2.jpg" style="width:50.0%" alt="Brandmaier et al. (2018) Figure 4" /><figcaption aria-hidden="true">Brandmaier et al. (2018) Figure 4</figcaption>
</figure>

``` r
struc <- data.frame(time = c("T1", "T2", "T3", "T4"),
                    day = c("day1","day1","day2","day2"),
                    session = c("ses1", "ses1","ses2", "ses3"))

syn <- iced_syntax(struc)
```

    ## ! regressions
    ## T =~ 1*T1
    ## T =~ 1*T2
    ## T =~ 1*T3
    ## T =~ 1*T4
    ## day1 =~ 1*T1
    ## day1 =~ 1*T2
    ## day2 =~ 1*T3
    ## day2 =~ 1*T4
    ## ses1 =~ 1*T1
    ## ses1 =~ 1*T2
    ## ses2 =~ 1*T3
    ## ses3 =~ 1*T4
    ## E1 =~ 1*T1
    ## E2 =~ 1*T2
    ## E3 =~ 1*T3
    ## E4 =~ 1*T4
    ## ! residuals, variances and covariances
    ## T ~~ time*T
    ## day1 ~~ day*day1
    ## day2 ~~ day*day2
    ## ses1 ~~ session*ses1
    ## ses2 ~~ session*ses2
    ## ses3 ~~ session*ses3
    ## E1 ~~ e*E1
    ## E2 ~~ e*E2
    ## E3 ~~ e*E3
    ## E4 ~~ e*E4
    ## T ~~ 0*day1
    ## T ~~ 0*day2
    ## T ~~ 0*ses1
    ## T ~~ 0*ses2
    ## T ~~ 0*ses3
    ## T ~~ 0*E1
    ## T ~~ 0*E2
    ## T ~~ 0*E3
    ## T ~~ 0*E4
    ## day1 ~~ 0*day2
    ## day1 ~~ 0*ses1
    ## day1 ~~ 0*ses2
    ## day1 ~~ 0*ses3
    ## day1 ~~ 0*E1
    ## day1 ~~ 0*E2
    ## day1 ~~ 0*E3
    ## day1 ~~ 0*E4
    ## day2 ~~ 0*ses1
    ## day2 ~~ 0*ses2
    ## day2 ~~ 0*ses3
    ## day2 ~~ 0*E1
    ## day2 ~~ 0*E2
    ## day2 ~~ 0*E3
    ## day2 ~~ 0*E4
    ## ses1 ~~ 0*ses2
    ## ses1 ~~ 0*ses3
    ## ses1 ~~ 0*E1
    ## ses1 ~~ 0*E2
    ## ses1 ~~ 0*E3
    ## ses1 ~~ 0*E4
    ## ses2 ~~ 0*ses3
    ## ses2 ~~ 0*E1
    ## ses2 ~~ 0*E2
    ## ses2 ~~ 0*E3
    ## ses2 ~~ 0*E4
    ## ses3 ~~ 0*E1
    ## ses3 ~~ 0*E2
    ## ses3 ~~ 0*E3
    ## ses3 ~~ 0*E4
    ## E1 ~~ 0*E2
    ## E1 ~~ 0*E3
    ## E1 ~~ 0*E4
    ## E2 ~~ 0*E3
    ## E2 ~~ 0*E4
    ## E3 ~~ 0*E4
    ## ! observed means
    ## T1~1
    ## T2~1
    ## T3~1
    ## T4~1
    ## !set lower bounds of variances
    ## time > 0.0001 
    ## day > 0.0001 
    ## session > 0.0001 
    ## e > 0.0001

## simulate data

We’ll simulate data to run the ICED model on. The `sim_ICED` function
takes the model structure dataframe we used earlier and a list of
variances for each latent variable. The function returns several
outputs, including the simulated data.

``` r
sim1 <- sim_ICED(struc,
                 variances = list(time = 10,
                                  day = 2,
                                  session = 1,
                                  error = 3),
                 n = 2000)

head(sim1$data)
```

    ##           T1         T2         T3        T4
    ## 1  3.8884021  5.7111398  9.2510322  1.734300
    ## 2  0.4563695 -3.4225649  0.7174651  2.866610
    ## 3 -1.6606465 -2.0894755 -4.2143129 -9.473110
    ## 4  2.9509632  3.0319737  2.8749466  4.409724
    ## 5 -4.8511868 -0.6909584 -1.9342631 -2.677889
    ## 6 -0.4074239  1.1211309 -0.0586734  1.992257

we can also examine how well `sim_ICED` has recovered our variance
parameters by setting `check_recovery = TRUE`. lets simulate two
datasets, one large and another small.

``` r
sim2 <- sim_ICED(struc,
                 variances = list(time = 10,
                                  day = 2,
                                  session = 1,
                                  error = 3),
                 n = 2000,
                 check_recovery = TRUE)
```

    ## [1] "n =  2000 data simulated"
    ## [1] "data simulated based on ICC1 =  0.625"
    ##    time     day session   error 
    ##      10       2       1       3 
    ## [1] "model parameters recovered:"
    ## [1] "ICC1 = 0.62706128809452"
    ##    timeest     dayest sessionest       eest 
    ## 10.0994140  2.0549181  0.7616429  3.1899690

``` r
sim3 <- sim_ICED(struc,
                 variances = list(time = 10,
                                  day = 2,
                                  session = 1,
                                  error = 3),
                 n = 20,
                 check_recovery = TRUE)
```

    ## [1] "n =  20 data simulated"
    ## [1] "data simulated based on ICC1 =  0.625"
    ##    time     day session   error 
    ##      10       2       1       3 
    ## [1] "model parameters recovered:"
    ## [1] "ICC1 = 0.532973664276451"
    ##      timeest       dayest   sessionest         eest 
    ## 6.349666e+00 2.357447e+00 9.945911e-05 3.206445e+00

### str2cov

The `sim_ICED` function uses a helper function `str2cov`, which takes
the same structure data.frame and the list of variances we specified
earlier to generate the expected covariance of the model. This is then
passed to `mvrnorm` to generate the data. e.g.

``` r
str2cov(struc,
        list(time = 10,
             day = 2,
             session = 1,
             error = 3))
```

    ##    T1 T2 T3 T4
    ## T1 16 13 10 10
    ## T2 13 16 10 10
    ## T3 10 10 16 12
    ## T4 10 10 12 16

## run\_ICED

we can now run our model. The `run_ICED` function will print a bunch of
relevant outputs. Note that the `sim_ICED` function returns a list of
objects, so we need to specify the data part

``` r
res1 <- run_ICED(model = syn,
                 data = sim1$data)
```

    ## $ICC
    ## [1] 0.6380726
    ## 
    ## $ICC2
    ## [1] 0.8332127
    ## 
    ## $timeest
    ## [1] 10.54941
    ## 
    ## $dayest
    ## [1] 1.941511
    ## 
    ## $sessionest
    ## [1] 1.116496
    ## 
    ## $eest
    ## [1] 2.925827
    ## 
    ## $EffectiveError
    ## [1] 2.111715
    ## 
    ## $AbsoluteError
    ## [1] 2.074378
    ## 
    ## $phi_dependability
    ## [1] 0.8356771
    ## 
    ## $lavaan
    ## lavaan 0.6-8 ended normally after 198 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        14
    ##   Number of inequality constraints                   4
    ##                                                       
    ##   Number of observations                          2000
    ##   Number of missing patterns                         1
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                 6.667
    ##   Degrees of freedom                                 6
    ##   P-value (Chi-square)                           0.353
    ## 
    ## $est_cov
    ##    T1     T2     T3     T4    
    ## T1 16.533                     
    ## T2 13.607 16.533              
    ## T3 10.549 10.549 16.533       
    ## T4 10.549 10.549 12.491 16.533

we can also bootstrap our estimates. The output now includes 95% CIs on
the ICC and ICC2. Best to use more than 10 boots, but set to 10 for
speed here

``` r
run_ICED(model = syn,
         data = sim1$data,
         boot = 10)
```

    ## Warning in norm.inter(t, alpha): extreme order statistics used as endpoints

    ## Warning in norm.inter(t, alpha): extreme order statistics used as endpoints

    ## $ICC
    ## [1] 0.6380726
    ## 
    ## $ICC_CIs
    ## [1] 0.6369655 0.6613609
    ## 
    ## $ICC2
    ## [1] 0.8332127
    ## 
    ## $ICC2_CIs
    ## [1] 0.8229607 0.8483737
    ## 
    ## $timeest
    ## [1] 10.54941
    ## 
    ## $dayest
    ## [1] 1.941511
    ## 
    ## $sessionest
    ## [1] 1.116496
    ## 
    ## $eest
    ## [1] 2.925827
    ## 
    ## $EffectiveError
    ## [1] 2.111715
    ## 
    ## $AbsoluteError
    ## [1] 2.074378
    ## 
    ## $phi_dependability
    ## [1] 0.8356771
    ## 
    ## $lavaan
    ## lavaan 0.6-8 ended normally after 198 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        14
    ##   Number of inequality constraints                   4
    ##                                                       
    ##   Number of observations                          2000
    ##   Number of missing patterns                         1
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                 6.667
    ##   Degrees of freedom                                 6
    ##   P-value (Chi-square)                           0.353
    ## 
    ## $est_cov
    ##    T1     T2     T3     T4    
    ## T1 16.533                     
    ## T2 13.607 16.533              
    ## T3 10.549 10.549 16.533       
    ## T4 10.549 10.549 12.491 16.533

## model comparison

we can compare alternative models, for example constraining the variance
of one component to zero

``` r
syntax2 <- iced_syntax(struc,
                       set_variances = c(res1$timeest, 
                                         res1$dayest, 
                                         0,
                                         res1$eest))
```

    ## ! regressions
    ## T =~ 1*T1
    ## T =~ 1*T2
    ## T =~ 1*T3
    ## T =~ 1*T4
    ## day1 =~ 1*T1
    ## day1 =~ 1*T2
    ## day2 =~ 1*T3
    ## day2 =~ 1*T4
    ## ses1 =~ 1*T1
    ## ses1 =~ 1*T2
    ## ses2 =~ 1*T3
    ## ses3 =~ 1*T4
    ## E1 =~ 1*T1
    ## E2 =~ 1*T2
    ## E3 =~ 1*T3
    ## E4 =~ 1*T4
    ## ! residuals, variances and covariances
    ## T ~~ time*T
    ## day1 ~~ day*day1
    ## day2 ~~ day*day2
    ## ses1 ~~ session*ses1
    ## ses2 ~~ session*ses2
    ## ses3 ~~ session*ses3
    ## E1 ~~ e*E1
    ## E2 ~~ e*E2
    ## E3 ~~ e*E3
    ## E4 ~~ e*E4
    ## T ~~ 0*day1
    ## T ~~ 0*day2
    ## T ~~ 0*ses1
    ## T ~~ 0*ses2
    ## T ~~ 0*ses3
    ## T ~~ 0*E1
    ## T ~~ 0*E2
    ## T ~~ 0*E3
    ## T ~~ 0*E4
    ## day1 ~~ 0*day2
    ## day1 ~~ 0*ses1
    ## day1 ~~ 0*ses2
    ## day1 ~~ 0*ses3
    ## day1 ~~ 0*E1
    ## day1 ~~ 0*E2
    ## day1 ~~ 0*E3
    ## day1 ~~ 0*E4
    ## day2 ~~ 0*ses1
    ## day2 ~~ 0*ses2
    ## day2 ~~ 0*ses3
    ## day2 ~~ 0*E1
    ## day2 ~~ 0*E2
    ## day2 ~~ 0*E3
    ## day2 ~~ 0*E4
    ## ses1 ~~ 0*ses2
    ## ses1 ~~ 0*ses3
    ## ses1 ~~ 0*E1
    ## ses1 ~~ 0*E2
    ## ses1 ~~ 0*E3
    ## ses1 ~~ 0*E4
    ## ses2 ~~ 0*ses3
    ## ses2 ~~ 0*E1
    ## ses2 ~~ 0*E2
    ## ses2 ~~ 0*E3
    ## ses2 ~~ 0*E4
    ## ses3 ~~ 0*E1
    ## ses3 ~~ 0*E2
    ## ses3 ~~ 0*E3
    ## ses3 ~~ 0*E4
    ## E1 ~~ 0*E2
    ## E1 ~~ 0*E3
    ## E1 ~~ 0*E4
    ## E2 ~~ 0*E3
    ## E2 ~~ 0*E4
    ## E3 ~~ 0*E4
    ## ! observed means
    ## T1~1
    ## T2~1
    ## T3~1
    ## T4~1
    ## !set variances
    ##  
    ##  time == 10.5494120784226
    ##  day == 1.94151093101342
    ##  session == 0
    ##  e == 2.92582731892883

``` r
res2 <- run_ICED(syntax2,
                 sim1$data)
```

    ## $ICC
    ## [1] 0.6842825
    ## 
    ## $ICC2
    ## [1] 0.8610623
    ## 
    ## $timeest
    ## [1] 10.54941
    ## 
    ## $dayest
    ## [1] 1.941511
    ## 
    ## $sessionest
    ## [1] 0
    ## 
    ## $eest
    ## [1] 2.925827
    ## 
    ## $EffectiveError
    ## [1] 1.702212
    ## 
    ## $AbsoluteError
    ## [1] 1.702212
    ## 
    ## $phi_dependability
    ## [1] 0.8610623
    ## 
    ## $lavaan
    ## lavaan 0.6-8 ended normally after 1 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        14
    ##   Number of equality constraints                    10
    ##                                                       
    ##   Number of observations                          2000
    ##   Number of missing patterns                         1
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                               178.194
    ##   Degrees of freedom                                10
    ##   P-value (Chi-square)                           0.000
    ## 
    ## $est_cov
    ##    T1     T2     T3     T4    
    ## T1 15.417                     
    ## T2 12.491 15.417              
    ## T3 10.549 10.549 15.417       
    ## T4 10.549 10.549 12.491 15.417

``` r
anova(res1$lavaan,
      res2$lavaan)
```

    ## Chi-Squared Difference Test
    ## 
    ##             Df   AIC   BIC    Chisq Chisq diff Df diff Pr(>Chisq)    
    ## res1$lavaan  6 39786 39831   6.6671                                  
    ## res2$lavaan 10 39949 39972 178.1938     171.53       4  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# group comparisons

we can use `iced_syntax` to generate the syntax for multiple groups. The
user can specify a number of groups or a vector of strings. Here we’ll
also highlight that we can set the variances to be greater than zero
also.

``` r
group_syntax <- iced_syntax(struc,
                            groups = 2,
                            fix_lower_bounds = TRUE)
```

    ## ! regressions
    ## T =~ 1*T1
    ## T =~ 1*T2
    ## T =~ 1*T3
    ## T =~ 1*T4
    ## day1 =~ 1*T1
    ## day1 =~ 1*T2
    ## day2 =~ 1*T3
    ## day2 =~ 1*T4
    ## ses1 =~ 1*T1
    ## ses1 =~ 1*T2
    ## ses2 =~ 1*T3
    ## ses3 =~ 1*T4
    ## E1 =~ 1*T1
    ## E2 =~ 1*T2
    ## E3 =~ 1*T3
    ## E4 =~ 1*T4
    ## ! residuals, variances and covariances
    ## T ~~ c(lattime1,lattime2)*T
    ## day1 ~~ c(latday1,latday2)*day1
    ## day2 ~~ c(latday1,latday2)*day2
    ## ses1 ~~ c(latsession1,latsession2)*ses1
    ## ses2 ~~ c(latsession1,latsession2)*ses2
    ## ses3 ~~ c(latsession1,latsession2)*ses3
    ## E1 ~~ c(late1,late2)*E1
    ## E2 ~~ c(late1,late2)*E2
    ## E3 ~~ c(late1,late2)*E3
    ## E4 ~~ c(late1,late2)*E4
    ## T ~~ 0*day1
    ## T ~~ 0*day2
    ## T ~~ 0*ses1
    ## T ~~ 0*ses2
    ## T ~~ 0*ses3
    ## T ~~ 0*E1
    ## T ~~ 0*E2
    ## T ~~ 0*E3
    ## T ~~ 0*E4
    ## day1 ~~ 0*day2
    ## day1 ~~ 0*ses1
    ## day1 ~~ 0*ses2
    ## day1 ~~ 0*ses3
    ## day1 ~~ 0*E1
    ## day1 ~~ 0*E2
    ## day1 ~~ 0*E3
    ## day1 ~~ 0*E4
    ## day2 ~~ 0*ses1
    ## day2 ~~ 0*ses2
    ## day2 ~~ 0*ses3
    ## day2 ~~ 0*E1
    ## day2 ~~ 0*E2
    ## day2 ~~ 0*E3
    ## day2 ~~ 0*E4
    ## ses1 ~~ 0*ses2
    ## ses1 ~~ 0*ses3
    ## ses1 ~~ 0*E1
    ## ses1 ~~ 0*E2
    ## ses1 ~~ 0*E3
    ## ses1 ~~ 0*E4
    ## ses2 ~~ 0*ses3
    ## ses2 ~~ 0*E1
    ## ses2 ~~ 0*E2
    ## ses2 ~~ 0*E3
    ## ses2 ~~ 0*E4
    ## ses3 ~~ 0*E1
    ## ses3 ~~ 0*E2
    ## ses3 ~~ 0*E3
    ## ses3 ~~ 0*E4
    ## E1 ~~ 0*E2
    ## E1 ~~ 0*E3
    ## E1 ~~ 0*E4
    ## E2 ~~ 0*E3
    ## E2 ~~ 0*E4
    ## E3 ~~ 0*E4
    ## ! observed means
    ## T1~1
    ## T2~1
    ## T3~1
    ## T4~1
    ## !set lower bounds of variances
    ## lattime1 > 0.0001 
    ## lattime2 > 0.0001 
    ## latday1 > 0.0001 
    ## latday2 > 0.0001 
    ## latsession1 > 0.0001 
    ## latsession2 > 0.0001 
    ## late1 > 0.0001 
    ## late2 > 0.0001

``` r
group_syntax <- iced_syntax(struc,
                            groups = c("group1", "group2"),
                            fix_lower_bounds = TRUE)
```

    ## ! regressions
    ## T =~ 1*T1
    ## T =~ 1*T2
    ## T =~ 1*T3
    ## T =~ 1*T4
    ## day1 =~ 1*T1
    ## day1 =~ 1*T2
    ## day2 =~ 1*T3
    ## day2 =~ 1*T4
    ## ses1 =~ 1*T1
    ## ses1 =~ 1*T2
    ## ses2 =~ 1*T3
    ## ses3 =~ 1*T4
    ## E1 =~ 1*T1
    ## E2 =~ 1*T2
    ## E3 =~ 1*T3
    ## E4 =~ 1*T4
    ## ! residuals, variances and covariances
    ## T ~~ c(lattimegroup1,lattimegroup2)*T
    ## day1 ~~ c(latdaygroup1,latdaygroup2)*day1
    ## day2 ~~ c(latdaygroup1,latdaygroup2)*day2
    ## ses1 ~~ c(latsessiongroup1,latsessiongroup2)*ses1
    ## ses2 ~~ c(latsessiongroup1,latsessiongroup2)*ses2
    ## ses3 ~~ c(latsessiongroup1,latsessiongroup2)*ses3
    ## E1 ~~ c(lategroup1,lategroup2)*E1
    ## E2 ~~ c(lategroup1,lategroup2)*E2
    ## E3 ~~ c(lategroup1,lategroup2)*E3
    ## E4 ~~ c(lategroup1,lategroup2)*E4
    ## T ~~ 0*day1
    ## T ~~ 0*day2
    ## T ~~ 0*ses1
    ## T ~~ 0*ses2
    ## T ~~ 0*ses3
    ## T ~~ 0*E1
    ## T ~~ 0*E2
    ## T ~~ 0*E3
    ## T ~~ 0*E4
    ## day1 ~~ 0*day2
    ## day1 ~~ 0*ses1
    ## day1 ~~ 0*ses2
    ## day1 ~~ 0*ses3
    ## day1 ~~ 0*E1
    ## day1 ~~ 0*E2
    ## day1 ~~ 0*E3
    ## day1 ~~ 0*E4
    ## day2 ~~ 0*ses1
    ## day2 ~~ 0*ses2
    ## day2 ~~ 0*ses3
    ## day2 ~~ 0*E1
    ## day2 ~~ 0*E2
    ## day2 ~~ 0*E3
    ## day2 ~~ 0*E4
    ## ses1 ~~ 0*ses2
    ## ses1 ~~ 0*ses3
    ## ses1 ~~ 0*E1
    ## ses1 ~~ 0*E2
    ## ses1 ~~ 0*E3
    ## ses1 ~~ 0*E4
    ## ses2 ~~ 0*ses3
    ## ses2 ~~ 0*E1
    ## ses2 ~~ 0*E2
    ## ses2 ~~ 0*E3
    ## ses2 ~~ 0*E4
    ## ses3 ~~ 0*E1
    ## ses3 ~~ 0*E2
    ## ses3 ~~ 0*E3
    ## ses3 ~~ 0*E4
    ## E1 ~~ 0*E2
    ## E1 ~~ 0*E3
    ## E1 ~~ 0*E4
    ## E2 ~~ 0*E3
    ## E2 ~~ 0*E4
    ## E3 ~~ 0*E4
    ## ! observed means
    ## T1~1
    ## T2~1
    ## T3~1
    ## T4~1
    ## !set lower bounds of variances
    ## lattimegroup1 > 0.0001 
    ## lattimegroup2 > 0.0001 
    ## latdaygroup1 > 0.0001 
    ## latdaygroup2 > 0.0001 
    ## latsessiongroup1 > 0.0001 
    ## latsessiongroup2 > 0.0001 
    ## lategroup1 > 0.0001 
    ## lategroup2 > 0.0001

generating multiple group data takes a few more lines of code currently
- but I plan to adapt this to take vectors within the lists.

``` r
variances_hi <- list(time = 8,
                  day = .25,
                  session = .2,
                  error = .25)

variances_lo <- list(time = 2,
                     day = .25,
                     session = .2,
                     error = .25)

sim_hi <- sim_ICED(structure = struc,
                 variances = variances_hi,
                 n = 100)$data

sim_lo <- sim_ICED(structure = struc,
                 variances = variances_lo,
                 n = 100)$data

sim_hi$group <- "high"
sim_lo$group <- "low"

sim_all <- rbind(sim_hi, sim_lo)
```

we can check the ICC reliability we have specified fairly easily,
e.g. for the high group
`variances_hi$time / (sum(unlist(variances_hi)))`. For the high group
ICC = 0.9195402, and the low group ICC = 0.7407407.

Then we can compare the groups (not currenly within run\_ICED). Here, m0
is our base model constraining variances across groups, and m1 using the
model we just generated to allow them to vary across groups.

``` r
m1 <- lavaan::lavaan(model = group_syntax,
               data = sim_all,
               group = "group")

m0 <- lavaan::lavaan(model = syn,
                     data = sim_all,
                     group = "group")
```

    ## Warning in lavaanify(model = FLAT, constraints = constraints, varTable = lavdata@ov, : lavaan WARNING: using a single label per parameter in a multiple group
    ##   setting implies imposing equality constraints across all the groups;
    ##   If this is not intended, either remove the label(s), or use a vector
    ##   of labels (one for each group);
    ##   See the Multiple groups section in the man page of model.syntax.

``` r
summary(m1)
```

    ## lavaan 0.6-8 ended normally after 338 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        28
    ##   Number of inequality constraints                   8
    ##                                                       
    ##   Number of observations per group:                   
    ##     high                                           100
    ##     low                                            100
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                13.011
    ##   Degrees of freedom                                12
    ##   P-value (Chi-square)                           0.368
    ##   Test statistic for each group:
    ##     high                                         5.908
    ##     low                                          7.103
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## 
    ## Group 1 [high]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   T =~                                                
    ##     T1                1.000                           
    ##     T2                1.000                           
    ##     T3                1.000                           
    ##     T4                1.000                           
    ##   day1 =~                                             
    ##     T1                1.000                           
    ##     T2                1.000                           
    ##   day2 =~                                             
    ##     T3                1.000                           
    ##     T4                1.000                           
    ##   ses1 =~                                             
    ##     T1                1.000                           
    ##     T2                1.000                           
    ##   ses2 =~                                             
    ##     T3                1.000                           
    ##   ses3 =~                                             
    ##     T4                1.000                           
    ##   E1 =~                                               
    ##     T1                1.000                           
    ##   E2 =~                                               
    ##     T2                1.000                           
    ##   E3 =~                                               
    ##     T3                1.000                           
    ##   E4 =~                                               
    ##     T4                1.000                           
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   T ~~                                                
    ##     day1              0.000                           
    ##     day2              0.000                           
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   day1 ~~                                             
    ##     day2              0.000                           
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   day2 ~~                                             
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   ses1 ~~                                             
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   ses2 ~~                                             
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   ses3 ~~                                             
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   E1 ~~                                               
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   E2 ~~                                               
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   E3 ~~                                               
    ##     E4                0.000                           
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .T1               -0.447    0.246   -1.814    0.070
    ##    .T2               -0.477    0.246   -1.938    0.053
    ##    .T3               -0.432    0.246   -1.755    0.079
    ##    .T4               -0.381    0.246   -1.547    0.122
    ##     T                 0.000                           
    ##     day1              0.000                           
    ##     day2              0.000                           
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     T       (ltt1)    5.413    0.800    6.762    0.000
    ##     day1    (ltd1)    0.239    0.082    2.927    0.003
    ##     day2    (ltd1)    0.239    0.082    2.927    0.003
    ##     ses1    (lts1)    0.178    0.066    2.698    0.007
    ##     ses2    (lts1)    0.178    0.066    2.698    0.007
    ##     ses3    (lts1)    0.178    0.066    2.698    0.007
    ##     E1      (ltg1)    0.230    0.032    7.075    0.000
    ##     E2      (ltg1)    0.230    0.032    7.075    0.000
    ##     E3      (ltg1)    0.230    0.032    7.075    0.000
    ##     E4      (ltg1)    0.230    0.032    7.075    0.000
    ##    .T1                0.000                           
    ##    .T2                0.000                           
    ##    .T3                0.000                           
    ##    .T4                0.000                           
    ## 
    ## 
    ## Group 2 [low]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   T =~                                                
    ##     T1                1.000                           
    ##     T2                1.000                           
    ##     T3                1.000                           
    ##     T4                1.000                           
    ##   day1 =~                                             
    ##     T1                1.000                           
    ##     T2                1.000                           
    ##   day2 =~                                             
    ##     T3                1.000                           
    ##     T4                1.000                           
    ##   ses1 =~                                             
    ##     T1                1.000                           
    ##     T2                1.000                           
    ##   ses2 =~                                             
    ##     T3                1.000                           
    ##   ses3 =~                                             
    ##     T4                1.000                           
    ##   E1 =~                                               
    ##     T1                1.000                           
    ##   E2 =~                                               
    ##     T2                1.000                           
    ##   E3 =~                                               
    ##     T3                1.000                           
    ##   E4 =~                                               
    ##     T4                1.000                           
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   T ~~                                                
    ##     day1              0.000                           
    ##     day2              0.000                           
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   day1 ~~                                             
    ##     day2              0.000                           
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   day2 ~~                                             
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   ses1 ~~                                             
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   ses2 ~~                                             
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   ses3 ~~                                             
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   E1 ~~                                               
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   E2 ~~                                               
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   E3 ~~                                               
    ##     E4                0.000                           
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .T1                0.137    0.170    0.805    0.421
    ##    .T2                0.231    0.170    1.354    0.176
    ##    .T3               -0.085    0.170   -0.497    0.619
    ##    .T4                0.127    0.170    0.744    0.457
    ##     T                 0.000                           
    ##     day1              0.000                           
    ##     day2              0.000                           
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     T       (ltt2)    2.014    0.338    5.956    0.000
    ##     day1    (ltd2)    0.460    0.110    4.198    0.000
    ##     day2    (ltd2)    0.460    0.110    4.198    0.000
    ##     ses1    (lts2)    0.122    0.074    1.652    0.099
    ##     ses2    (lts2)    0.122    0.074    1.652    0.099
    ##     ses3    (lts2)    0.122    0.074    1.652    0.099
    ##     E1      (ltg2)    0.304    0.043    7.083    0.000
    ##     E2      (ltg2)    0.304    0.043    7.083    0.000
    ##     E3      (ltg2)    0.304    0.043    7.083    0.000
    ##     E4      (ltg2)    0.304    0.043    7.083    0.000
    ##    .T1                0.000                           
    ##    .T2                0.000                           
    ##    .T3                0.000                           
    ##    .T4                0.000                           
    ## 
    ## Constraints:
    ##                                                |Slack|
    ##     lattimegroup1 - (0.0001)                     5.413
    ##     lattimegroup2 - (0.0001)                     2.014
    ##     latdaygroup1 - (0.0001)                      0.239
    ##     latdaygroup2 - (0.0001)                      0.460
    ##     latsessiongroup1 - (0.0001)                  0.178
    ##     latsessiongroup2 - (0.0001)                  0.122
    ##     lategroup1 - (0.0001)                        0.230
    ##     lategroup2 - (0.0001)                        0.304

``` r
summary(m0)
```

    ## lavaan 0.6-8 ended normally after 340 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        28
    ##   Number of inequality constraints                   4
    ##                                                       
    ##   Number of observations per group:                   
    ##     high                                           100
    ##     low                                            100
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                36.846
    ##   Degrees of freedom                                16
    ##   P-value (Chi-square)                           0.002
    ##   Test statistic for each group:
    ##     high                                        15.575
    ##     low                                         21.271
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## 
    ## Group 1 [high]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   T =~                                                
    ##     T1                1.000                           
    ##     T2                1.000                           
    ##     T3                1.000                           
    ##     T4                1.000                           
    ##   day1 =~                                             
    ##     T1                1.000                           
    ##     T2                1.000                           
    ##   day2 =~                                             
    ##     T3                1.000                           
    ##     T4                1.000                           
    ##   ses1 =~                                             
    ##     T1                1.000                           
    ##     T2                1.000                           
    ##   ses2 =~                                             
    ##     T3                1.000                           
    ##   ses3 =~                                             
    ##     T4                1.000                           
    ##   E1 =~                                               
    ##     T1                1.000                           
    ##   E2 =~                                               
    ##     T2                1.000                           
    ##   E3 =~                                               
    ##     T3                1.000                           
    ##   E4 =~                                               
    ##     T4                1.000                           
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   T ~~                                                
    ##     day1              0.000                           
    ##     day2              0.000                           
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   day1 ~~                                             
    ##     day2              0.000                           
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   day2 ~~                                             
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   ses1 ~~                                             
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   ses2 ~~                                             
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   ses3 ~~                                             
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   E1 ~~                                               
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   E2 ~~                                               
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   E3 ~~                                               
    ##     E4                0.000                           
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .T1               -0.447    0.212   -2.110    0.035
    ##    .T2               -0.477    0.212   -2.255    0.024
    ##    .T3               -0.432    0.212   -2.042    0.041
    ##    .T4               -0.381    0.212   -1.800    0.072
    ##     T                 0.000                           
    ##     day1              0.000                           
    ##     day2              0.000                           
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     T       (time)    3.712    0.402    9.234    0.000
    ##     day1     (day)    0.350    0.067    5.194    0.000
    ##     day2     (day)    0.350    0.067    5.194    0.000
    ##     ses1    (sssn)    0.149    0.049    3.016    0.003
    ##     ses2    (sssn)    0.149    0.049    3.016    0.003
    ##     ses3    (sssn)    0.149    0.049    3.016    0.003
    ##     E1         (e)    0.267    0.027   10.009    0.000
    ##     E2         (e)    0.267    0.027   10.009    0.000
    ##     E3         (e)    0.267    0.027   10.009    0.000
    ##     E4         (e)    0.267    0.027   10.009    0.000
    ##    .T1                0.000                           
    ##    .T2                0.000                           
    ##    .T3                0.000                           
    ##    .T4                0.000                           
    ## 
    ## 
    ## Group 2 [low]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   T =~                                                
    ##     T1                1.000                           
    ##     T2                1.000                           
    ##     T3                1.000                           
    ##     T4                1.000                           
    ##   day1 =~                                             
    ##     T1                1.000                           
    ##     T2                1.000                           
    ##   day2 =~                                             
    ##     T3                1.000                           
    ##     T4                1.000                           
    ##   ses1 =~                                             
    ##     T1                1.000                           
    ##     T2                1.000                           
    ##   ses2 =~                                             
    ##     T3                1.000                           
    ##   ses3 =~                                             
    ##     T4                1.000                           
    ##   E1 =~                                               
    ##     T1                1.000                           
    ##   E2 =~                                               
    ##     T2                1.000                           
    ##   E3 =~                                               
    ##     T3                1.000                           
    ##   E4 =~                                               
    ##     T4                1.000                           
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   T ~~                                                
    ##     day1              0.000                           
    ##     day2              0.000                           
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   day1 ~~                                             
    ##     day2              0.000                           
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   day2 ~~                                             
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   ses1 ~~                                             
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   ses2 ~~                                             
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   ses3 ~~                                             
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   E1 ~~                                               
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   E2 ~~                                               
    ##     E3                0.000                           
    ##     E4                0.000                           
    ##   E3 ~~                                               
    ##     E4                0.000                           
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .T1                0.137    0.212    0.647    0.517
    ##    .T2                0.231    0.212    1.090    0.276
    ##    .T3               -0.085    0.212   -0.400    0.689
    ##    .T4                0.127    0.212    0.598    0.550
    ##     T                 0.000                           
    ##     day1              0.000                           
    ##     day2              0.000                           
    ##     ses1              0.000                           
    ##     ses2              0.000                           
    ##     ses3              0.000                           
    ##     E1                0.000                           
    ##     E2                0.000                           
    ##     E3                0.000                           
    ##     E4                0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     T       (time)    3.712    0.402    9.234    0.000
    ##     day1     (day)    0.350    0.067    5.194    0.000
    ##     day2     (day)    0.350    0.067    5.194    0.000
    ##     ses1    (sssn)    0.149    0.049    3.016    0.003
    ##     ses2    (sssn)    0.149    0.049    3.016    0.003
    ##     ses3    (sssn)    0.149    0.049    3.016    0.003
    ##     E1         (e)    0.267    0.027   10.009    0.000
    ##     E2         (e)    0.267    0.027   10.009    0.000
    ##     E3         (e)    0.267    0.027   10.009    0.000
    ##     E4         (e)    0.267    0.027   10.009    0.000
    ##    .T1                0.000                           
    ##    .T2                0.000                           
    ##    .T3                0.000                           
    ##    .T4                0.000                           
    ## 
    ## Constraints:
    ##                                                |Slack|
    ##     time - (0.0001)                              3.712
    ##     day - (0.0001)                               0.350
    ##     session - (0.0001)                           0.149
    ##     e - (0.0001)                                 0.267

``` r
anova(m1, m0)
```

    ## Chi-Squared Difference Test
    ## 
    ##    Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)    
    ## m1 12 2429.3 2482.1 13.011                                  
    ## m0 16 2445.2 2484.8 36.846     23.834       4  8.621e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
