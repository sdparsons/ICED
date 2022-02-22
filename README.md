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

## simulate data

We’ll simulate data to run the ICED model on. The `sim_ICED` function
takes the model structure dataframe we used earlier and a list of
variances for each latent variable.

``` r
sim1 <- sim_ICED(struc,
                 variances = list(time = 10,
                                  day = 2,
                                  session = 1,
                                  error = 3),
                 n = 2000)

head(sim1)
```

    ##           T1         T2        T3         T4
    ## 1  0.8529433  0.2744556  3.620116  0.3717968
    ## 2 -2.1493394 -4.7157453 -3.846986 -0.9015089
    ## 3  2.6485623  2.0071485  5.726791  6.5298138
    ## 4 -7.9424647 -3.0994557 -2.031928 -3.8026836
    ## 5  6.0055917 -1.2844163  1.574438  1.9936629
    ## 6  1.9358403 -1.0520566  2.302554  5.3258177

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
    ## [1] "ICC1 = 0.623757288017518"
    ##    timeest     dayest sessionest       eest 
    ##  9.9577122  1.8309141  0.9791315  3.1963237

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
    ## [1] "ICC1 = 0.681300341947098"
    ##      timeest       dayest   sessionest         eest 
    ## 12.256490408  2.478900145  0.000100193  3.254358449

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
relevant outputs

``` r
res1 <- run_ICED(model = syn,
                 data = sim1)
```

    ## $ICC
    ## [1] 0.6429239
    ## 
    ## $ICC2
    ## [1] 0.833994
    ## 
    ## $timeest
    ## [1] 10.75619
    ## 
    ## $dayest
    ## [1] 2.192604
    ## 
    ## $sessionest
    ## [1] 0.8357306
    ## 
    ## $eest
    ## [1] 2.94559
    ## 
    ## $EffectiveError
    ## [1] 2.141013
    ## 
    ## $AbsoluteError
    ## [1] 2.111276
    ## 
    ## $phi_dependability
    ## [1] 0.8359214
    ## 
    ## $lavaan
    ## lavaan 0.6-8 ended normally after 211 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        14
    ##   Number of inequality constraints                   3
    ##                                                       
    ##   Number of observations                          2000
    ##   Number of missing patterns                         1
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                 4.325
    ##   Degrees of freedom                                 6
    ##   P-value (Chi-square)                           0.633

we can also bootstrap our estimates. The output now includes 95% CIs on
the ICC and ICC2. Best to use more than 10 boots, but set to 10 for
speed here

``` r
run_ICED(model = syn,
         data = sim1,
         boot = 10)
```

    ## Warning in norm.inter(t, alpha): extreme order statistics used as endpoints

    ## Warning in norm.inter(t, alpha): extreme order statistics used as endpoints

    ## $ICC
    ## [1] 0.6429239
    ## 
    ## $ICC_CIs
    ## [1] 0.6214441 0.6793939
    ## 
    ## $ICC2
    ## [1] 0.833994
    ## 
    ## $ICC2_CIs
    ## [1] 0.8280621 0.8406967
    ## 
    ## $timeest
    ## [1] 10.75619
    ## 
    ## $dayest
    ## [1] 2.192604
    ## 
    ## $sessionest
    ## [1] 0.8357306
    ## 
    ## $eest
    ## [1] 2.94559
    ## 
    ## $EffectiveError
    ## [1] 2.141013
    ## 
    ## $AbsoluteError
    ## [1] 2.111276
    ## 
    ## $phi_dependability
    ## [1] 0.8359214
    ## 
    ## $lavaan
    ## lavaan 0.6-8 ended normally after 211 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        14
    ##   Number of inequality constraints                   3
    ##                                                       
    ##   Number of observations                          2000
    ##   Number of missing patterns                         1
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                 4.325
    ##   Degrees of freedom                                 6
    ##   P-value (Chi-square)                           0.633

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
    ##  time == 10.7561916194784
    ##  day == 2.19260400095988
    ##  session == 0
    ##  e == 2.94559036661988

``` r
res2 <- run_ICED(syntax2,
                 sim1)
```

    ## $ICC
    ## [1] 0.676729
    ## 
    ## $ICC2
    ## [1] 0.8544193
    ## 
    ## $timeest
    ## [1] 10.75619
    ## 
    ## $dayest
    ## [1] 2.192604
    ## 
    ## $sessionest
    ## [1] 0
    ## 
    ## $eest
    ## [1] 2.94559
    ## 
    ## $EffectiveError
    ## [1] 1.8327
    ## 
    ## $AbsoluteError
    ## [1] 1.8327
    ## 
    ## $phi_dependability
    ## [1] 0.8544193
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
    ##   Test statistic                               100.028
    ##   Degrees of freedom                                10
    ##   P-value (Chi-square)                           0.000

``` r
anova(res1$lavaan,
      res2$lavaan)
```

    ## Chi-Squared Difference Test
    ## 
    ##             Df   AIC   BIC   Chisq Chisq diff Df diff Pr(>Chisq)    
    ## res1$lavaan  6 39726 39771   4.325                                  
    ## res2$lavaan 10 39814 39836 100.028     95.703       4  < 2.2e-16 ***
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
                 n = 100)

sim_lo <- sim_ICED(structure = struc,
                 variances = variances_lo,
                 n = 100)

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

    ## lavaan 0.6-8 ended normally after 336 iterations
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
    ##   Test statistic                                13.357
    ##   Degrees of freedom                                12
    ##   P-value (Chi-square)                           0.344
    ##   Test statistic for each group:
    ##     high                                         3.653
    ##     low                                          9.704
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
    ##    .T1                0.102    0.236    0.434    0.665
    ##    .T2                0.135    0.236    0.573    0.567
    ##    .T3                0.203    0.236    0.859    0.390
    ##    .T4                0.178    0.236    0.753    0.451
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
    ##     T       (ltt1)    4.931    0.733    6.725    0.000
    ##     day1    (ltd1)    0.246    0.082    2.988    0.003
    ##     day2    (ltd1)    0.246    0.082    2.988    0.003
    ##     ses1    (lts1)    0.228    0.061    3.762    0.000
    ##     ses2    (lts1)    0.228    0.061    3.762    0.000
    ##     ses3    (lts1)    0.228    0.061    3.762    0.000
    ##     E1      (ltg1)    0.168    0.024    7.073    0.000
    ##     E2      (ltg1)    0.168    0.024    7.073    0.000
    ##     E3      (ltg1)    0.168    0.024    7.073    0.000
    ##     E4      (ltg1)    0.168    0.024    7.073    0.000
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
    ##    .T1               -0.135    0.176   -0.770    0.441
    ##    .T2               -0.167    0.176   -0.949    0.343
    ##    .T3               -0.074    0.176   -0.421    0.674
    ##    .T4               -0.112    0.176   -0.637    0.524
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
    ##     T       (ltt2)    2.343    0.373    6.281    0.000
    ##     day1    (ltd2)    0.274    0.094    2.921    0.003
    ##     day2    (ltd2)    0.274    0.094    2.921    0.003
    ##     ses1    (lts2)    0.226    0.074    3.055    0.002
    ##     ses2    (lts2)    0.226    0.074    3.055    0.002
    ##     ses3    (lts2)    0.226    0.074    3.055    0.002
    ##     E1      (ltg2)    0.241    0.034    7.080    0.000
    ##     E2      (ltg2)    0.241    0.034    7.080    0.000
    ##     E3      (ltg2)    0.241    0.034    7.080    0.000
    ##     E4      (ltg2)    0.241    0.034    7.080    0.000
    ##    .T1                0.000                           
    ##    .T2                0.000                           
    ##    .T3                0.000                           
    ##    .T4                0.000                           
    ## 
    ## Constraints:
    ##                                                |Slack|
    ##     lattimegroup1 - (0.0001)                     4.931
    ##     lattimegroup2 - (0.0001)                     2.343
    ##     latdaygroup1 - (0.0001)                      0.246
    ##     latdaygroup2 - (0.0001)                      0.274
    ##     latsessiongroup1 - (0.0001)                  0.228
    ##     latsessiongroup2 - (0.0001)                  0.226
    ##     lategroup1 - (0.0001)                        0.168
    ##     lategroup2 - (0.0001)                        0.241

``` r
summary(m0)
```

    ## lavaan 0.6-8 ended normally after 394 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        28
    ##   Number of inequality constraints                   3
    ##                                                       
    ##   Number of observations per group:                   
    ##     high                                           100
    ##     low                                            100
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                29.071
    ##   Degrees of freedom                                16
    ##   P-value (Chi-square)                           0.023
    ##   Test statistic for each group:
    ##     high                                        10.521
    ##     low                                         18.550
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
    ##    .T1                0.102    0.208    0.492    0.623
    ##    .T2                0.135    0.208    0.650    0.516
    ##    .T3                0.203    0.208    0.975    0.330
    ##    .T4                0.178    0.208    0.855    0.393
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
    ##     T       (time)    3.634    0.391    9.299    0.000
    ##     day1     (day)    0.265    0.062    4.250    0.000
    ##     day2     (day)    0.265    0.062    4.250    0.000
    ##     ses1    (sssn)    0.222    0.047    4.695    0.000
    ##     ses2    (sssn)    0.222    0.047    4.695    0.000
    ##     ses3    (sssn)    0.222    0.047    4.695    0.000
    ##     E1         (e)    0.206    0.021   10.006    0.000
    ##     E2         (e)    0.206    0.021   10.006    0.000
    ##     E3         (e)    0.206    0.021   10.006    0.000
    ##     E4         (e)    0.206    0.021   10.006    0.000
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
    ##    .T1               -0.135    0.208   -0.650    0.516
    ##    .T2               -0.167    0.208   -0.801    0.423
    ##    .T3               -0.074    0.208   -0.356    0.722
    ##    .T4               -0.112    0.208   -0.538    0.591
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
    ##     T       (time)    3.634    0.391    9.299    0.000
    ##     day1     (day)    0.265    0.062    4.250    0.000
    ##     day2     (day)    0.265    0.062    4.250    0.000
    ##     ses1    (sssn)    0.222    0.047    4.695    0.000
    ##     ses2    (sssn)    0.222    0.047    4.695    0.000
    ##     ses3    (sssn)    0.222    0.047    4.695    0.000
    ##     E1         (e)    0.206    0.021   10.006    0.000
    ##     E2         (e)    0.206    0.021   10.006    0.000
    ##     E3         (e)    0.206    0.021   10.006    0.000
    ##     E4         (e)    0.206    0.021   10.006    0.000
    ##    .T1                0.000                           
    ##    .T2                0.000                           
    ##    .T3                0.000                           
    ##    .T4                0.000                           
    ## 
    ## Constraints:
    ##                                                |Slack|
    ##     time - (0.0001)                              3.634
    ##     day - (0.0001)                               0.265
    ##     session - (0.0001)                           0.222

``` r
anova(m1, m0)
```

    ## Chi-Squared Difference Test
    ## 
    ##    Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)   
    ## m1 12 2363.2 2416.0 13.357                                 
    ## m0 16 2371.0 2410.5 29.071     15.713       4   0.003429 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
