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

    ##           T1         T2         T3        T4
    ## 1  0.3859495  2.9976520  2.2406373 -2.213329
    ## 2 -1.3263371 -1.6079571 -2.5485554 -2.141473
    ## 3  3.1847058 -1.6158796 -3.2294465 -3.459617
    ## 4  2.6249049  3.6541991  0.9069199 -1.092978
    ## 5  3.1363230  4.1303944  1.3504050  1.080929
    ## 6  2.0919728 -0.4947136  0.8099815 -3.081045

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
    ## [1] "ICC1 = 0.613690052213968"
    ##    timeest     dayest sessionest       eest 
    ##   9.726541   2.136493   0.955077   3.031161

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
    ## [1] "ICC1 = 0.534068517674343"
    ##    timeest     dayest sessionest       eest 
    ##   4.673251   1.373817   1.457165   1.246050

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
    ## [1] 0.6171321
    ## 
    ## $ICC2
    ## [1] 0.8211038
    ## 
    ## $timeest
    ## [1] 9.435869
    ## 
    ## $dayest
    ## [1] 1.863148
    ## 
    ## $sessionest
    ## [1] 1.083308
    ## 
    ## $eest
    ## [1] 2.907544
    ## 
    ## $EffectiveError
    ## [1] 2.055819
    ## 
    ## $AbsoluteError
    ## [1] 2.019563
    ## 
    ## $phi_dependability
    ## [1] 0.8237026
    ## 
    ## $lavaan
    ## lavaan 0.6-8 ended normally after 205 iterations
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
    ##   Test statistic                                 3.115
    ##   Degrees of freedom                                 6
    ##   P-value (Chi-square)                           0.794

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
    ## [1] 0.6171321
    ## 
    ## $ICC_CIs
    ## [1] 0.5848186 0.6350318
    ## 
    ## $ICC2
    ## [1] 0.8211038
    ## 
    ## $ICC2_CIs
    ## [1] 0.8072732 0.8372958
    ## 
    ## $timeest
    ## [1] 9.435869
    ## 
    ## $dayest
    ## [1] 1.863148
    ## 
    ## $sessionest
    ## [1] 1.083308
    ## 
    ## $eest
    ## [1] 2.907544
    ## 
    ## $EffectiveError
    ## [1] 2.055819
    ## 
    ## $AbsoluteError
    ## [1] 2.019563
    ## 
    ## $phi_dependability
    ## [1] 0.8237026
    ## 
    ## $lavaan
    ## lavaan 0.6-8 ended normally after 205 iterations
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
    ##   Test statistic                                 3.115
    ##   Degrees of freedom                                 6
    ##   P-value (Chi-square)                           0.794

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
    ##  time == 9.43586880843286
    ##  day == 1.86314789770665
    ##  session == 0
    ##  e == 2.90754378952659

``` r
res2 <- run_ICED(syntax2,
                 sim1)
```

    ## $ICC
    ## [1] 0.6641909
    ## 
    ## $ICC2
    ## [1] 0.8505128
    ## 
    ## $timeest
    ## [1] 9.435869
    ## 
    ## $dayest
    ## [1] 1.863148
    ## 
    ## $sessionest
    ## [1] 0
    ## 
    ## $eest
    ## [1] 2.907544
    ## 
    ## $EffectiveError
    ## [1] 1.65846
    ## 
    ## $AbsoluteError
    ## [1] 1.65846
    ## 
    ## $phi_dependability
    ## [1] 0.8505128
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
    ##   Test statistic                               169.387
    ##   Degrees of freedom                                10
    ##   P-value (Chi-square)                           0.000

``` r
anova(res1$lavaan,
      res2$lavaan)
```

    ## Chi-Squared Difference Test
    ## 
    ##             Df   AIC   BIC    Chisq Chisq diff Df diff Pr(>Chisq)    
    ## res1$lavaan  6 39500 39545   3.1148                                  
    ## res2$lavaan 10 39658 39681 169.3869     166.27       4  < 2.2e-16 ***
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

    ## lavaan 0.6-8 ended normally after 389 iterations
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
    ##   Test statistic                                 9.192
    ##   Degrees of freedom                                12
    ##   P-value (Chi-square)                           0.686
    ##   Test statistic for each group:
    ##     high                                         3.820
    ##     low                                          5.372
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
    ##    .T1               -0.014    0.301   -0.047    0.963
    ##    .T2               -0.133    0.301   -0.444    0.657
    ##    .T3               -0.124    0.301   -0.414    0.679
    ##    .T4               -0.076    0.301   -0.253    0.800
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
    ##     T       (ltt1)    8.399    1.223    6.869    0.000
    ##     day1    (ltd1)    0.224    0.083    2.697    0.007
    ##     day2    (ltd1)    0.224    0.083    2.697    0.007
    ##     ses1    (lts1)    0.238    0.065    3.672    0.000
    ##     ses2    (lts1)    0.238    0.065    3.672    0.000
    ##     ses3    (lts1)    0.238    0.065    3.672    0.000
    ##     E1      (ltg1)    0.183    0.026    7.073    0.000
    ##     E2      (ltg1)    0.183    0.026    7.073    0.000
    ##     E3      (ltg1)    0.183    0.026    7.073    0.000
    ##     E4      (ltg1)    0.183    0.026    7.073    0.000
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
    ##    .T1               -0.118    0.171   -0.688    0.491
    ##    .T2               -0.153    0.171   -0.894    0.372
    ##    .T3               -0.153    0.171   -0.897    0.370
    ##    .T4               -0.193    0.171   -1.130    0.259
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
    ##     T       (ltt2)    2.253    0.355    6.351    0.000
    ##     day1    (ltd2)    0.217    0.084    2.564    0.010
    ##     day2    (ltd2)    0.217    0.084    2.564    0.010
    ##     ses1    (lts2)    0.193    0.073    2.641    0.008
    ##     ses2    (lts2)    0.193    0.073    2.641    0.008
    ##     ses3    (lts2)    0.193    0.073    2.641    0.008
    ##     E1      (ltg2)    0.259    0.037    7.083    0.000
    ##     E2      (ltg2)    0.259    0.037    7.083    0.000
    ##     E3      (ltg2)    0.259    0.037    7.083    0.000
    ##     E4      (ltg2)    0.259    0.037    7.083    0.000
    ##    .T1                0.000                           
    ##    .T2                0.000                           
    ##    .T3                0.000                           
    ##    .T4                0.000                           
    ## 
    ## Constraints:
    ##                                                |Slack|
    ##     lattimegroup1 - (0.0001)                     8.399
    ##     lattimegroup2 - (0.0001)                     2.253
    ##     latdaygroup1 - (0.0001)                      0.224
    ##     latdaygroup2 - (0.0001)                      0.216
    ##     latsessiongroup1 - (0.0001)                  0.238
    ##     latsessiongroup2 - (0.0001)                  0.193
    ##     lategroup1 - (0.0001)                        0.183
    ##     lategroup2 - (0.0001)                        0.259

``` r
summary(m0)
```

    ## lavaan 0.6-8 ended normally after 422 iterations
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
    ##   Test statistic                                48.587
    ##   Degrees of freedom                                16
    ##   P-value (Chi-square)                           0.000
    ##   Test statistic for each group:
    ##     high                                        16.780
    ##     low                                         31.807
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
    ##    .T1               -0.014    0.245   -0.057    0.954
    ##    .T2               -0.133    0.245   -0.546    0.585
    ##    .T3               -0.124    0.245   -0.509    0.611
    ##    .T4               -0.076    0.245   -0.311    0.755
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
    ##     T       (time)    5.325    0.557    9.554    0.000
    ##     day1     (day)    0.220    0.059    3.711    0.000
    ##     day2     (day)    0.220    0.059    3.711    0.000
    ##     ses1    (sssn)    0.216    0.049    4.426    0.000
    ##     ses2    (sssn)    0.216    0.049    4.426    0.000
    ##     ses3    (sssn)    0.216    0.049    4.426    0.000
    ##     E1         (e)    0.221    0.022   10.006    0.000
    ##     E2         (e)    0.221    0.022   10.006    0.000
    ##     E3         (e)    0.221    0.022   10.006    0.000
    ##     E4         (e)    0.221    0.022   10.006    0.000
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
    ##    .T1               -0.118    0.245   -0.481    0.631
    ##    .T2               -0.153    0.245   -0.624    0.532
    ##    .T3               -0.153    0.245   -0.627    0.531
    ##    .T4               -0.193    0.245   -0.789    0.430
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
    ##     T       (time)    5.325    0.557    9.554    0.000
    ##     day1     (day)    0.220    0.059    3.711    0.000
    ##     day2     (day)    0.220    0.059    3.711    0.000
    ##     ses1    (sssn)    0.216    0.049    4.426    0.000
    ##     ses2    (sssn)    0.216    0.049    4.426    0.000
    ##     ses3    (sssn)    0.216    0.049    4.426    0.000
    ##     E1         (e)    0.221    0.022   10.006    0.000
    ##     E2         (e)    0.221    0.022   10.006    0.000
    ##     E3         (e)    0.221    0.022   10.006    0.000
    ##     E4         (e)    0.221    0.022   10.006    0.000
    ##    .T1                0.000                           
    ##    .T2                0.000                           
    ##    .T3                0.000                           
    ##    .T4                0.000                           
    ## 
    ## Constraints:
    ##                                                |Slack|
    ##     time - (0.0001)                              5.325
    ##     day - (0.0001)                               0.220
    ##     session - (0.0001)                           0.216

``` r
anova(m1, m0)
```

    ## Chi-Squared Difference Test
    ## 
    ##    Df    AIC    BIC   Chisq Chisq diff Df diff Pr(>Chisq)    
    ## m1 12 2412.6 2465.3  9.1921                                  
    ## m0 16 2444.0 2483.6 48.5866     39.394       4  5.775e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
