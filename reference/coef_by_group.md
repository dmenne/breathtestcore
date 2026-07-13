# Tabulates per-group breath test parameters

Given a fit to 13C breath test curves, computes absolute values and
their confidence intervals of parameters, e.g. of the half emptying time
`t50`. Generic S3 method for class breathtestfit.

## Usage

``` r
coef_by_group(fit, ...)
```

## Arguments

- fit:

  Object of class `breathtestfit`, for example from
  [`nlme_fit`](https://dmenne.github.io/breathtestcore/reference/nlme_fit.md),
  [`nls_fit`](https://dmenne.github.io/breathtestcore/reference/nls_fit.md)
  or
  [`stan_fit`](https://dmenne.github.io/breathteststan/reference/stan_fit.html)

- ...:

  Not used

## Value

A `tibble` of class `coef_by_group` with columns

- parameter:

  Parameter of fit, e.g. `beta, k, m, t50`

- method:

  Method used to compute parameter. `exp_beta` refers to primary fit
  parameters `beta, k, m`. `maes_ghoos` uses the method from Maes B D,
  Ghoos Y F, Rutgeerts P J, Hiele M I, Geypens B and Vantrappen G 1994
  Dig. Dis. Sci. 39 S104-6. `bluck_coward` is the self-correcting method
  from Bluck L J C and Coward W A 2006

- group:

  Grouping parameter of the fit, e.g. `patient, normal, liquid, solid`

- estimate:

  Parameter estimate

- conf.low, conf.high:

  Lower and upper 95 estimate.

- diff_group:

  Letters a, b, c indicate that parameter would be in mutually
  significantly different groups. Letter combinations like `ab` or `abc`
  indicated that this parameter is not significantly different from the
  given other groups in a Tukey-corrected pairwise test.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
data("usz_13c")
data = usz_13c %>%
  dplyr::filter( patient_id %in%
    c("norm_001", "norm_002", "norm_003", "norm_004", "pat_001", "pat_002","pat_003")) %>%
  cleanup_data()
fit = nls_fit(data)
coef_by_group(fit)
#> # A tibble: 27 × 7
#>    parameter method   group         estimate  conf.low conf.high diff_group
#>    <chr>     <chr>    <chr>            <dbl>     <dbl>     <dbl> <chr>     
#>  1 beta      exp_beta liquid_normal  2.09      1.73      2.45    a         
#>  2 beta      exp_beta solid_normal   2.23      1.92      2.54    a         
#>  3 beta      exp_beta solid_patient  2.11      1.75      2.48    a         
#>  4 deviance  exp_beta liquid_normal 16.8     -20.1      53.7     ab        
#>  5 deviance  exp_beta solid_normal   2.72    -33.4      38.8     a         
#>  6 deviance  exp_beta solid_patient 68.9      27.2     110.      b         
#>  7 k         exp_beta liquid_normal  0.0131    0.0106    0.0156  a         
#>  8 k         exp_beta solid_normal   0.00963   0.00737   0.0119  b         
#>  9 k         exp_beta solid_patient  0.00704   0.00444   0.00965 b         
#> 10 m         exp_beta liquid_normal 36.5      22.6      50.4     a         
#> # ℹ 17 more rows
# \donttest{
fit = nlme_fit(data)
coef_by_group(fit)
#> # A tibble: 24 × 7
#>    parameter method       group         estimate conf.low conf.high diff_group
#>    <chr>     <chr>        <chr>            <dbl>    <dbl>     <dbl> <chr>     
#>  1 beta      exp_beta     liquid_normal  2.09     1.89      2.28    a         
#>  2 beta      exp_beta     solid_normal   2.17     2.00      2.35    a         
#>  3 beta      exp_beta     solid_patient  2.20     2.01      2.40    a         
#>  4 k         exp_beta     liquid_normal  0.0131   0.0112    0.015   a         
#>  5 k         exp_beta     solid_normal   0.00937  0.00764   0.0111  b         
#>  6 k         exp_beta     solid_patient  0.00759  0.00559   0.00960 b         
#>  7 m         exp_beta     liquid_normal 36.3     23.1      49.6     a         
#>  8 m         exp_beta     solid_normal  20.8      7.72     33.9     b         
#>  9 m         exp_beta     solid_patient 31.9     16.8      47.0     ab        
#> 10 t50       bluck_coward liquid_normal 29.7     19.9      39.5     a         
#> # ℹ 14 more rows
# }
```
