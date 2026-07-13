# S3 coef and summary for breathtestfit

Function `coef` extracts the estimates such as t50, tlag, from fitted
13C beta exponential models. The result is the same as `fit$coef`, but
without column `stat`, which always is `"estimate"` for
[`nls_fit`](https://dmenne.github.io/breathtestcore/reference/nls_fit.md)
and
[`nlme_fit`](https://dmenne.github.io/breathtestcore/reference/nlme_fit.md).

The `summary` method only extracts `t50` by the Maes/Ghoos method

## Usage

``` r
# S3 method for class 'breathtestfit'
coef(object, ...)
```

## Arguments

- object:

  of class `breathtestfit`, as returned by
  [`nls_fit`](https://dmenne.github.io/breathtestcore/reference/nls_fit.md)
  or
  [`nlme_fit`](https://dmenne.github.io/breathtestcore/reference/nlme_fit.md)

- ...:

  other parameters passed to methods

## Examples

``` r
# Generate simulated data
data = cleanup_data(simulate_breathtest_data())
# Fit with the population method
fit = nlme_fit(data)
# All coefficients in the long form
coef(fit)
#> # A tibble: 80 × 5
#>    patient_id group parameter method                      value
#>    <chr>      <chr> <chr>     <chr>                       <dbl>
#>  1 rec_01     A     m         exp_beta                 37.1    
#>  2 rec_01     A     k         exp_beta                  0.00955
#>  3 rec_01     A     beta      exp_beta                  1.70   
#>  4 rec_01     A     t50       bluck_coward             23.0    
#>  5 rec_01     A     t50       maes_ghoos              115.     
#>  6 rec_01     A     t50       maes_ghoos_scintigraphy  43.4    
#>  7 rec_01     A     tlag      bluck_coward            -16.8    
#>  8 rec_01     A     tlag      maes_ghoos               55.7    
#>  9 rec_02     A     m         exp_beta                 43.3    
#> 10 rec_02     A     k         exp_beta                  0.0112 
#> # ℹ 70 more rows
# Access coefficients directly
fit$coef
#> # A tibble: 80 × 6
#>    patient_id group parameter method                  stat         value
#>    <chr>      <chr> <chr>     <chr>                   <chr>        <dbl>
#>  1 rec_01     A     m         exp_beta                estimate  37.1    
#>  2 rec_01     A     k         exp_beta                estimate   0.00955
#>  3 rec_01     A     beta      exp_beta                estimate   1.70   
#>  4 rec_01     A     t50       bluck_coward            estimate  23.0    
#>  5 rec_01     A     t50       maes_ghoos              estimate 115.     
#>  6 rec_01     A     t50       maes_ghoos_scintigraphy estimate  43.4    
#>  7 rec_01     A     tlag      bluck_coward            estimate -16.8    
#>  8 rec_01     A     tlag      maes_ghoos              estimate  55.7    
#>  9 rec_02     A     m         exp_beta                estimate  43.3    
#> 10 rec_02     A     k         exp_beta                estimate   0.0112 
#> # ℹ 70 more rows
# Only t50 by Maes/Ghoos
# Can also be used with stan fit (slow!)
if (FALSE) { # \dontrun{
if (require("breathteststan")) {
  fit = stan_fit(data, iter = 300, chain = 1)
  coef(fit)
  # We get quantiles here in key/value format
  unique(fit$coef$stat)
}
} # }
```
