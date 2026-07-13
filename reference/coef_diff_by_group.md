# Tabulates breath test parameter differences of groups

Given a fit to 13C breath test curves, computes between-group confidence
intervals and p-values, for examples of the half emptying time `t50`,
with correction for multiple testing.

## Usage

``` r
coef_diff_by_group(fit, mcp_group = "Tukey", reference_group = NULL, ...)
```

## Arguments

- fit:

  Object of class `breathtestfit`, for example from
  [`nlme_fit`](https://dmenne.github.io/breathtestcore/reference/nlme_fit.md),
  [`nls_fit`](https://dmenne.github.io/breathtestcore/reference/nls_fit.md)

- mcp_group:

  "Tukey" (default) for all pairwise comparisons, "Dunnett" for
  comparisons relative to the reference group.

- reference_group:

  Used as the first group and as reference group for
  `mcp_group == "Dunnett"`

- ...:

  Not used

## Value

A `tibble` of class `coef_diff_by_group` with columns

- parameter:

  Parameter of fit, e.g. `beta, k, m, t50`

- method:

  Method used to compute parameter. `exp_beta` refers to primary fit
  parameters `beta, k, m`. `maes_ghoos` uses the method from Maes B D,
  Ghoos Y F, Rutgeerts P J, Hiele M I, Geypens B and Vantrappen G 1994
  Dig. Dis. Sci. 39 S104-6. `bluck_coward` is the self-correcting method
  from Bluck L J C and Coward W A 2006

- groups:

  Which pairwise difference, e.g `solid - liquid`

- estimate:

  Estimate of the difference

- conf.low, conf.high:

  Lower and upper 95 A comparison is significantly different from zero
  when both estimates have the same sign.

- p.value:

  p-value of the difference against 0, corrected for multiple testing

## Examples

``` r
library(dplyr)
data("usz_13c")
data = usz_13c %>%
  dplyr::filter( patient_id %in%
    c("norm_001", "norm_002", "norm_003", "norm_004", "pat_001", "pat_002","pat_003")) %>%
  cleanup_data()
fit = nls_fit(data)
coef_diff_by_group(fit)
#> # A tibble: 27 × 7
#>    parameter method   groups                 estimate conf.low conf.high p.value
#>    <chr>     <chr>    <chr>                     <dbl>    <dbl>     <dbl>   <dbl>
#>  1 beta      exp_beta solid_normal - liquid…  1.40e-1 -3.29e-1   6.09e-1 7.63e-1
#>  2 beta      exp_beta solid_patient - liqui…  2.45e-2 -4.77e-1   5.26e-1 9.93e-1
#>  3 beta      exp_beta solid_patient - solid… -1.16e-1 -5.85e-1   3.53e-1 8.32e-1
#>  4 deviance  exp_beta solid_normal - liquid… -1.41e+1 -2.96e+1   1.38e+0 8.21e-2
#>  5 deviance  exp_beta solid_patient - liqui…  5.20e+1 -2.95e+0   1.07e+2 6.76e-2
#>  6 deviance  exp_beta solid_patient - solid…  6.62e+1  1.17e+1   1.21e+2 1.31e-2
#>  7 k         exp_beta solid_normal - liquid… -3.45e-3 -6.00e-3  -9.01e-4 4.45e-3
#>  8 k         exp_beta solid_patient - liqui… -6.04e-3 -9.60e-3  -2.48e-3 2.25e-4
#>  9 k         exp_beta solid_patient - solid… -2.59e-3 -5.97e-3   7.93e-4 1.71e-1
#> 10 m         exp_beta solid_normal - liquid… -1.57e+1 -1.91e+1  -1.24e+1 0      
#> # ℹ 17 more rows
# \donttest{
fit = nlme_fit(data)
coef_diff_by_group(fit)
#> # A tibble: 24 × 7
#>    parameter method       groups            estimate conf.low conf.high  p.value
#>    <chr>     <chr>        <chr>                <dbl>    <dbl>     <dbl>    <dbl>
#>  1 beta      exp_beta     solid_normal - l…  8.74e-2 -1.70e-1   3.44e-1 7.05e- 1
#>  2 beta      exp_beta     solid_patient - …  1.17e-1 -1.58e-1   3.92e-1 5.78e- 1
#>  3 beta      exp_beta     solid_patient - …  2.96e-2 -2.27e-1   2.86e-1 9.61e- 1
#>  4 k         exp_beta     solid_normal - l… -3.73e-3 -5.35e-3  -2.11e-3 1.80e- 7
#>  5 k         exp_beta     solid_patient - … -5.51e-3 -8.22e-3  -2.80e-3 4.52e- 6
#>  6 k         exp_beta     solid_patient - … -1.78e-3 -4.38e-3   8.21e-4 2.42e- 1
#>  7 m         exp_beta     solid_normal - l… -1.55e+1 -2.00e+1  -1.11e+1 7.44e-15
#>  8 m         exp_beta     solid_patient - … -4.48e+0 -2.44e+1   1.54e+1 8.54e- 1
#>  9 m         exp_beta     solid_patient - …  1.11e+1 -8.72e+0   3.08e+1 3.82e- 1
#> 10 t50       bluck_coward solid_normal - l…  1.49e+1  3.90e+0   2.59e+1 4.28e- 3
#> # ℹ 14 more rows
# }
# TODO: Add example for Stan fit typecast to class \code{breathtestfit} to compute
# confidence intervals instead of credible intervals
```
