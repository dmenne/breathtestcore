# Half-emptying time by Maes/Ghoos method

Half-emptying time t50 as determined from the fit of a beta exponential
function. In the Maes/Ghoos model, it is defined as the time when the
area under curve (AUC) is 50% of the AUC from 0 to infinity.

Maes B D, Ghoos Y F, Rutgeerts P J, Hiele M I, Geypens B and Vantrappen
G 1994 Dig. Dis. Sci. 39 S104-6.

## Usage

``` r
t50_maes_ghoos(cf)
```

## Arguments

- cf:

  named vector of coefficients; only `k` and `beta` are required note
  that `k` is measured in 1/min (e.g. 0.01/min), usually it is quoted as
  1/h (e.g. 0.6/h).

## Value

Time in minutes when area under curve is 50% of the AUC to infinity. In
the Maes/Ghoos model, this is used as a surrogate for gastric emptying
half time `t50`.

## See also

[`exp_beta`](https://dmenne.github.io/breathtestcore/reference/exp_beta.md),
and
[`t50_bluck_coward`](https://dmenne.github.io/breathtestcore/reference/t50_bluck_coward.md)
for an example.

## Examples

``` r
# Integral from 0 to infinity is 100 at dose 100 mg
integrate(exp_beta, 0, Inf, beta = 1.5, k = 0.01, m = 1, dose = 100)
#> 100 with absolute error < 0.0014
t50_mg = t50_maes_ghoos(c(beta = 1.5, k = 0.01, dose = 100))
t50_mg
#> [1] 99
# Integral to half-emptying time \code{t50_maes_ghoos} is 50 
integrate(exp_beta, 0, t50_mg, beta = 1.5, k = 0.01, m = 1, dose = 100)
#> 50 with absolute error < 0.0042
```
