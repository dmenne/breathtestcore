# Cumulative exponential beta function

Equation (2), page 4 from Bluck, "Recent advances in the interpretation
of the 13C octanoate breath test for gastric emptying"

## Usage

``` r
cum_exp_beta(minute, dose, cf)
```

## Arguments

- minute:

  time in minutes

- dose:

  in mg

- cf:

  named vector of coefficients; only `k` and `beta` are required. Note
  that `k` is measured in 1/min (e_g\_ 0_01/min), while often it is
  quoted as 1/h (e_g\_ 0_6/h).

## Value

Vector of predicted cumulative pdr

## See also

[`exp_beta`](https://dmenne.github.io/breathtestcore/reference/exp_beta.md)
