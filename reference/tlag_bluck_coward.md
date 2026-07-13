# Lag phase for Bluck-Coward self-correcting fit

This parameter is probably not very useful, as it can be negative

## Usage

``` r
tlag_bluck_coward(cf)
```

## Arguments

- cf:

  named vector of coefficients; only `k` and `beta` are required. Note
  that in this package, `k` is measured in 1/min (e.g. 0.01/min), while
  in the literature is is often quoted as 1/h (e.g. 0.6/h).

## Value

Lag phase in minutes (time t at which the maximum in the rate of change
of g(t) occurs)

## See also

[`exp_beta`](https://dmenne.github.io/breathtestcore/reference/exp_beta.md),
and
[`t50_bluck_coward`](https://dmenne.github.io/breathtestcore/reference/t50_bluck_coward.md)
for an example.
