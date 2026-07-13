# So-called lag time from Maes/Ghoos fit

Computes `tlag` from uncorrected fit to the beta exponential function.
The name `tlag` is a misnomer; it simply is the maximum of the PDR
curve, so in papers by Bluck et al. it is renamed to t_max.

Maes B D, Ghoos Y F, Rutgeerts P J, Hiele M I, Geypens B and Vantrappen
G 1994 Dig. Dis. Sci. 39 S104-6.

## Usage

``` r
tlag_maes_ghoos(cf)
```

## Arguments

- cf:

  named vector of coefficients; only `k` and `beta` are required `k` is
  measured in 1/min (e.g. 0.01/min).

## Value

Lag time as defined from Maes/Ghoos fit

## See also

[`exp_beta`](https://dmenne.github.io/breathtestcore/reference/exp_beta.md),
and
[`t50_bluck_coward`](https://dmenne.github.io/breathtestcore/reference/t50_bluck_coward.md)
for an example.
