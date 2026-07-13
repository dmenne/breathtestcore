# Bluck-Coward self-corrected half-emptying time

Uses Newton's method to solve the self-corrected Bluck-Coward equation
for 1/2 to compute the half-emptying time t_50.

See also equation G(n,t) in

Bluck LJC, Jackson S, Vlasakakis G, Mander A (2011) Bayesian
hierarchical methods to interpret the 13C-octanoic acid breath test for
gastric emptying. Digestion 83_96-107, page 98.

## Usage

``` r
t50_bluck_coward(cf)
```

## Arguments

- cf:

  Named vector of coefficients; only `k` and `beta` are required. In
  this package, `k` is measured in units of 1/min (e.g. 0.01/min), in
  publications it is often quoted as 1/h (e.g. 0.6/h).

## Value

Time where value is 1/2 of the maximum, i.e. t_50 or t_1/2 in minutes;
in the publication by Bluck et al, the parameter is called t_1/2(in).

## See also

[`exp_beta`](https://dmenne.github.io/breathtestcore/reference/exp_beta.md)

## Examples

``` r
# From table 3 and 4 in Bluck et al.; values for \code{k} and \code{beta} 
# (nls, bayesian) are entered and checked against the tabulated values of 
# t_{1/2(in)}.
# Most errors are small, but there are some outliers; errors in paper table?
# Parameters and Bluck et al. results:
# table 3 of Bluck et al.
cf3 = data.frame(
          method = rep(c("nls", "bayesian")),
          group = rep(c("lean", "obese"),each=2),
          k =    c(0.576,0.606,0.529,0.608),
          beta = c(5.24, 5.79, 5.95, 7.54),
          t12 =  c(3.67, 3.63, 4.23, 3.99),
          t12in = c(2.076, 2.110, 2.422, 2.466),
          tlag = c(2.88, 2.88, 3.34, 3.26),
          tlagin = c(1.632, 1.724, 1.92, 2.101)
)
cf3 = dplyr::mutate(cf3,
          t50_maes_ghoos = t50_maes_ghoos(cf3),
          t50_bluck_coward = t50_bluck_coward(cf3),
          tlag_maes_ghoos = tlag_maes_ghoos(cf3),
          tlag_bluck_coward = tlag_bluck_coward(cf3),
          err_t50_maes_ghoos = round(100*(t50_maes_ghoos-t12)/t12, 2),
          err_t50_bluck_coward =
            round(100*(t50_bluck_coward-t12in)/t12in, 2),
          err_lag_maes = round(100*(tlag_maes_ghoos-tlag)/tlag,2),
          err_lag_bluck_coward =
            round(100*(tlag_bluck_coward-tlagin)/tlagin,2)
)
cf3
#>     method group    k beta t12 t12in tlag tlagin t50_maes_ghoos
#> 1      nls  lean 0.58  5.2 3.7   2.1  2.9    1.6            3.6
#> 2 bayesian  lean 0.61  5.8 3.6   2.1  2.9    1.7            3.6
#> 3      nls obese 0.53  6.0 4.2   2.4  3.3    1.9            4.2
#> 4 bayesian obese 0.61  7.5 4.0   2.5  3.3    2.1            4.0
#>   t50_bluck_coward tlag_maes_ghoos tlag_bluck_coward err_t50_maes_ghoos
#> 1              2.1             2.9               1.7              -1.21
#> 2              2.1             2.9               1.8              -0.81
#> 3              2.5             3.4               2.1              -1.34
#> 4              2.5             3.3               2.2               0.26
#>   err_t50_bluck_coward err_lag_maes err_lag_bluck_coward
#> 1                 0.58        -0.15                  2.5
#> 2                 1.37         0.62                  1.8
#> 3                 3.18         0.94                  7.3
#> 4                 3.20         1.92                  3.9
# table 4
# there are large differences for mj3, both using the bayesian (26%)
# and the nls method (16%).  The other data are within the expected limits
cf4 = data.frame(
          method = rep(c("nls", "bayesian"),each=3),
          group = rep(c("mj1",   "mj2",   "mj3")),  
          k = c(0.585,  0.437,  0.380,  0.588,  0.418,  0.361),  
          beta=c(4.35,  4.08,  4.44,  4.49, 4.30, 4.29), 
          t12 = c(3.39, 4.25, 4.82, 3.40, 4.61, 5.09), 
          t12in = c(1.77, 2.16, 2.19, 1.81, 2.34, 2.43), 
          tlag = c(2.56, 3.17, 3.39, 2.58, 3.40, 3.62), 
          tlagin = c(1.30, 1.53, 1.33, 1.35, 1.65, 1.57)
)
cf4 = dplyr::mutate(cf4,
          t50_maes_ghoos = t50_maes_ghoos(cf4),
          t50_bluck_coward = t50_bluck_coward(cf4),
          tlag_maes_ghoos = tlag_maes_ghoos(cf4),
          tlag_bluck_coward = tlag_bluck_coward(cf4),
          err_t50_maes_ghoos = unlist(round(100*(t50_maes_ghoos-t12)/t12)),
          err_t50_bluck_coward =
            round(100*(t50_bluck_coward-t12in)/t12in,2),
          err_lag_maes = round(100*(tlag_maes_ghoos-tlag)/tlag,2),
          err_lag_bluck_coward =
            round(100*(tlag_bluck_coward-tlagin)/tlagin,2)
)
cf4
#>     method group    k beta t12 t12in tlag tlagin t50_maes_ghoos
#> 1      nls   mj1 0.58  4.3 3.4   1.8  2.6    1.3            3.3
#> 2      nls   mj2 0.44  4.1 4.2   2.2  3.2    1.5            4.2
#> 3      nls   mj3 0.38  4.4 4.8   2.2  3.4    1.3            5.1
#> 4 bayesian   mj1 0.59  4.5 3.4   1.8  2.6    1.4            3.3
#> 5 bayesian   mj2 0.42  4.3 4.6   2.3  3.4    1.6            4.6
#> 6 bayesian   mj3 0.36  4.3 5.1   2.4  3.6    1.6            5.3
#>   t50_bluck_coward tlag_maes_ghoos tlag_bluck_coward err_t50_maes_ghoos
#> 1              1.8             2.5               1.3                 -3
#> 2              2.2             3.2               1.6                  0
#> 3              2.8             3.9               2.1                  6
#> 4              1.8             2.6               1.4                 -3
#> 5              2.4             3.5               1.8                 -1
#> 6              2.8             4.0               2.1                  4
#>   err_t50_bluck_coward err_lag_maes err_lag_bluck_coward
#> 1                -0.56         -1.8                  2.2
#> 2                 2.82          1.5                  6.6
#> 3                26.03         15.7                 57.8
#> 4                -0.50         -1.0                  1.9
#> 5                 4.19          2.6                 11.0
#> 6                15.93         11.4                 34.6
```
