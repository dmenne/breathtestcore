# Half-emptying time t50 from Maes/Ghoos fit with scintigraphic correction

Half-emptying time t50 in minutes from beta exponential function fit,
with linear and rather arbitrary correction for scintigraphic values.
This is given for comparison with published data only; there is little
justification to use it, even if it is closer to real gastric emptying
times as determined by MRI or scintigraphy. Ghoos YF, Maes BD, Geypens
BJ, Mys G, Hiele MI, Rutgeerts PJ, Vantrappen G. Measurement of gastric
emptying rate of solids by means of a carbon-labeled octanoic acid
breath test. Gastroenterology. 1993;104:1640-1647.

## Usage

``` r
t50_maes_ghoos_scintigraphy(cf)
```

## Arguments

- cf:

  named vector of coefficients; only `k` and `beta` are required

## Value

Time where value is 1/2 of maximum, i.e. t50 in minutes.

## See also

[`exp_beta`](https://dmenne.github.io/breathtestcore/reference/exp_beta.md),
and
[`t50_bluck_coward`](https://dmenne.github.io/breathtestcore/reference/t50_bluck_coward.md)
for an example.
