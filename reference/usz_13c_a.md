# Exotic 13C breath test data

13C time series PDR data from three different groups in a randomized (=
not-crossover) design. This are unpublished data from [Gastroenterology
and Hepatology, University Hospital
Zurich](https://www.usz.ch/fachbereich/gastroenterologie-und-hepatologie/).

Data are formatted as described in
[`usz_13c`](https://dmenne.github.io/breathtestcore/reference/usz_13c.md).
These time series present a challenge for algorithms.

## Usage

``` r
data(usz_13c_a)
```

## Examples

``` r
# \donttest{
library(dplyr)
library(ggplot2)
data(usz_13c_a)
d = usz_13c_a %>% 
  cleanup_data() %>% # recommended to test for validity
  nlme_fit()
plot(d)

# }
```
