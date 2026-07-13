# S3 method to extract the fit's residual standard deviation

Functions for `nls` and `nlme` are available; additional functions for
Stan-based fits are defined in package `breathteststan`.

## Usage

``` r
# S3 method for class 'breathtestnlmefit'
sigma(object, ...)
```

## Arguments

- object:

  Result of class `breathtestfit`

- ...:

  Not used

## Value

A numeric value giving the standard deviation of the residuals.
