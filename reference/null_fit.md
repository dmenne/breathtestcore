# Convert data to class breathtestfit

Does not change the data set, but returns a class suitable for plotting
raw data with
[`plot.breathtestfit`](https://dmenne.github.io/breathtestcore/reference/plot.breathtestfit.md).
See
[`read_any_breathtest`](https://dmenne.github.io/breathtestcore/reference/read_any_breathtest.md)
for an example.

## Usage

``` r
null_fit(data, ...)
```

## Arguments

- data:

  Data frame or tibble as created by
  [`cleanup_data`](https://dmenne.github.io/breathtestcore/reference/cleanup_data.md),
  with mandatory columns `patient_id, group, minute` and `pdr`.

- ...:

  Not used

## Value

A list of classes `breathtestnullfit, breathtestfit` with element `data`
which contains the unmodified data.
