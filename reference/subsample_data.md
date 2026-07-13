# Decimate densely sampled 13C time series

When data of a record are more closely spaced than `sample_minutes`,
these are spline-subsampled to `sample_minutes`. In the region of the
initial slope, i.e. the initial fifth of the time, the record is sampled
more densely. Too dense sampling leads to non-convergent `nlme` fits and
to long runs with Stan-based fits. The function is used internally by
function `link{nlme_fit}` in package `breathtestcore` and is exported
for use by package `breathteststan`.

## Usage

``` r
subsample_data(data, sample_minutes)
```

## Arguments

- data:

  Data frame with columns `patient_id, group, minute, pdr`.

- sample_minutes:

  Required average density. When points are more closely spaced, data
  are subsampled. No upsampling occurs when data are more sparse.
