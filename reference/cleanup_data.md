# Transforms 13C breath data into a clean format for fitting

Accepts various data formats of ungrouped or grouped 13C breath test
time series, and transforms these into a data frame that can be used by
all fitting functions, e.g.
[`nls_fit`](https://dmenne.github.io/breathtestcore/reference/nls_fit.md).
If in doubt, pass data frame through `cleanup_data` before forwarding it
to a fitting function. If the function cannot repair the format, it
gives better error messages than the `xxx_fit` functions.

## Usage

``` r
cleanup_data(data, ...)
```

## Arguments

- data:

  - A data frame, array or tibble with at least two numeric columns with
    optional names `minute` and `pdr` to fit a single 13C record.

  - A data frame or tibble with three columns named `patient_id`,
    `minute` and `pdr`.

  - A matrix that can be converted to one of the above.

  - A list of data frames/tibbles that are concatenated. When the list
    has named elements, the names are converted to group labels. When
    the list elements are not named, group name `A` is used for all
    items.

  - A structure of class
    [`breathtest_data`](https://dmenne.github.io/breathtestcore/reference/breathtest_data.md),
    as imported from a file with
    [`read_any_breathtest`](https://dmenne.github.io/breathtestcore/reference/read_any_breathtest.md)

  - A list of class `breathtest_data_list` as generated from read
    function such as
    [`read_breathid_xml`](https://dmenne.github.io/breathtestcore/reference/read_breathid_xml.md)

- ...:

  optional.

  use_filename_as_patient_id

  :   Always use filename instead of patient name. Use this when patient
      id are not unique.

## Value

A tibble with 4 columns. Column `patient_id` is created with a dummy
entry of `pat_a` if no patient_id was present in the input data set. A
column `group` is required in the input data if the patients are from
different treatment groups or within-subject repeats, e.g. in crossover
design. A dummy group name "A" is added if no group column was available
in the input data set. If `group` is present, this is a hint to the
analysis functions to do post-hoc breakdown or use it as a grouping
variable in population-based methods. A patient can have records in
multiple groups, for example in a cross-over designs.

Columns `minute` and `pdr` are the same as given on input, but negative
minute values are removed, and an entry at 0 minutes is shifted to 0.01
minutes because most fit methods cannot handle the singularity at t=0.

An error is raised if dummy columns `patient_id` and `group` cannot be
added in a unique way, i.e. when multiple values for a given minute
cannot be disambiguated.

Comments are persistent; multiple comments are concatenated with newline
separators.

## Examples

``` r
options(digits = 4)
# Full manual
minute = seq(0,30, by = 10)
data1 = data.frame(minute, 
   pdr = exp_beta(minute, dose = 100, m = 30,  k = 0.01, beta = 2))
# Two columns with data at t = 0
data1
#>   minute    pdr
#> 1      0  0.000
#> 2     10  5.166
#> 3     20  8.905
#> 4     30 11.520
# Four columns with data at t = 0.01
cleanup_data(data1)
#>   patient_id group minute    pdr
#> 1      pat_a     A   0.01  0.000
#> 2      pat_a     A  10.00  5.166
#> 3      pat_a     A  20.00  8.905
#> 4      pat_a     A  30.00 11.520

# Results from simulate_breathtest_data can be passed directly to cleanup_data
cleanup_data(simulate_breathtest_data(3))
#> # A tibble: 33 × 4
#>    patient_id group minute   pdr
#>    <chr>      <chr>  <dbl> <dbl>
#>  1 rec_01     A          5     3
#>  2 rec_01     A         20     5
#>  3 rec_01     A         35     9
#>  4 rec_01     A         50    10
#>  5 rec_01     A         65    12
#>  6 rec_01     A         80    12
#>  7 rec_01     A         95    12
#>  8 rec_01     A        110    11
#>  9 rec_01     A        125    11
#> 10 rec_01     A        140    11
#> # ℹ 23 more rows
# .. which implicitly does
cleanup_data(simulate_breathtest_data(3)$data)
#> # A tibble: 33 × 4
#>    patient_id group minute   pdr
#>    <chr>      <chr>  <dbl> <dbl>
#>  1 rec_01     A          5    11
#>  2 rec_01     A         20    22
#>  3 rec_01     A         35    27
#>  4 rec_01     A         50    29
#>  5 rec_01     A         65    29
#>  6 rec_01     A         80    25
#>  7 rec_01     A         95    21
#>  8 rec_01     A        110    21
#>  9 rec_01     A        125    18
#> 10 rec_01     A        140    14
#> # ℹ 23 more rows

# Use simulated data
data2 = list(
  Z = simulate_breathtest_data(seed = 10)$data,
  Y = simulate_breathtest_data(seed = 11)$data)
d = cleanup_data(data2)
str(d)
#> tibble [220 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ patient_id: chr [1:220] "rec_01" "rec_01" "rec_01" "rec_01" ...
#>  $ group     : chr [1:220] "Z" "Z" "Z" "Z" ...
#>  $ minute    : num [1:220] 5 20 35 50 65 80 95 110 125 140 ...
#>  $ pdr       : num [1:220] 0 9 16 18 18 18 19 15 15 12 ...
#>  - attr(*, "comment")= chr "10 records,  m = 40, k =  0.010, beta =  2.0, cov-matrix, \n Gaussian noise amplitude = 1, 0% missing"
unique(d$patient_id)
#>  [1] "rec_01" "rec_02" "rec_03" "rec_04" "rec_05" "rec_06" "rec_07" "rec_08"
#>  [9] "rec_09" "rec_10"
unique(d$group)
#> [1] "Z" "Y"
# "Z" "Y"

# Mix multiple input formats
f1 = btcore_file("350_20043_0_GER.txt")
f2 = btcore_file("IrisMulti.TXT")
f3 = btcore_file("IrisCSV.TXT")
# With a named list, the name is used as a group parameter
data = list(A = read_breathid(f1), B = read_iris(f2), C = read_iris_csv(f3)) 
d = cleanup_data(data)
str(d)
#> tibble [115 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ patient_id: chr [1:115] "350_20043_0_GER" "350_20043_0_GER" "350_20043_0_GER" "350_20043_0_GER" ...
#>  $ group     : chr [1:115] "A" "A" "A" "A" ...
#>  $ minute    : num [1:115] 0.01 0.5 1.6 6.4 8.9 11.3 13.7 16 18.5 23.3 ...
#>  $ pdr       : num [1:115] 0 -0.1 0.4 0.3 1.7 3.1 3.5 3.9 4.7 5.7 ...
unique(d$patient_id)
#> [1] "350_20043_0_GER" "1871960"         "123456"         
# "350_20043_0_GER" "1871960"         "123456"
# File name is used as patient name if none is available
unique(d$group)
#> [1] "A" "B" "C"
# "A" "B" "C"
```
