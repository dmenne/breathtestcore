# Data structure with PDR data and descriptors for breath test records

Generates structure of class `breathtest_data` with required fields and
optional fields. Optional fields by default are NA. This structure is
used internally as an intermediate when reading in external file
formats. All `read_xxx` functions return this structure, or a list of
this structure (e.g.
[`read_breathid_xml`](https://dmenne.github.io/breathtestcore/reference/read_breathid_xml.md)),
and any converter to a new format should do the same to be used with
[`cleanup_data`](https://dmenne.github.io/breathtestcore/reference/cleanup_data.md).
To support a new format with, also update
[`breathtest_read_function`](https://dmenne.github.io/breathtestcore/reference/breathtest_read_function.md)
which returns the most likely function to read the file by reading a few
lines in it.

## Usage

``` r
breathtest_data(
  patient_id,
  name = NA,
  first_name = NA,
  initials = NA,
  dob = NA,
  birth_year = NA,
  gender = NA,
  study = NA,
  pat_study_id = NA,
  file_name,
  device = "generic",
  substrate,
  record_date,
  start_time = record_date,
  end_time = record_date,
  test_no,
  dose = 100,
  height = 180,
  weight = 75,
  t50 = NA,
  gec = NA,
  tlag = NA,
  data = data
)
```

## Arguments

- patient_id:

  required, string or number for unique identification

- name:

  optional

- first_name:

  optional

- initials:

  optional, 2 characters, 1 number

- dob:

  optional date of birth (not to be confused with "delta over baseline")

- birth_year:

  optional

- gender:

  optional `m` or `f`

- study:

  optional name of study; can be used in population fit

- pat_study_id:

  optional; patient number within study\_ does not need to be globally
  unique

- file_name:

  required; file where data were read from, or other unique string\_
  when data are read again, this string is tested and record is skipped
  when same filename is already in database, therefore uniqueness is
  important\_ when some record does not turn up in database after
  repeated reading, check if a record with the same file name is already
  there, and rename the file to avoid collisions\_

- device:

  breath_id or iris; default "generic"

- substrate:

  should contain string "ace" or "oct" or "okt", case insensitive\_ will
  be replaced by "acetate" or "octanoate". If empty, "ocatanoate" is
  assumed.

- record_date:

  required record date\_

- start_time:

  optional

- end_time:

  optional

- test_no:

  required integer; unique test number converted to integer if factor

- dose:

  optional, default 100 mg

- height:

  optional, in cm; when pdr must be calculated, default values are used;
  see
  [`dob_to_pdr`](https://dmenne.github.io/breathtestcore/reference/dob_to_pdr.md)

- weight:

  optional, in kg

- t50:

  optional, only present if device computes this value

- gec:

  optional, only present if device computes this value

- tlag:

  optional, only present if device computes this value

- data:

  data frame with at least 5 rows and columns `minute` or `time` and one
  or both of `dob` or `pdr`. If pdr is missing, and height, weight and
  substrate are given, computes pdr via function
  [`dob_to_pdr`](https://dmenne.github.io/breathtestcore/reference/dob_to_pdr.md).
  When height and weight are missing, defaults 180 cm and 75 kg are used
  instead.

## Examples

``` r
# Read a file with known format
iris_csv_file = btcore_file("IrisCSV.TXT")
iris_csv_data = read_iris_csv(iris_csv_file)
# Note that many filds are NA
str(iris_csv_data)
#> List of 23
#>  $ patient_id  : chr "123456"
#>  $ name        : chr "Einstein"
#>  $ first_name  : chr "Albert"
#>  $ initials    : chr "EA"
#>  $ dob         : logi NA
#>  $ birth_year  : logi NA
#>  $ gender      : logi NA
#>  $ study       : chr "GE FEST"
#>  $ pat_study_id: logi NA
#>  $ file_name   : chr "IrisCSV.TXT"
#>  $ device      : chr "Iris"
#>  $ substrate   : chr "acetate"
#>  $ record_date : chr "2013-09-13"
#>  $ start_time  : chr "2013-09-13"
#>  $ end_time    : chr "2013-09-13"
#>  $ test_no     : int 9999
#>  $ dose        : num 100
#>  $ height      : num 180
#>  $ weight      : num 75
#>  $ t50         : logi NA
#>  $ gec         : logi NA
#>  $ tlag        : logi NA
#>  $ data        :'data.frame':    14 obs. of  3 variables:
#>   ..$ minute: num [1:14] 0 10 20 45 60 75 90 105 120 140 ...
#>   ..$ dob   : num [1:14] 0 2.02 5.22 8.9 9.48 ...
#>   ..$ pdr   : num [1:14] 0 1.11 2.86 4.87 5.19 ...
#>  - attr(*, "class")= chr "breathtest_data"
# Convert to a format that can be fed to one of the fit functions
iris_df = cleanup_data(iris_csv_data)
# Individual curve fit
coef(nls_fit(iris_df)) 
#> # A tibble: 9 × 5
#>   patient_id group parameter method                      value
#>   <chr>      <chr> <chr>     <chr>                       <dbl>
#> 1 123456     A     m         exp_beta                 31.9    
#> 2 123456     A     k         exp_beta                  0.00451
#> 3 123456     A     beta      exp_beta                  1.93   
#> 4 123456     A     deviance  exp_beta                  0.696  
#> 5 123456     A     t50       bluck_coward             70.7    
#> 6 123456     A     t50       maes_ghoos              266.     
#> 7 123456     A     t50       maes_ghoos_scintigraphy 178.     
#> 8 123456     A     tlag      bluck_coward             -7.53   
#> 9 123456     A     tlag      maes_ghoos              146.     
```
