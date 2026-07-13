# Snoop method to read breath test file

Reads the first line of a file, and returns the best matching function
to read the breath test data in it. To automatically read the file with
the inferred file type, use
[`read_any_breathtest`](https://dmenne.github.io/breathtestcore/reference/read_any_breathtest.md).
For Excel files, only the first sheet is read.

## Usage

``` r
breathtest_read_function(filename = NULL, text = NULL)
```

## Arguments

- filename:

  breath test data file from Iris/Wagner (extended or CSV), BreathID

- text:

  as alternative to filename, pass the text of the file directly. This
  parameter is not used for Excel files.

## Value

Function to read the file or the text; NULL if no matching function was
found

## Examples

``` r
 file = btcore_file("IrisCSV.TXT")
 # Get function to read this file. Returns \code{\link{read_iris_csv}}.
 read_fun = breathtest_read_function(file)
 str(read_fun(file))
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
 # or, simple (returns a list!)
 str(read_any_breathtest(file), 1 )
#> List of 1
#>  $ :List of 23
#>   ..- attr(*, "class")= chr "breathtest_data"
#>  - attr(*, "class")= chr "breathtest_data_list"
 
```
