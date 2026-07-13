# Read 13C data from IRIS/Wagner Analysen in CSV Format

Reads 13C data from IRIS/Wagner Analysen in CSV Format The CSV files
start as follows:


    "Name","Vorname","Test","Identifikation"

This format does not have information about the substrate (acetate,
octanoate), the dose and body weight and height. The following defaults
are used: `substrate = acetate, dose = 100, weight = 75, height = 180`.

## Usage

``` r
read_iris_csv(filename = NULL, text = NULL)
```

## Arguments

- filename:

  Name of IRIS/Wagner file in CSV format

- text:

  alternatively, text can be given as string

## Value

List of class `breath_test_data` with
`file name, patient name, patient first name, test, identifikation`, and
data frame `data` with `time` and `dob`

## Examples

``` r
filename = btcore_file("IrisCSV.TXT")
cat(readLines(filename, n = 3, encoding = "latin1"), sep="\n")
#> "Name","Vorname","Test","Identifikation","Testzeit[min]","DOB [o/oo]","Delta [o/oo]","Std. Abw.[o/oo]","CO2 [%]","Std. Abw.[%]","Atom ppm Excess 13C [ppm]","Datum","Zeit"
#> "Einstein","Albert","GE FEST","123456","0","0","-26.32","4.501891E-02","3.236342","4.746388E-03","0","13.09.2013","13:21"
#> "Einstein","Albert","GE FEST","123456","10","2.02","-24.3","5.617962E-02","2.391013","2.576674E-03","22.27","13.09.2013","13:23"
#
iris_data = read_iris_csv(filename)
str(iris_data)
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
```
