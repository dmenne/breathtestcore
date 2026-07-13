# Read 13C data from IRIS/Wagner Analysen

Reads composite files with 13C data from IRIS/Wagner Analysen. The
composite files start as follows:


    "Testergebnis"
    "Nummer","1330"
    "Datum","10.10.2013"
    "Testart"

## Usage

``` r
read_iris(filename = NULL, text = NULL)
```

## Arguments

- filename:

  name of IRIS/Wagner file in composite format

- text:

  alternatively, text can be given as string

## Value

List of class
[`breathtest_data`](https://dmenne.github.io/breathtestcore/reference/breathtest_data.md)
with
`file_name, patient_name, patient_first_name, test, identifikation`, and
data frame `data` with `time` and `dob`

## Examples

``` r
filename = btcore_file("IrisMulti.TXT")
cat(readLines(filename, n = 10, encoding = "latin1"), sep="\n")
#> "Testergebnis"
#> "Nummer","22"
#> "Datum","12.06.2009"
#> "Testart"
#> "Name","Magenentleerung fest"
#> "Abkürzung","GE FEST"
#> "Substrat","Natriumoktanoat"
#> "Molekulargewicht [g/mol]","0"
#> "13C-Atome/Molekül","1"
#> "Anreicherung [%]","0"
#
iris_data = read_iris(filename)
str(iris_data)
#> List of 23
#>  $ patient_id  : chr "1871960"
#>  $ name        : chr "V"
#>  $ first_name  : chr "S"
#>  $ initials    : chr "VS"
#>  $ dob         : logi NA
#>  $ birth_year  : logi NA
#>  $ gender      : logi NA
#>  $ study       : chr "GE FEST"
#>  $ pat_study_id: logi NA
#>  $ file_name   : chr "IrisMulti.TXT"
#>  $ device      : chr "Iris"
#>  $ substrate   : chr "octanoate"
#>  $ record_date : chr "2020-06-12"
#>  $ start_time  : chr "2020-06-12"
#>  $ end_time    : chr "2020-06-12"
#>  $ test_no     : int 22
#>  $ dose        : num 100
#>  $ height      : num 180
#>  $ weight      : num 80
#>  $ t50         : logi NA
#>  $ gec         : logi NA
#>  $ tlag        : logi NA
#>  $ data        :'data.frame':    14 obs. of  3 variables:
#>   ..$ minute: num [1:14] 0 10 20 45 60 75 90 105 120 140 ...
#>   ..$ dob   : num [1:14] 0 -2.46 1.13 1.39 2.91 2.35 3.24 4.27 3.22 3.64 ...
#>   ..$ pdr   : num [1:14] 0 -2.8 1.29 1.58 3.32 ...
#>  - attr(*, "class")= chr "breathtest_data"
```
