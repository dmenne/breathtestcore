# Read BreathID file

Reads 13c data from a BreathID file, and returns a structure of class
`breathtest_data`.

## Usage

``` r
read_breathid(filename = NULL, text = NULL)
```

## Arguments

- filename:

  name of txt-file to be read

- text:

  alternatively, text can be given as string

## Value

Structure of class
[`breathtest_data`](https://dmenne.github.io/breathtestcore/reference/breathtest_data.md)

## Examples

``` r
filename = btcore_file("350_20043_0_GER.txt")
# Show first lines
cat(readLines(filename, n = 10), sep="\n")
#> Test and Patient parameters                  
#> 
#>                  
#> Date           -    12/11/12                 
#> End time       -    08:54                    
#> Start time     -    12:49                    
#> Patient # - 0                    
#> Patient ID   - Franz                     
#> Test No.   -    20043                    
#> Type       -    Octanoic-Acid                    
#
bid = read_breathid(filename)
str(bid)
#> List of 23
#>  $ patient_id  : chr "0"
#>  $ name        : logi NA
#>  $ first_name  : logi NA
#>  $ initials    : logi NA
#>  $ dob         : logi NA
#>  $ birth_year  : logi NA
#>  $ gender      : chr "m"
#>  $ study       : logi NA
#>  $ pat_study_id: logi NA
#>  $ file_name   : chr "350_20043_0_GER.txt"
#>  $ device      : chr "BreathID"
#>  $ substrate   : chr "octanoate"
#>  $ record_date : chr "2012-12-11"
#>  $ start_time  : chr "2012-12-11 08:54"
#>  $ end_time    : chr "2012-12-11 12:49"
#>  $ test_no     : int 20043
#>  $ dose        : num 100
#>  $ height      : num 180
#>  $ weight      : num 75
#>  $ t50         : num 71.2
#>  $ gec         : num 2.99
#>  $ tlag        : num 28.2
#>  $ data        :'data.frame':    87 obs. of  6 variables:
#>   ..$ minute : num [1:87] 0 0.5 1.6 6.4 8.9 11.3 13.7 16 18.5 23.3 ...
#>   ..$ dob    : num [1:87] 0 -0.1 0.4 0.3 1.6 2.9 3.3 3.7 4.5 5.4 ...
#>   ..$ pdr    : num [1:87] 0 -0.1 0.4 0.3 1.7 3.1 3.5 3.9 4.7 5.7 ...
#>   ..$ pdrfit : num [1:87] 0 0.2 0.5 1.8 2.5 3.1 3.7 4.2 4.8 5.7 ...
#>   ..$ cpdr   : num [1:87] 0 0 0 0 0.1 0.2 0.3 0.4 0.6 1 ...
#>   ..$ cpdrfit: num [1:87] 0 0 0 0.1 0.2 0.3 0.4 0.6 0.7 1.1 ...
#>  - attr(*, "class")= chr "breathtest_data"
```
