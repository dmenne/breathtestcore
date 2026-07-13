# Path to example breath test data file

Path to example breath test data file

## Usage

``` r
btcore_file(filename = NULL, full.names = FALSE)
```

## Arguments

- filename:

  example file in `extdata` directory without path. Case sensitive on
  Unix. When filename is missing, returns the names of the available
  sample files.

- full.names:

  When `filename` is NULL, return full path names

## Value

full filename to example file to use in read_xxx

## Examples

``` r
  head(btcore_file())
#> [1] "350_20023_0_GERWithNan.txt"       "350_20043_0_GER.txt"             
#> [3] "350_20043_0_GERBadHeader.txt"     "350_20043_0_GERDuplicateTime.txt"
#> [5] "350_20043_0_GERNoData.txt"        "350_20043_0_GERNoT50.txt"        
  filename = btcore_file("IrisMulti.TXT")
  data = read_iris(filename)
```
