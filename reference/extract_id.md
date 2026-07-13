# Extracts an ID from string IRIS CSV file

First tries to extract only digits, separating these by underscore when
there are multiple blocks. If this give a non-valid id, returns the
whole string without spaces and periods, hoping it makes sense. For
internal use, but should be overridden for exotic IDs

## Usage

``` r
extract_id(id)
```

## Arguments

- id:

  One item from column Identifikation, e.g. "KEK-ZH-Nr.2013-1234"

## Examples

``` r
extract_id
#> function (id) 
#> {
#>     id1 = paste(str_match_all(id, "([\\d]+)")[[1]][, 2], collapse = "_")
#>     if (nchar(id1) >= 5) 
#>         return(id1)
#>     tolower(str_replace_all(id, "[\\.\\-\\W]+", "_"))
#> }
#> <bytecode: 0x5587b2ae8558>
#> <environment: namespace:breathtestcore>
```
