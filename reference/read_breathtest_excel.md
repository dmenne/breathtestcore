# Reads breathtest data in Excel format

Can read several formats of data sets in Excel, from 2
(`minute, pdr or dob` for 1 record) to 4 columns
(`patient_id, group, minute, pdr or dob`). Conversion from dob to pdf is
done for assuming 180 cm height and 75 kg weight. See the example below
with several sheets for supported formats

## Usage

``` r
read_breathtest_excel(filename, sheet = 1)
```

## Arguments

- filename:

  Name of Excel-file to be read

- sheet:

  Name or number of Excel file to be read. When used with
  [`read_any_breathtest`](https://dmenne.github.io/breathtestcore/reference/read_any_breathtest.md),
  the first sheet is always read. You must call `read_breathtest_excel`
  explicitly to read other worksheets, as shown in the example below.

## Value

Different from the other readXXX function, this returns a list with a
data frame, not a structure of
[`breathtest_data`](https://dmenne.github.io/breathtestcore/reference/breathtest_data.md).
Pass result through
[`cleanup_data`](https://dmenne.github.io/breathtestcore/reference/cleanup_data.md)
to make it compatible with other formats.

## Examples

``` r
filename = btcore_file("ExcelSamples.xlsx")
sheets = readxl::excel_sheets(filename)
# First 4 lines of each sheet
for (sheet in sheets) {
  cat("\nSheet ", sheet,"\n")
  ex = readxl::read_excel(filename, sheet = sheet, n_max = 4)
  print(ex)
}  
#> 
#> Sheet  4col_1group 
#> # A tibble: 4 × 4
#>   patient_id group minute   pdr
#>        <dbl> <chr>  <dbl> <dbl>
#> 1    7951444 A       0.42 0.547
#> 2    7951444 A      11.9  1.64 
#> 3    7951444 A      23.4  3.89 
#> 4    7951444 A      34.9  6.13 
#> 
#> Sheet  4col_2group 
#> # A tibble: 4 × 4
#>   patient_id group         minute   pdr
#>   <chr>      <chr>          <dbl> <dbl>
#> 1 norm_001   liquid_normal     10   2.6
#> 2 norm_001   liquid_normal     20   8.1
#> 3 norm_001   liquid_normal     30  10.7
#> 4 norm_001   liquid_normal     40   9.1
#> 
#> Sheet  4col_2group_spaces 
#> # A tibble: 4 × 4
#>   patient_id group         minute   pdr
#>   <chr>      <chr>          <dbl> <dbl>
#> 1 norm 001   liquid normal     10   2.6
#> 2 norm 001   liquid normal     20   8.1
#> 3 norm 001   liquid normal     30  10.7
#> 4 norm 001   liquid normal     40   9.1
#> 
#> Sheet  4col_2group_dob 
#> # A tibble: 4 × 4
#>   patient_id group         minute   dob
#>   <chr>      <chr>          <dbl> <dbl>
#> 1 norm_001   liquid_normal     10  0.26
#> 2 norm_001   liquid_normal     20  0.81
#> 3 norm_001   liquid_normal     30  1.07
#> 4 norm_001   liquid_normal     40  0.91
#> 
#> Sheet  3col 
#> # A tibble: 4 × 3
#>   patient_id minute   pdr
#>        <dbl>  <dbl> <dbl>
#> 1    7951500   0.42 0.547
#> 2    7951500  11.9  1.64 
#> 3    7951500  23.4  3.89 
#> 4    7951500  34.9  6.13 
#> 
#> Sheet  2col 
#> # A tibble: 4 × 2
#>   minute   pdr
#>    <dbl> <dbl>
#> 1   0.42 0.547
#> 2  11.9  1.64 
#> 3  23.4  3.89 
#> 4  34.9  6.13 
#> 
#> Sheet  bad_header 
#> # A tibble: 4 × 4
#>   `7951444` A     `0.42` `0.54737400595475105`
#>       <dbl> <chr>  <dbl>                 <dbl>
#> 1   7951444 A       11.9                  1.64
#> 2   7951444 A       23.4                  3.89
#> 3   7951444 A       34.9                  6.13
#> 4   7951444 A       46.4                  8.16
#> 
#> Sheet  bad_order 
#> # A tibble: 4 × 4
#>   group minute   pdr patient_id
#>   <chr>  <dbl> <dbl>      <dbl>
#> 1 A       0.42 0.547    7951444
#> 2 A      11.9  1.64     7951444
#> 3 A      23.4  3.89     7951444
#> 4 A      34.9  6.13     7951444
#> 
#> Sheet  bad_columns 
#> # A tibble: 4 × 1
#>   patient_id
#>   <chr>     
#> 1 a         
#> 2 b         
#> 3 c         
#> 4 d         
# To get consistently formatted data from a sheet
bt_data = read_breathtest_excel(filename, sheets[6])
# 3 columns
str(bt_data)
#> List of 1
#>  $ : tibble [22 × 2] (S3: tbl_df/tbl/data.frame)
#>   ..$ minute: num [1:22] 0.42 11.92 23.4 34.89 46.39 ...
#>   ..$ pdr   : num [1:22] 0.547 1.642 3.886 6.131 8.156 ...
bt_cleaned = cleanup_data(bt_data)
# 4 columns standard format
str(bt_cleaned)
#> tibble [22 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ patient_id: chr [1:22] "pat_a" "pat_a" "pat_a" "pat_a" ...
#>  $ group     : chr [1:22] "A" "A" "A" "A" ...
#>  $ minute    : num [1:22] 0.42 11.92 23.4 34.89 46.39 ...
#>  $ pdr       : num [1:22] 0.547 1.642 3.886 6.131 8.156 ...
```
