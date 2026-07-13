# Read new BreathID/Examens XML file

Reads 13c data from an XML BreathID file, and returns a structure of
class `breathtest_data_list`, which is a list with elements of class
`breathtest_data`.

## Usage

``` r
read_breathid_xml(filename = NULL, text = NULL)
```

## Arguments

- filename:

  name of xml-file to be read

- text:

  alternatively, text can be given as string

## Value

List of class `breathtest_data_list` of structures of class
[`breathtest_data`](https://dmenne.github.io/breathtestcore/reference/breathtest_data.md);
an XML file can contain multiple data sets. Errors string for individual
records are returned as attribute "errors".

## Examples

``` r
filename = btcore_file("NewBreathID_01.xml")
# Show first lines
cat(readLines(filename, n = 10), sep="\n")
#> <Tests Device="1402">
#>   <Test Number="12">
#>     <ID>10727002</ID>
#>     <StartTime>24Jul2017 07:42</StartTime>
#>     <EndTime>24Jul2017 12:03</EndTime>
#>     <LastResultCode>1</LastResultCode>
#>     <StoppedByUser>false</StoppedByUser>
#>     <DOBListTimes>1.33,12.79,24.25,35.81,57.90,69.38,80.87,92.33,103.78,115.22,126.68,138.14,149.59,161.09,172.59,184.07,195.55,208.15,219.64,231.10,242.57</DOBListTimes>
#>     <DOBListValues>0.7,1.8,3.7,4.9,7.5,8.6,10.2,11.5,10.4,9.7,10.8,10.7,11.7,11.7,11.4,10.6,8.5,8.4,6.9,7.6,7.1</DOBListValues>
#>   </Test>
bid = read_breathid_xml(filename)
# List with length 1
str(bid, 1)
#> List of 1
#>  $ :List of 23
#>   ..- attr(*, "class")= chr "breathtest_data"
#>  - attr(*, "class")= chr "breathtest_data_list"
#>  - attr(*, "errors")= chr ""
filename = btcore_file("NewBreathID_multiple.xml")
bids = read_breathid_xml(filename)
str(bids, 1) # 3 elements - the others in the file have no data
#> List of 3
#>  $ :List of 23
#>   ..- attr(*, "class")= chr "breathtest_data"
#>  $ :List of 23
#>   ..- attr(*, "class")= chr "breathtest_data"
#>  $ :List of 23
#>   ..- attr(*, "class")= chr "breathtest_data"
#>  - attr(*, "class")= chr "breathtest_data_list"
#>  - attr(*, "errors")= chr "Empty data set in NewBreathID_multiple.xml\nEmpty data set in NewBreathID_multiple.xml\nEmpty data set in NewBr"| __truncated__
# Create hook function to deselect first record
choose_record = function(records) {
  r  = rep(TRUE, length(records))
  r[1] = FALSE
  r
}
options(breathtestcore.choose_record = choose_record)
bids = read_breathid_xml(filename)
str(bids, 1) # 2 elements, first deselected
#> List of 2
#>  $ :List of 23
#>   ..- attr(*, "class")= chr "breathtest_data"
#>  $ :List of 23
#>   ..- attr(*, "class")= chr "breathtest_data"
#>  - attr(*, "class")= chr "breathtest_data_list"
#>  - attr(*, "errors")= chr "Empty data set in NewBreathID_multiple.xml\nEmpty data set in NewBreathID_multiple.xml\nEmpty data set in NewBr"| __truncated__

```
