#' @title Reads breathtest data in Excel format
#'
#' @description Can read several formats of data sets in Excel, from
#' 2 (\code{minute, pdr or dob} for 1 record) to 4 columns (\code{patient_id, 
#' group, minute, pdr or dob}). Conversion from dob to pdf is done for 
#' assuming 180 cm height and 75 kg weight.
#' See the example below with several sheets for supported formats
#' 
#' @param filename Name of Excel-file to be read
#' @param sheet Name or number of Excel file to be read. When used with 
#' \code{\link{read_any_breathtest}}, the first sheet is always read. You must 
#' call  \code{read_breathtest_excel} explicitly to read other files, as shown
#' in the example below.
#'
#' @return Different from the other readXXX function, this returns a list
#' with a data frame, not a structure of \code{\link{breathtest_data}}. 
#' Pass result through \code{\link{cleanup_data}} to make it compatible with 
#' other formats. 
#' 
#' @examples
#' filename = btcore_file("ExcelSamples.xlsx")
#' sheets = readxl::excel_sheets(filename)
#' # First 4 lines of each sheet
#' for (sheet in sheets) {
#'   cat("\nSheet ", sheet,"\n")
#'   ex = readxl::read_excel(filename, sheet = sheet, n_max = 4)
#'   print(ex)
#' }  
#' # To get consistently formatted data from a sheet
#' bt_data = read_breathtest_excel(filename, sheets[6])
#' # 3 columns
#' str(bt_data)
#' bt_cleaned = cleanup_data(bt_data)
#' # 4 columns standard format
#' str(bt_cleaned)
#' @importFrom readxl read_excel
#' @export
read_breathtest_excel = function(filename, sheet = 1) {
  d = read_excel(filename, sheet = sheet, trim_ws = TRUE)
  n_col = ncol(d)
  if (n_col < 2 || n_col > 4)
    stop("File ", basename(filename), " has ", n_col, 
         " column(s) '", paste(names(d), collapse = ", "),
         "'. Expected 2, 3 or 4 columns.")
  # Replace dob 
  if (names(d)[n_col] == "dob") {
    d$dob = dob_to_pdr(d$dob) # uses standard assumptions of 75 kg/180cm
    names(d)[n_col] = "pdr"
  }
  expect_names = list(
    "4" = c("patient_id", "group", "minute", "pdr"),
    "3" = c("patient_id", "minute", "pdr"),
    "2" = c("minute", "pdr"))
  expect_name = expect_names[[as.character(n_col)]]
  if ( all.equal(expect_name, names(d)) != TRUE)  
    stop("With ", n_col, " columns, column names should be ", 
         paste(expect_name, collapse = ", "),
         ", but is ", paste(names(d), collapse = ", "))
  list(d)
}

