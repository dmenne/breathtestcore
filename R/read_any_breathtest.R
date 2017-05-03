#' @title Read any external files with breathtest data
#' @description Uses \code{\link{breathtest_read_function}} to determine the file type
#' and reads it if it is a valid format. 
#' @param files A single filename, a list or a character vector of filenames.
#' @return A list of \code{\link{breathtest_data}}, even if
#' only one file was passed. The list can be passed to \code{link{cleanup_data}}
#' to extract only 
#' @examples
#' files = c(
#'   system.file("extdata", "IrisCSV.TXT", package = "breathtestcore"),
#'   system.file("extdata", "350_20043_0_GER.txt", package = "breathtestcore"),
#'   system.file("extdata", "IrisMulti.TXT", package = "breathtestcore")
#'  )
#'  bt = read_any_breathtest(files)
#'  str(bt, 1)
#'  # Passing through cleanup_data give a data frame
#'  bt_df = cleanup_data(bt)
#'  # Plot fit with decimated data
#'  plot(nlme_fit(bt_df))
#'  # or, if you want data only, use null_fit()
#'  plot(null_fit(bt_df))
# })
#' @export
read_any_breathtest = function(files){
  files = as.list(files)
  lapply(files, function(file) breathtest_read_function(file)(file))
}


