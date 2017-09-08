#' @title Read any external files with breathtest data
#' @description Uses \code{\link{breathtest_read_function}} to determine the file type
#' and reads it if it has a valid format. 
#' @param files A single filename, a list or a character vector of filenames.
#' @return A list of \code{\link{breathtest_data}}, even if
#' only one file was passed. The list can be passed to \code{\link{cleanup_data}}
#' to extract one concatenated data frame for processing with \code{\link{nls_fit}},  
#' \code{\link{nlme_fit}}, \code{\link{null_fit}} (no processing) or
#' \code{stan_fit} in separate package \code{breathteststan}.  
#' @examples
#' files = c(
#'   btcore_file("IrisCSV.TXT"),
#'   btcore_file("350_20043_0_GER.txt"),
#'   btcore_file("IrisMulti.TXT"),
#'   btcore_file("NewBreathID_multiple.xml")  
#'  )
#'  bt = read_any_breathtest(files)
#'  str(bt, 1)
#'  # Passing through cleanup_data gives a data frame/tibble
#'  bt_df = cleanup_data(bt)
#'  str(bt_df)
#'  # If you want data only, use null_fit()
#'  plot(null_fit(bt_df))
#'  # Plot population fit with decimated data
#'  plot(nlme_fit(bt_df))
# })
#' @importFrom purrr modify_if flatten
#' @export
read_any_breathtest = function(files){
  # https://stackoverflow.com/questions/46097093/partially-unnest-a-list
  files = as.list(files)
  ret = 
    lapply(files, function(file) breathtest_read_function(file)(file)) %>% 
    modify_if(function(x) is(x, "breathtest_data"), list ) %>% 
    flatten() 
  class(ret) = "breathtest_data_list"
  ret
}
  


