#' @title Transforms 13C breath data into a clean format for fitting
#' @description Accepts various data formats of ungrouped or grouped 13C breath 
#' test time series, and transforms these into a data frame that can be used by
#' all fitting functions. 
#' @param data A data frame or tibble with at least two numeric columns with 
#' names \code{minute} and \code{pdr} which can be used to fit a single 13C record; 
#' or a list of data frames/tibbles that are concatenated.
#'
#' @return A tibble with 2, 3 or 4 columns. patient_id and group are coerced to 
#' character.
#' @export 
cleanup_data = function(data) {
  UseMethod("cleanup_data")
} 


#' @export
cleanup_data.data.frame = function(data){
  nc = ncol(data)
  assert_that(nc >= 2)
  # When there are only two column, assume they are minute and time
  # implying that it is only one record
  if (nc == 2) {
    names(data) = c("minute", "pdr")
    # when there are multiple versions of the same time,
    # suspect multiple records
    if (max(table(data$minute)) > 1)
      stop("Only two columns, and multiple PDR values for given time. Forgot column pat_id?")
    return(data)
  }
  # When there are three or four columns, they must be named correctly
  if (nc == 3) {
    if (!all(names(data) == c("patient_id", "minute", "pdr"))) {
      stop("Three columns must be named patient_id, minute, pdr")
    }
  }
  # We do not use factors for easier combination of records
  data$patient_id = as.character(data$patient_id)  
  if (nc == 4) {
    if (!all(names(data) == c("patient_id", "group", "minute", "pdr"))) {
      stop("Four columns must be named patient_id, group, minute, pdr")
    }
    data$group = as.character(data$group)  
  }
  # Check if columns are numeric
  if (!(is.numeric(data$minute) && (is.numeric(data$pdr))))
    stop("Columns minute and pdr must be numeric")
  
  return(data)
}

#' @export 
cleanup_data.list = function(data){
  purrr::map_df(data, function(x){rbind(cleanup_data(x))})
}
  

#library(assertthat)
#library(breathtestcore)
#library(testthat)
#library(purrr)

