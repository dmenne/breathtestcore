#' @title Transforms 13C breath data into a clean format for fitting
#' @description Accepts various data formats of ungrouped or grouped 13C breath 
#' test time series, and transforms these into a data frame that can be used by
#' all fitting functions
#' @param data A data frame with at least two numeric columns with names \code{minute} 
#' and \code{pdr} which can be used to fit a single 13C record
#'
#' @return A dat
#' @export
cleanup_data = function(data){
  assert_that(is.data.frame(data))
  nc = ncol(data)
  assert_that(nc >=2)
}

#library(assertthat)
#library(breathtestcore)


#data = simulate_breathtest_data(n_records = 1, seed = 4711)$data
#a = cleanup_data(data)
