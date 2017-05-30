#' @title Convert data to class breathtestfit
#' @description Does not change the data set, but returns a class suitable
#' for plotting raw data with \code{\link{plot.breathtestfit}}. 
#' See \code{\link{read_any_breathtest}} for an example.
#'
#' @param data Data frame or tibble as created by \code{\link{cleanup_data}}, 
#' with mandatory columns \code{patient_id, group, minute} and \code{pdr}. 
#' @param ... Not used
#'
#' @return A list of classes \code{breathtestnullfit, breathtestfit} 
#' with element \code{data} which contains the unmodified data.
#' @export
null_fit = function(data, ...){
  x = list(data = data) 
  class(x) = c("breathtestnullfit", "breathtestfit")
  x
}

