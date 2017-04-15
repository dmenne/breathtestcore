#' @title Convert data to class breathtestfit
#' @description Does not fit the data set, but returns a class suitable
#' for plotting raw data only with \code{plot.breathtestfit}.
#'
#' @param data Data frame or tibble as created by \code{\link{cleanup_data}}, 
#' with mandatory columns \code{patient_id, group, minute} and \code{pdr}. 
#' @param ... Not used
#'
#' @return A list of class "breathtestfit" with element \code{data} which 
#' contains the unmodfied data.
#' @export
null_fit = function(data, ...){
  x = list(data = data) 
  class(x) = "breathtestfit"
  x
}

