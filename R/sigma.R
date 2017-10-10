#' @title S3 method to extract the fit's residual standard deviation
#' @description Functions for \code{nls} and \code{nlme} are available; 
#' additional functions for Stan-based fits are defined in package \code{breathteststan}. 
#' @param object Result of class \code{breathtestfit}
#' @param ... Not used
#' @return A numeric value giving the standard deviation of the residuals.
#' @importFrom stats residuals var sigma

#' @export 
sigma.breathtestnlmefit = function(object, ...){
  object$nlme_fit$sigma
}

#' @export 
sigma.breathtestnlsfit = function(object, ...){
  sqrt(var(residuals(object$nls_fit)))
}

#' @export 
sigma.breathtestnullfit = function(object, ...){
  0
}

