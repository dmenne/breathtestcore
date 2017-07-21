#' @title S3 method to exctract the residual error
#' @description Functions for `nls ànd `nlme` are available; additional functions
#' for Stan-based fits are defined in package `breathteststan`. 
#' @param fit Result of class `breathtestfit
#' @return A numeric value giving the standard deviation of the residuals.
#' @export 
sigma_fit = function(fit) {
  UseMethod("sigma_fit")
} 

#' @export 
sigma_fit.breathtestnlmefit = function(fit){
  fit$nlme_fit$sigma
}

#' @export 
sigma_fit.breathtestnlsfit = function(fit){
  sqrt(var(residuals(fit$nls_fit)))
}

#' @export 
sigma_fit.breathtestnullfit = function(fit){
  0
}

