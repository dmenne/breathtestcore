#' @title S3 coef and summary for breathtestfit
#' @description Function \code{coef} extracts the estimates such as t50, 
#' tlag, from fitted 13C beta  exponential models. The result is the same 
#' as \code{fit$coef}, but without
#' column \code{stat}, which always is \code{"estimate"} for \code{\link{nls_fit}}
#' and \code{\link{nlme_fit}}. 
#' 
#' The \code{summary} method only extracts \code{t50} by the Maes/Ghoos method
#' @param object of class \code{breathtestfit}, as returned by \code{\link{nls_fit}} or 
#' \code{\link{nlme_fit}}
#' @param ... other parameters passed to methods
#' @examples
#' # Generate simulated data
#' data = cleanup_data(simulate_breathtest_data()$data)
#' # Fit with the population method
#' fit = nlme_fit(data)
#' # All coefficients in the long form
#' coef(fit)
#' # Access coefficients directly
#' fit$coef
#' # Only t50 by Maes/Ghoos
#' # Can also be used with stan fit (slow!)
#' \dontrun{
#' if (require("breathteststan")) {
#'   fit = breathteststan::stan_fit(data)
#'   coef(fit)
#'   # We get quantiles here, not only estimates
#'   unique(fit$coef$stat)
#' }
#' }
#' @export
coef.breathtestfit = function(object, ...){
  stat = NULL # CRAN
  if (is.null(object$coef)) return(NULL)    
  object$coef %>% 
    filter(stat == "estimate") %>% 
    select(-stat)   
}

#' @export
summary.breathtestfit = function(object, ...){
  stat = parameter = method = NULL # CRAN
  if (is.null(object$coef)) return(NULL)    
  object$coef %>% 
    filter(stat == "estimate" && parameter == "t50" && method == "maes_ghoos" ) %>% 
    select(-stat, -parameter, -method)   
}
