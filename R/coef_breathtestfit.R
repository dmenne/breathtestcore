#' @title S3 coef for breathtestfit
#' @description Extracts the estimate of the coefficients part from 
#' fitted 13C beta exponential model; same as \code{fit$coef}, but without
#' column \code{stat}. This column always has value \code{"estimate"} for \code{\link{nls_fit}}
#' and \code{\link{nlme_fit}}, but additional statistics such as credible intervals
#' are available for results from \code{\link{stan_fit}}.
#' @param object of class breathtestfit, as returned by nls_fit or nlme_fit
#' @param ... other parameters passed to methods
#' @examples
#' # Generate simulated data
#' data = cleanup_data(simulate_breathtest_data()$data)
#' # Fit with the population method
#' fit = nlme_fit(data)
#' # All coefficients in the long form
#' coef(fit)
#' # Access coefficients directory
#' fit$coef
#' # Can also be use with stan fit (slow!)
#' \dontrun{
#' if (require("breathteststan")) {
#'   fit = breathteststan::stan_fit(data)
#'   coef(fit)
#'   # We get quantiles here
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
