#' @title Single curve fit with nls to 13C breath test data
#' @description Fits individual exponential beta curves to 13C breath test time series
#'
#' @param data Data frame or tibble as created by \code{\link{cleanup_data}}, 
#' with mandatory columns \code{patient_id, group, minute} and \code{pdr}. 
#' It is recommended to run all data through \code{\link{cleanup_data}} which
#' will insert dummy columns for \code{patient_id} and \code{minute} if the
#' data are distinct, and report an error if not.
#' @param dose Dose of acetate or octanoate. Currently, only one common dose
#' for all records is supported. 
#' @param start Optional start values
#' \code{patient_id} and \code{group}.
#'
#' @return A list of class "breathtestfit" with elements
#' \itemize{
#'   \item {\code{coef} Estimated parameters in a key-value format with 
#'    columns \code{patient_id, group, parameter, method} and \code{value}}
#'    \item {\code{data}  The input data; nls_fit does not decimate the 
#'    data.}
#' }
#' @seealso Base methods \code{coef, plot, print}; methods from package
#'  \code{broom: tidy, augment}.
#' @importFrom stats deviance
#' @importFrom stats coef
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom nlme nlsList 
#' @examples 
#' d = simulate_breathtest_data(n_records = 3, noise = 0.2, seed = 4711)
#' data = cleanup_data(d$data)
#' cf = nls_fit(data)$coef
#' # Input parameters from simulation \code{m_in, beta_in, k_in} and estimates from 
#' # beta exponential fit \code{m_out, beta_out, k_out}
#' options(digits = 2)
#' library(dplyr)
#' cf %>% 
#'   filter(grepl("m|k|beta", parameter )) %>% 
#'   select(-method, -group) %>% 
#'   tidyr::spread(parameter, value) %>% 
#'   inner_join(d$record, by = "patient_id") %>% 
#'   select(patient_id, m_in = m.y, m_out = m.x, 
#'          beta_in = beta.y, beta_out = beta.x,
#'          k_in = k.y, k_out = k.x)
#' @export
#'
nls_fit = function(data, dose = 100, 
                   start = list(m = 50, k = 1 / 100, beta = 2)){
  
  # Check if data have been validated by cleanup_data
  assert_that(are_equal(names(data), c("patient_id", "group", "minute", "pdr")))
  if (min(data$minute) <= 0)
    stop("Values at minute = 0 are not permitted. Please shift to minute = 0.001, 
         or better use function <<cleanup_data>>.")
  # Avoid notes on CRAN
  value = patient_id = NULL 
  # since it is such a nasty job to pass constant parameter dose to nlsList,
  # fit is done with a real constant, and m, the only affected parameter
  # is renormalized if required.
  # This has kept me busy for more than 11 years now, stumbling over
  # https://stat.ethz.ch/pipermail/r-help/2006-February/087295.html
  bid.nls = try(suppressWarnings(nlsList(
    pdr ~ exp_beta(minute, 100, m, k, beta)|patient_id/group, data = data, 
    start = start)), silent = TRUE)
  # strip off group and patient_id, add deviance
  cf = coef(bid.nls)
  if (!is.data.frame(cf)){
    stop("No valid fit possible with this data set")
  }
  if (dose != 100)
    cf$m = cf$m * dose/100
   
  dev0 = function(x){
    d = deviance(x)
    if (is.null(d)) return(NA)
    d
  } 
  cf = cf %>% 
    tibble::rownames_to_column(var = "patient_id") %>% 
    mutate(
      deviance = as.vector(purrr::map_dbl(bid.nls, dev0)),
      group = str_match(patient_id,"/(.*)$")[,2],
      patient_id = str_match(patient_id,"^(.*)/")[,2]
    ) %>% 
    na.omit()

  methods = c(
    "exp_beta","exp_beta","exp_beta","exp_beta","bluck_coward","maes_ghoos",
    "maes_ghoos_scint", "bluck_coward","maes_ghoos"
  )
  parameters = c("m", "k", "beta", "deviance", "t50", "t50","t50","tlag","tlag")
  # TODO: replace the for-loop by purring (for elegance, speed is secondars)
  pars = list()
  for (i in 1:nrow(cf))  {
    cf1 = cf[i, , drop = FALSE]
    pars[[i]] = data_frame(
      patient_id = cf1$patient_id,
      group = cf1$group,
      parameter = parameters,
      method = methods,
      value = unlist(
        c(
          cf1$m,
          cf1$k,
          cf1$beta,
          cf1$deviance,
          t50_bluck_coward(cf1),
          t50_maes_ghoos(cf1),
          t50_maes_ghoos_scintigraphy(cf1),
          tlag_bluck_coward(cf1),
          tlag_maes_ghoos(cf1)
        )
      )
    )
  }
  cf = purrr::map_df(pars, rbind )  %>% 
    filter(value != 0) %>% 
    tibble::as_tibble(cf)
  
  ret = list(coef = cf, data = data)
  class(ret) = "breathtestfit"
  ret
}

#' @title S3 coef for breathtestfit
#' @description Extracts the coefficients part from fitted 13C beta exponential
#' model; same as \code{fit$coef}
#' @param object of class breathtestfit, as returned by nls_fit or nlme_fit
#' @param ... other parameters passed to methods
#' @export
coef.breathtestfit = function(object, ...){
 object$coef 
}
