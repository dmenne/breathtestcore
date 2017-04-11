#' @title Mixed-model nlme fit to 13C Breath Data
#' @description Fits exponential beta curves to 13C breath test series data using
#' a mixed-model population approach. See 
#' \url{https://menne-biomed.de/blog/breath-test-stan} for a comparision between
#' single curve, mixed-model population and Bayesian methods.
#' 
#' @param data Data frame or tibble as created by \code{\link{cleanup_data}}, 
#' with mandatory columns \code{patient_id, group, minute} and \code{pdr}. 
#' It is recommended to run all data through \code{\link{cleanup_data}} which
#' will insert dummy columns for \code{patient_id} and \code{minute} if the
#' data are distinct, and report an error if not. At least 3 records are required
#' for a population fit, but 10 or more are recommended to obtain a stable result.
#' @param dose Dose of acetate or octanoate. Currently, only one common dose
#' for all records is supported. 
#' @param start Optional start values.
#' @param pnlsTol See \code{\link[nlme]{nlmeControl}} Default = 0.01. When you get the 
#' error message "step halving factor reduced below minimum in PNLS step", try larger
#' values up to 1; carefully check you results if they make sens for pnlsTol > 0.5.
#'
#' @return A list of class "breathtestfit" with elements
#' \itemize{
#'   \item {\code{coef} Estimated parameters as data frame in a key-value format with 
#'    columns \code{patient_id, group, parameter, method} and \code{value}. 
#'    Has an attribute AIC.}
#'    \item {\code{data}  The effectively analyzed data. If density of points
#'    is too high, e.g. with BreathId devices, data are subsampled before fitting.}
#' }
#' @seealso Base methods \code{coef, plot, print}; methods from package
#'  \code{broom: tidy, augment}.
#' @importFrom stats coef
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom nlme nlme nlmeControl fixef 
#' @importFrom stats AIC
#' @examples 
#' d = simulate_breathtest_data(n_records = 3, noise = 0.2, seed = 4711)
#' data = cleanup_data(d$data)
#' cf = nlme_fit(data)$coef
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
nlme_fit = function(data, dose = 100, 
                   start = list(m = 30, k = 1 / 100, beta = 2),
                   pnlsTol = 0.01) {
  
  # Check if data have been validated by cleanup_data
  assert_that(are_equal(names(data), c("patient_id", "group", "minute", "pdr")))
  if (min(data$minute) <= 0)
    stop("Values at minute = 0 are not permitted. Please shift to minute = 0.001, 
         or better use function <<cleanup_data>>")
  # Avoid notes on CRAN
  group =  value = patient_id = NULL 

  start = c(m = 30, k = 0.01, beta = 1.5)
  # Combine patient and group
  data = data %>% 
    ungroup() %>% 
    mutate(
      pat_group = paste(patient_id, group, sep = "/")
    )
    
  # since it is such a nasty job to pass constant parameter dose to nlsList,
  # fit is done with a real constant, and m, the only affected parameter
  # is renormalized if required.
  # This has kept me busy for more than 11 years now, stumbling over
  # https://stat.ethz.ch/pipermail/r-help/2006-February/087295.html
  bc.nls <- suppressWarnings(nlme::nlsList(
    pdr ~ exp_beta( minute, 100, m, k, beta) | pat_group,
    data = data, start = start
  ))
  bc.nlme = suppressWarnings(try(
    nlme::nlme(
      pdr ~ exp_beta(minute, 100, m, k, beta),
      data = data,
      control = nlme::nlmeControl(pnlsTol = pnlsTol),
      fixed = m + k + beta ~ 1,
      random = (m + k +beta)~1|pat_group,
      start = nlme::fixef(bc.nls)
  ),silent = TRUE))
  if (inherits(bc.nlme, "try-error")) {
    stop("No valid fit mixed-model population fit with this data set.\n",
         as.character(bc.nlme))
  }

  cf = coef(bc.nlme)
  if (dose != 100)
    cf$m = cf$m * dose/100
   
  cf = cf %>% 
    tibble::rownames_to_column(var = "patient_id") %>% 
    mutate(
      group = str_match(patient_id,"/(.*)$")[,2],
      patient_id = str_match(patient_id,"^(.*)/")[,2]
    ) %>% 
    na.omit()

  methods = c(
    "exp_beta","exp_beta","exp_beta","bluck_coward","maes_ghoos",
    "maes_ghoos_scint", "bluck_coward","maes_ghoos"
  )
  parameters = c("m", "k", "beta", "t50", "t50", "t50", "tlag", "tlag")
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
  attr(cf, "AIC") = AIC(bc.nlme)
  ret = list(coef = cf, data = data)
  class(ret) = "breathtestfit"
  ret
}

#' @export
AIC.breathtestfit = function(object, ..., k){
  return(attr(object$coef, "AIC"))
}


