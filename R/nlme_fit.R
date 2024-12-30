#' @title Mixed-model nlme fit to 13C Breath Data
#' @description Fits exponential beta curves to 13C breath test series data using
#' a mixed-model population approach. See
#' \url{https://menne-biomed.de/blog/breath-test-stan/} for a comparison between
#' single curve, mixed-model population and Bayesian methods.
#'
#' @param data Data frame or tibble as created by \code{\link{cleanup_data}},
#' with mandatory columns \code{patient_id, group, minute} and \code{pdr}.
#' It is recommended to run all data through \code{\link{cleanup_data}} to
#' insert dummy columns for \code{patient_id} and \code{group} if the
#' data are distinct, and report an error if not. At least 2 records are required
#' for a population fit, but 10 or more are recommended to obtain a stable result.
#' @param dose Dose of acetate or octanoate. Currently, only one common dose
#' for all records is supported. The dose only affects parameter \code{m} of the 
#' fit; all important t50-parameters are unaffected by the dose.
#' @param start Optional start values. In most case, the default values are good
#' enough to achieve convergence, but slightly different values for \code{beta}
#' (between 1 and 2.5) can save a non-convergent run.
#' 
#' @param sample_minutes When the mean sampling interval is < \code{sampleMinutes}, 
#' data are subsampled using a spline algorithm by function \code{\link{subsample_data}}.
#' See the graphical output of \code{\link{plot.breathtestfit}} for an example where
#' too densely sampled data of one patients were subsampled for the fit.
#'
#' @return A list of class ("breathtestnlmefit" "breathtestfit") with elements
#' \describe{
#'   \item{coef}{Estimated parameters in a key-value format with 
#'    columns \code{patient_id, group, parameter, stat, method} and \code{value}.
#'    Parameter \code{stat} currently always has value \code{"estimate"}. 
#'    Confidence intervals will be added later, so do not take for granted that 
#'    all parameters are estimates.  Has an attribute AIC which can be retrieved by
#'    the S3-function \code{AIC}.}
#'    \item{data}{The data effectively fitted. If points are to closely sampled
#'    in the input, e.g. with BreathId devices, data are subsampled before fitting.}
#' }
#' @seealso Base methods \code{coef, plot, print}; methods from package
#'  \code{broom: tidy, augment}.
#' @examples
#' d = simulate_breathtest_data(n_records = 3, noise = 0.7, seed = 4712)
#' data = cleanup_data(d$data)
#' fit = nlme_fit(data)
#' plot(fit) # calls plot.breathtestfit
#' options(digits = 3)
#' library(dplyr)
#' cf = coef(fit)
#' # The coefficients are in long key-value format
#' cf
#' # AIC can be extracted
#' AIC(fit)
#' # Reformat the coefficients to wide format and compare 
#' # with the expected coefficients from the simulation 
#' # in d$record.
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
                   sample_minutes = 15) {

  # Check if data have been validated by cleanup_data
  assert_that(are_equal(names(data), c("patient_id", "group", "minute", "pdr")))
  # Avoid notes on CRAN
  group =  value = patient_id = pat_group = . = NULL
  
  # Check and decimate; create pat_group
  data  = subsample_data(data, sample_minutes)
  # since it is such a nasty job to pass constant parameter dose to nlsList,
  # fit is done with a real constant, and m, the only affected parameter
  # is renormalized if required.
  # This has kept me busy for more than 11 years now, stumbling over
  # https://stat.ethz.ch/pipermail/r-help/2006-February/087295.html
  bc.nls <- suppressWarnings(nlme::nlsList(
    pdr ~ breathtestcore::exp_beta( minute, 100, m, k, beta) | pat_group,
    data = data, start = start
  ))
  
#  capture.output(bc.nls, file = stderr())
  
  success = FALSE
  pnlsTol = 0.01
  while (!success && pnlsTol < 0.5) {
    bc_nlme = suppressWarnings(try(
      nlme::nlme(
        pdr ~ breathtestcore::exp_beta(minute, 100, m, k, beta),
        data = data,
        control = nlme::nlmeControl(msMaxIter = 20,
                                    pnlsTol = pnlsTol, maxIter = 15),
        fixed = m + k + beta ~ 1,
        random = pdDiag(m + k +beta)~1,
        groups = ~pat_group,
        start = nlme::fixef(bc.nls)
      ), silent = TRUE))
    success = !inherits(bc_nlme, "try-error")
    if (!success) {
#      capture.output(print(summary(bc_nlme)), file = stderr())             
#      cat("\nnlme fit failed with pnlsTol=", pnlsTol, "\n", file = stderr())
      pnlsTol = pnlsTol * 5
    }
  }
  # Return data only if not successful
  if (!success) {
    data = data %>% select(-pat_group) # only used locally
    comment(data) = paste(comment(data),  "--", "no successful fit with nlme")
    #cat(comment(data),"\n", file = stderr())
    ret = list(data = data)
    class(ret) = "breathtestfit"
    return(ret)
  }

  cf = coef(bc_nlme)
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
    "maes_ghoos_scintigraphy", "bluck_coward","maes_ghoos"
  )
  parameters = c("m", "k", "beta", "t50", "t50", "t50", "tlag", "tlag")
  # TODO: replace the for-loop by purring (for elegance, speed is secondars)
  pars = list()
  for (i in seq_len(nrow(cf)))  {
    cf1 = cf[i, , drop = FALSE]
    pars[[i]] = tibble(
      patient_id = cf1$patient_id,
      group = cf1$group,
      parameter = parameters,
      method = methods,
      stat = "estimate",
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
    tibble::as_tibble(.name_repair = "minimal")
  attr(cf, "AIC") = AIC(bc_nlme)
  data = data %>% select(-pat_group) # only used locally
  ret = list(coef = cf, data = data, nlme_fit = bc_nlme)
  class(ret) = c("breathtestnlmefit", "breathtestfit")
  ret
}

#' @title S3 AIC method for breathtestnlmefit
#' @description Extract AIC from a model fitted with \code{\link{nlme_fit}}
#' @param object of class breathtestnlmefit
#' @param ... not used
#' @export
AIC.breathtestnlmefit = function(object, ...){
  return(attr(object$coef, "AIC"))
}


