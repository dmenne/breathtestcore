#' @title Bayesian Stan fit to 13C Breath Data
#' @description Fits exponential beta curves to 13C breath test series data using
#' Bayesian Stan methods. See
#' \url{https://menne-biomed.de/blog/breath-test-stan} for a comparision between
#' single curve, mixed-model population and Bayesian methods.
#'
#' @param data Data frame or tibble as created by \code{\link{cleanup_data}},
#' with mandatory columns \code{patient_id, group, minute} and \code{pdr}.
#' It is recommended to run all data through \code{\link{cleanup_data}} which
#' will insert dummy columns for \code{patient_id} and \code{minute} if the
#' data are distinct, and report an error if not. Since the Bayesian method
#' is stabilized by priors, it is possible to fit single curves.
#' @param dose Dose of acetate or octanoate. Currently, only one common dose
#' for all records is supported.
#' @param sample_minutes If mean sampling interval is < sampleMinutes, data are subsampled
#' using a spline algorithm
#' @param student_df When student_df < 10, the student distribution is used to 
#' model the residuals. Recommended values to model typical outliers are from 3 to 6.
#' When student_df >= 10, the normal distribution is used
#' @param chains Number of chains for Stan
#' @param iter Number of iterations for each Stan chain
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
#' @examples
#' d = simulate_breathtest_data() # default 10 records
#' data = cleanup_data(d$data)
#' fit = stan_fit(data)
#' plot(fit) # calls plot.breathtestfit
#' options(digits = 2)
#' library(dplyr)
#' cf = coef(fit)
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
stan_fit = function(data, dose = 100, sample_minutes = 15, student_df = 10, 
                    chains = 2, iter = 1000) {

  # Avoid notes on CRAN
  group =  value = patient_id = pat_group = pat_group_i = NULL
  data = subsample_data(data, sample_minutes)

  # Integer index of records
  data$pat_group_i =  as.integer(as.factor(data$pat_group))
  n_record = max(data$pat_group_i)
  data_list = list(
    n = nrow(data),
    n_record = n_record,
    dose = 100,
    student_df = student_df,
    pat_group_i = data$pat_group_i,
    minute = data$minute,
    pdr = data$pdr)
  
  init = rep(list(list(
    m_raw = rnorm(n_record,0,.1),
    mu_m = rnorm(1,40,2),
    sigma_m = abs(rnorm(1,2,.1)),
    
    k_raw = rnorm(n_record, 0,.1),
    mu_k = rlnorm(1, -6,.1),
    sigma_k = abs(rnorm(1,0,.001)),
    
    beta_raw = rnorm(n_record, 0, .1),
    mu_beta = rnorm(1, 2, 0.1),
    sigma_beta = abs(rnorm(1,.1,.1)),
    sigma = abs(rnorm(1,1,.1))
  )),chains)

  mod = stanmodels[["breath_test_1"]]
  #options(mc.cores = parallel::detectCores()/2)
  capture.output({
    fit = suppressWarnings(sampling(mod, data = data_list, init_r = init, 
                                    iter =  iter, chains = chains))
  })
  
  
  # get posterior means 
  cf = get_posterior_mean(fit, pars = c( "beta", "k", "m"))[,chains + 1]
  cf = do.call(cbind, split(cf,  str_extract(names(cf),"[a-z]*")))
  cf = as.data.frame(sapply(as.data.frame(cf), signif, 3))
  cf$pat_group_i = 1:n_record
  cf = cf  %>%  
    left_join(unique(data[,c("pat_group_i", "pat_group", "patient_id", "group")]), 
              by = "pat_group_i") 
  
  methods = c(
    "exp_beta","exp_beta","exp_beta","bluck_coward","maes_ghoos",
    "maes_ghoos_scint", "bluck_coward","maes_ghoos"
  )
  parameters = c("m", "k", "beta", "t50", "t50","t50","tlag","tlag")
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
  
  
  data = data %>% select(-pat_group, -pat_group_i) # only used locally
  ret = list(coef = cf, data = data)
  class(ret) = "breathtestfit"
  ret
}

