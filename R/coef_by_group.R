#' @title Tabulates per-group breath test parameters
#' @description Given a fit to 13C breath test curves, computes absolute values and
#' their confidence intervals of parameters, e.g. of the half emptying time \code{t50}.
#' Generic S3 method for class breathtestfit.
#'
#' @param fit Object of class \code{breathtestfit}, for example from 
#' \code{\link{nlme_fit}}, \code{\link{nls_fit}} or \code{\link[breathteststan]{stan_fit}} 
#' @param ... Not used
#'
#' @return A \code{tibble} of class \code{coef_by_group} with columns
#' \describe{
#'   \item{parameter}{Parameter of fit, e.g. \code{beta, k, m, t50}}
#'   \item{method}{Method used to compute parameter. \code{exp_beta} refers to primary
#'   fit parameters \code{beta, k, m}. \code{maes_ghoos} uses the method from 
#'   Maes B D, Ghoos Y F,
#'   Rutgeerts P J, Hiele M I, Geypens B and Vantrappen G 1994 Dig. Dis. Sci. 39 S104-6.
#'   \code{bluck_coward} is the self-correcting method from  Bluck L J C and 
#'   Coward W A 2006}
#'   \item{group}{Grouping parameter of the fit, e.g. \code{patient, normal, liquid, solid}}
#'   \item{estimate}{Parameter estimate}
#'   \item{conf.low, conf.high}{Lower and upper 95% confidence interval of parameter 
#'   estimate.}
#'   \item{diff_group}{Letters a, b, c indicate that parameter would be in mutually 
#'    significantly different groups. Letter combinations like \code{ab} or \code{abc} 
#'    indicated that this parameter is not significantly different from the given 
#'    other groups in a Tukey-corrected pairwise test. }
#' }  
#'
#' @examples
#' library(dplyr)
#' data("usz_13c")
#' data = usz_13c %>%
#'   dplyr::filter( patient_id %in%
#'     c("norm_001", "norm_002", "norm_003", "norm_004", "pat_001", "pat_002","pat_003")) %>%
#'   cleanup_data()
#' fit = nls_fit(data)
#' coef_by_group(fit)
#' \donttest{
#' fit = nlme_fit(data)
#' coef_by_group(fit)
#' }
#' @export
coef_by_group = function(fit, ...) {
  UseMethod("coef_by_group", fit)
}

#' @export
coef_by_group.breathtestfit = function(fit, ...) {
  if (!inherits(fit, "breathtestfit")) {
    stop("Function coef_by_group: parameter 'fit' must inherit from class breathtestfit")
  }
  cf = coef(fit)
  if (is.null(cf)) return(NULL)
  # Special case when there is only one group
  if (length(unique(cf$group)) == 1)
  {
    if (length(unique(cf$patient_id)) == 1) { # Single case
      return(coef_by_group.breathtestfit_1(fit, ...)) 
    } else {
      return(coef_by_group.breathtestfit_2(fit, ...)) 
    }
  }
  # Keep CRAN quite
  . = confint = estimate = lhs = method = parameter = 
    contrast =  conf.high = conf.low = parameter = method = NULL
  sig = as.integer(options("digits"))
  cm = comment(cf)
  
  cf = cf %>%
    mutate( # lme requires factors
      group = as.factor(.$group)
    ) %>%
    group_by(parameter, method) %>%
    do({
      fit_lme = nlme::lme(value~group, random = ~1|patient_id, data = .)
      # Marginal
      K = diag(length(nlme::fixef(fit_lme)))
      rownames(K) = levels(.$group)
      K[,1] = 1
      cld_0 = broom::tidy(
        multcomp::cld(multcomp::glht(fit_lme, linfct =  multcomp::mcp(group = "Tukey"))))
      broom::tidy(confint(multcomp::glht(fit_lme, linfct =  K))) %>%
        left_join(cld_0, by = c("contrast" = "group")) %>%
        rename(group = contrast, diff_group = letters) %>% 
      mutate(
        estimate = signif(estimate, sig),
        conf.low = signif(conf.low, sig),
        conf.high = signif(conf.high, sig)
      )
    }) %>%
    ungroup()
  comment(cf) = cm
  class(cf) = c("coef_by_group", class(cf))
  cf
}

# local function for the case of 1 group/ multiple subjects

coef_by_group.breathtestfit_2 = function(fit, ...) {
  . = parameter = method = NULL # CRAN
  cm = comment(fit$data)
  cf = coef(fit)
  if (is.null(cf)) return(NULL)
  sig = as.integer(options("digits"))
  cf = cf %>%
    group_by(parameter, method) %>% 
  do({
    fit_lme = nlme::lme(value~1, random = ~1|patient_id, data = .)
    ci = nlme::intervals(fit_lme, which = "fixed")$fixed
    tibble(
        group = .$group,
        estimate = signif(ci[1,"est."], sig),
        conf.low = signif(ci[1,"lower"], sig),
        conf.high = signif(ci[1, "upper"], sig),
        diff_group = "a"
      )
  }) %>%
  ungroup()
  comment(cf) = cm
  class(cf) = c("coef_by_group", class(cf))
  cf
}


# local function for the case of 1 group/ 1 subject
coef_by_group.breathtestfit_1 = function(fit, ...) {
  . = estimate = parameter = method = NULL # CRAN
  cm = comment(fit$data)
  sig = as.integer(options("digits"))
  cf = coef(fit) %>% 
    group_by(parameter, method) %>%
    do({
      tibble(group = .$group, estimate = .$value, conf.low = NA, conf.high = NA,
                 diff_group = NA)
    }) %>%
    mutate(
      estimate = signif(estimate, sig)
    ) %>% 
  ungroup()
  comment(cf) = cm
  class(cf) = c("coef_by_group", class(cf))
  cf
}  
