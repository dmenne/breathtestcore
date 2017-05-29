#' @title Tabulates per-group breath test parameters
#' @description Given a fit to 13C breath test curves, computes absolute values and
#' their conficence intervals of parameters, e.g. of the half emptying time \code{t50}.
#'
#' @param fit Object of class \code{breathtestfit}, for example from 
#' \code{\link{nlme_fit}}, \code{\link{nls_fit}} or \code{\link[breathteststan]{stan_fit}} 
#'
#' @return A \code{tibble} with columns
#' \describe{
#'   \item{parameter}{Parameter of fit, e.g. \code{beta, k, m, t50}}
#'   \item{method}{Method used to compute parameter. \code{exp_beta} refers to primary
#'   fit parameters \code{beta, k, m}. \code{maes_ghoos} uses the method from 
#'   Maes B D, Ghoos Y F,
#'   Rutgeerts P J, Hiele M I, Geypens B and Vantrappen G 1994 Dig. Dis. Sci. 39 S104-6.
#'   \code{bluck_coward} is the self-correcting method from  Bluck L J C and 
#'   Coward W A 2006 <doi:10.1088/0967-3334/27/3/006>}
#'   \item{group}{Grouping parameter of the fit, e.g. \code{patient, normal, liquid, solid}}
#'   \item{estimate}{Parameter estimate}
#'   \item{conf.low, conf.hight}{Lower and upper 95% confidence interval of parameter 
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
#' @export

coef_by_group = function(fit) {
  if (!inherits(fit, "breathtestfit")) {
    stop("Function coef_by_group: parameter 'fit' must inherit from class breathtestfit")
  }
  # Keep CRAN quite
  . = confint = estimate.x = estimate.y = lhs = method = parameter = NULL
  coef(fit) %>%
    mutate( # lme requires factors
      group = as.factor(.$group)
    ) %>%
    group_by_("parameter", "method") %>%
    do({
      fit_lme = nlme::lme(value~group, random = ~1|patient_id, data = .)
      # Marginal
      K = diag(length(nlme::fixef(fit_lme)))
      rownames(K) = levels(.$group)
      K[,1] = 1
      cld_0 = broom::tidy(
        multcomp::cld(multcomp::glht(fit_lme, linfct =  multcomp::mcp(group = "Tukey"))))
      broom::tidy(confint(multcomp::glht(fit_lme, linfct =  K)))[,-2] %>%
        left_join(cld_0, by = "lhs") %>%
        rename(group = lhs, diff_group = letters)
    }) %>%
    ungroup()
}
