#' @name t50_bluck_coward
#' @title self-corrected t_{50}
#' @description Uses Newton's method to solve the self-corrected Bluck-Coward equation
#' for 1/2 to compute t_50. 
#' See also equation G(n,t) in "Bluck LJC, Jackson S, Vlasakakis G, Mander A (2011)
#' Bayesian hierarchical methods to interpret  the 13C-octanoic acid breath
#' test for gastric emptying. Digestion 83_96-107, page 98.
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' note that in this package, \code{k} is measured in 1/min (e.g. 0.01/min),
#' in publications it is often quoated as 1/h (e.g. 0.6/h).
#' @return time where value is 1/2 of maximum, i.e. t_{50} or t_{1/2} in minutes; 
#' in the above  paper, the parameter is called t_{1/2(in)}.
#' @examples
#' # From table 3 and 4 in Bluck et al.; values for \code{k} and \code{beta} 
#' # (nls, bayesian) are entered and checked against the tabulated values of 
#' # t_{1/2(in)}.
#' # Most errors are small, but there are some outliers; errors in paper table?
#' # Parameters and Bluck et al. results:
#' # table 3 of Bluck et al.
#' cf3 = data.frame(
#'           method = rep(c("nls", "bayesian")),
#'           group = rep(c("lean", "obese"),each=2),
#'           k =    c(0.576,0.606,0.529,0.608),
#'           beta = c(5.24, 5.79, 5.95, 7.54),
#'           t12 =  c(3.67, 3.63, 4.23, 3.99),
#'           t12in = c(2.076, 2.110, 2.422, 2.466),
#'           tlag = c(2.88, 2.88, 3.34, 3.26),
#'           tlagin = c(1.632, 1.724, 1.92, 2.101)
#' )
#' cf3 = dplyr::mutate(cf3,
#'           t50_maes_ghoos = t50_maes_ghoos(cf3),
#'           t50_bluck_coward = t50_bluck_coward(cf3),
#'           tlag_maes_ghoos = tlag_maes_ghoos(cf3),
#'           tlag_bluck_coward = tlag_bluck_coward(cf3),
#'           err_t50_maes_ghoos = round(100*(t50_maes_ghoos-t12)/t12, 2),
#'           err_t50_bluck_coward =
#'             round(100*(t50_bluck_coward-t12in)/t12in, 2),
#'           err_lag_maes = round(100*(tlag_maes_ghoos-tlag)/tlag,2),
#'           err_lag_bluck_coward =
#'             round(100*(tlag_bluck_coward-tlagin)/tlagin,2)
#')
#' cf3
#' # table 4
#' # there are large differences for mj3, both using the bayesian (26%)
#' # and the nls method (16%).  The other data are within the expected limits
#' cf4 = data.frame(
#'           method = rep(c("nls", "bayesian"),each=3),
#'           group = rep(c("mj1",   "mj2",   "mj3")),  
#'           k = c(0.585,  0.437,  0.380,  0.588,  0.418,  0.361),  
#'           beta=c(4.35,  4.08,  4.44,  4.49, 4.30, 4.29), 
#'           t12 = c(3.39, 4.25, 4.82, 3.40, 4.61, 5.09), 
#'           t12in = c(1.77, 2.16, 2.19, 1.81, 2.34, 2.43), 
#'           tlag = c(2.56, 3.17, 3.39, 2.58, 3.40, 3.62), 
#'           tlagin = c(1.30, 1.53, 1.33, 1.35, 1.65, 1.57)
#' )
#' cf4 = dplyr::mutate(cf4,
#'           t50_maes_ghoos = t50_maes_ghoos(cf4),
#'           t50_bluck_coward = t50_bluck_coward(cf4),
#'           tlag_maes_ghoos = tlag_maes_ghoos(cf4),
#'           tlag_bluck_coward = tlag_bluck_coward(cf4),
#'           err_t50_maes_ghoos = unlist(round(100*(t50_maes_ghoos-t12)/t12)),
#'           err_t50_bluck_coward =
#'             round(100*(t50_bluck_coward-t12in)/t12in,2),
#'           err_lag_maes = round(100*(tlag_maes_ghoos-tlag)/tlag,2),
#'           err_lag_bluck_coward =
#'             round(100*(tlag_bluck_coward-tlagin)/tlagin,2)
#')
#' cf4
#'
#' @seealso \code{\link{exp_beta}}
#' @export
t50_bluck_coward = function(cf) {
  f = function(t,cf0)
    cum_exp_beta(t,1,cf0) - 0.5
  g = function(cf0) {
    ret = try(stats::uniroot(f, interval = c(1,1000),cf0)$root, silent = TRUE)
    if (inherits(ret, "try-error"))
      ret = NA 
    ret
  }
  if (inherits(cf, "numeric"))
    round(g(cf),3)
  else
    round(apply(cf[,c("k","beta")],1,g),3)
}

#' @name tlag_bluck_coward
#' @title lag phase for bluck_coward self-correcting fit
#' @description this parameter is misnamed: it is not the lag, but the maximum 
#' of the PDR time series.
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required.
#' Note that in this package,  \code{k} is measured in 1/min (e.g. 0.01/min),
#' while in the literature is is often quoted as 1/h (e.g. 0.6/h).
#' @return lag phase in minutes (time t at which the maximum in the rate of change
#' of g(t) occurs)
#' @seealso \code{\link{exp_beta}}, and \code{\link{t50_bluck_coward}} for an example.
#' @export
tlag_bluck_coward = function(cf) {
  unlist(log(cf["beta"] / 2) / cf["k"])
}

#' @name t50_maes_ghoos
#' @title t50 as determined from an uncorrected fit to the beta exponential function.
#' @description
#' Maes B D, Ghoos Y F, Rutgeerts P J, Hiele M I, Geypens B and Vantrappen G 1994 
#' Dig. Dis. Sci. 39 S104-6.
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' note that \code{k} is measured in 1/min (e.g. 0.01/min),
#' usually it is quoted as 1/h (e.g. 0.6/h).
#' @return time where value is 1/2 of maximum, i.e. \code{t50} in minutes
#' @seealso \code{\link{exp_beta}}, and \code{\link{t50_bluck_coward}} for an example.
#' @export
t50_maes_ghoos = function(cf) {
  unlist(-log(1 - 2 ^ (-1 / cf["beta"])) / cf["k"])
}

#' @name tlag_maes_ghoos
#' @title so-called lag time from maes/ghoos fit
#' @description compute \code{tlag} from uncorrected fit to the beta 
#' exponential function. The name \code{tlag} is a misnomer; it simply 
#' is the maximum of the PDR curve, 
#'  so in papers by Bluck et al. it is renamed to t_max.
#' Maes B D, Ghoos Y F, Rutgeerts P J, Hiele M I, Geypens B and Vantrappen G 1994 
#' Dig. Dis. Sci. 39 S104-6.
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' \code{k} is measured in 1/min (e.g. 0.01/min).
#' @return lag time as defined from Maes/Ghoos fit
#' @seealso \code{\link{exp_beta}}, and \code{\link{t50_bluck_coward}} for an example.
#' @export
tlag_maes_ghoos = function(cf) {
  unlist(log(cf["beta"]) / cf["k"])
}

#' @name t50_maes_ghoos_scintigraphy
#' @title t50 from maes with scintigrapic correction
#' @description t50 from beta exponential function, with linear correction for
#' scintigraphic values. This is for comparison with published data only;
#' there is little justification for using it, even if it is closer to real gastric
#' emptying times as determined by MRI or scintigraphy.
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required
#' @return time where value is 1/2 of maximum, i.e. t50 in minutes.
#' @seealso \code{\link{exp_beta}}, and \code{\link{t50_bluck_coward}} for an example.
#' @export
t50_maes_ghoos_scintigraphy = function(cf) {
  (t50_maes_ghoos(cf) - 66.09) / 1.12
}
