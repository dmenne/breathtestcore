#' @title Exponential beta function to fit 13c breath test PDR time series
#'
#' @description Functions to fit PDR time series data to exponential-beta function
#' given in: 
#
#' Sanaka M, Nakada K (2010) Stable isotope breath test for assessing gastric emptying:
#' A comprehensive review.  J. Smooth Muscle Research 46(6): 267-280
#'
#' Bluck L J C and Coward W A 2006 Measurement of gastric
#' emptying by the C-13-octanoate breath test --- rationalization
#' with scintigraphy Physiol. Meas. 27 279?89
#'
#' For a review, see
#'
#' Bluck LJC (2009) Recent advances in the interpretation of the 13C octanoate
#' breath test for gastric emptying. Journal of Breath Research, 3 1-8
#'
#' This is the same equation as (4)  in:
#'
#' The Wagner-Nelson Method Can Generate an Accurate Gastric Emptying Flow Curve from
#' 13CO2 Data Obtained by a 13C-Labeled Substrate Breath Test
#' Masaki Sanaka, Takatsugu Yamamoto, Tarou Ishii, Yasushi Kuyama
#'
#'
#' @name exp_beta
# exp_beta= expression(m*d*k*beta*(1-exp(-k*time))^(beta-1)*exp(-k*time))
# deriv(exp_beta,c("m","k","beta"))
#' @author dieter menne, \email{dieter_menne@@menne-biomed_de}
#'
#' @param time vector of time values in minutes
#' @param dose in mg
#' @param m efficiency
#' @param k time constant
#' @param beta form factor
#' @return Values and gradients of estimated PDR for use with \code{nls} and \code{nlme}
#' @examples
#' start = list(m=20,k=1/100,beta=2)
#'
#' # fit to real data set and show different t50 results
#' sample_file = system.file("extdata", "350_20043_0_GER.txt", 
#'     package = "breathtestcore")
#' # time 0 must be removed to avoid singularity
#' breath_id = read_breathid(sample_file)
#' data = subset(breath_id$data, time >0)
#' sample_nls = nls(pdr~exp_beta(time, 100, m, k, beta), data = data, start = start)
#' data$pdr_fit_bluck=predict(sample_nls)
#' plot(data$time, data$pdr, pch=16, cex=0.7, xlab="time (min)", ylab="PDR",
#'   main="t50 with different methods")
#' lines(data$time,data$pdr_fit_bluck, col="blue")
#' t50 = t50bluck_coward(coef(sample_nls))
#' t50maes = t50maes(coef(sample_nls))
#' t50scint = t50maes_scintigraphy(coef(sample_nls))
#' abline(v = t50, col = "red")
#' abline(v = t50maes, col = "darkgreen", lty = 2)
#' abline(v = breath_id$t50, col = "black", lty = 4)
#' abline(v = t50scint, col = "gray", lty = 3)
#' text(t50, 0, "Self-corrected Bluck/Coward", col = "red", adj = -0.01)
#' text(breath_id$t50, 0.5,"From BreathID device",col = "black", adj=-0.01)
#' text(t50scint, 1," Maes/Ghoos scintigraphic", col = "gray", adj = -0.01)
#' text(t50maes,1.5, "Classic Maes/Ghoos", col = "darkgreen", adj = -0.01)
#'
#' # simulated data set
#' dose = 100
#' set.seed(4711)
#' # do not use time 0, this gives singular gradients
#' # if required, shift time=0 by a small positive amount, e.g. 0.1
#' # create simulated data
#' pdr  = data.frame(time=seq(2, 200, by = 10))
#' pdr$pdr =
#'   exp_beta(pdr$time, 100, start$m, start$k, start$beta) + rnorm(nrow(pdr), 0, 1)
#' par(mfrow = c(2, 1))
#' # plot raw data
#' plot(pdr$time, pdr$pdr, pch=16, cex=0.5, xlab = "time (min)",ylab = "PDR")
#' # compute fit
#' pdr_nls = nls(pdr~exp_beta(time, 100, m, k, beta), data = pdr, start = start)
#' # compute prediction
#' pdr$pd_rfit = predict(pdr_nls)
#' lines(pdr$time, pdr$pd_rfit, col="red", lwd=2)
#'
#' # plot cumulative
#' plot(pdr$time, cum_exp_beta(pdr$time,100,coef(pdr_nls)), type="l",
#'      xlab = "time (min)", ylab = "cumulative PDR")
#' # show t50
#' t50 = t50bluck_coward(coef(pdr_nls))
#' tlag = t_lag_bluck_coward(coef(pdr_nls))
#' abline(v = t50, col = "gray")
#' abline(v = tlag,col = "green")
#' abline(h = 50, col = "gray")
#'
#'
#' # create simulated data from several patients
#' pdr1 = data.frame(patient = as.factor(letters[1:10]))
#' pdr1$m = start$m*(1 + rnorm(nrow(pdr1), 0, 0.1))
#' pdr1$k = start$k*(1 + rnorm(nrow(pdr1), 0, 0.3))
#' pdr1$beta = start$beta*(1 + rnorm(nrow(pdr1), 0, 0.1))
#' pdr1  = merge(pdr1, expand.grid(time = seq(2, 200, by = 10), 
#'    patient = letters[1:10]))
#' pdr1 = pdr1[order(pdr1$patient, pdr1$time), ]
#'
#' # simulated case: for patient a, only data up to 50 minutes are available
#' pdr1 = pdr1[!(pdr1$patient == "a" & pdr1$time > 50),]
#' set.seed(4711)
#' pdr1$pdr =
#'   with(pdr1, exp_beta(time, 100, m, k, beta) + rnorm(nrow(pdr1), 0, 1))
#'
#' # compute nls fit for patient a only: fails
#' # the following line will produce an error message
#' pdr_nls = try(nls(pdr~exp_beta(time,100,m,k,beta), data=pdr1, start=start,
#'                   subset=patient=="a"))
#' stopifnot(class(pdr_nls)=="try-error")
#'
#' # use nlme to fit the whole set with one truncated record
#' library(nlme)
#' pdr_nlme = nlme(pdr~exp_beta(time,100,m,k,beta), data = pdr1,
#'                 fixed = m+k+beta~1,
#'                 random = m+k+beta~1,
#'                 groups = ~patient,
#'                 start = c(m = 20, k = 1/100, beta = 2))
#' coef(pdr_nlme)
#' pred_data = expand.grid(time = seq(0, 400, 10), patient = letters[1:10])
#' pred_data$pdr = predict(pdr_nlme, newdata = pred_data)
#' library(ggplot2)
#' ggplot() +
#'   geom_point(data = pdr1, aes(x = time, y = pdr, color = "red")) + 
#'   geom_line(data = pred_data, aes(x=time, y = pdr), color = "black", size=1) +
#'   ggtitle("Short patient record 'a' gives a good fit with few data using nlme.\n
#'           Borrowing strength from nlme in action!")+
#'   facet_wrap(~patient) +
#'   theme(legend.position="none")
#' @export
exp_beta = function(time,dose,m,k,beta) {
  .expr1 <- m * dose
  .expr2 <- .expr1 * k
  .expr3 <- .expr2 * beta
  .expr6 <- exp(-k * time)
  .expr7 <- 1 - .expr6
  .expr8 <- beta - 1
  .expr9 <- .expr7 ^ .expr8
  .expr10 <- .expr3 * .expr9
  .expr20 <- .expr6 * time
  .value <- .expr10 * .expr6
  .grad <- array(0, c(length(.value), 3L), 
                list(NULL, c("m", "k", "beta")))
  .grad[, "m"] <- dose * k * beta * .expr9 * .expr6
  .grad[, "k"] <-
    (.expr1 * beta * .expr9 + .expr3 * 
       (.expr7 ^ (.expr8 - 1) * (.expr8 * .expr20))) * .expr6 - .expr10 * .expr20
  .grad[, "beta"] <- (.expr2 * .expr9 + 
        .expr3 * (.expr9 * log(.expr7))) * .expr6
  attr(.value, "gradient") <- .grad
  .value
}

#' @title cumulative exponential beta function
#' @description equation (2), page 4 from bluck, "recent advances in the interpretation of
#' the 13c octanoate breath test for gastric emptying"_ this is the cumulative beta exponential,
#' and can be used to compute the 50% points_
#'
#' @name cum_exp_beta
#' @param time in minutes
#' @param dose in mg
#' @param cf named vector of coefficients; only \code{k} and \code{beta} are required_
#' note that \code{k} is measured in 1/min (e_g_ 0_01/min),
#' usually it is quoted as 1/h (e_g_ 0_6/h)_
#' @return vector of predicted cumulative pdr
#' @seealso \code{\link{exp_beta}}
#' @export
cum_exp_beta  = function(time,dose,cf) {
  if (!is.numeric(cf))
    stop("cum_exp_beta requires a vector, does not work for data frames")
  ekt = 1 - exp(-cf["k"] * time)
  beta = cf["beta"]
  unlist(dose * (beta * (ekt) ^ (beta - 1) - (beta - 1) * ekt ^ beta))
}
