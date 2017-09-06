#' @title Convert breath test DOB data to PDR data
#'
#' @description Convert DOB (delta-over-baseline) to PDR for 13C breath test. 
#' This is equation (4) in
#' Sanaka, Yamamoto, Tsutsumi, Abe, Kuyama (2005) Wagner-Nelson method for analysing
#' the atypical double-peaked excretion curve in the [13c]-octanoate gastric
#' emptying breath test in humans. Clinical and experimental pharmacology and
#' physiology 32, 590-594.
#' @note I have no idea where the factor 10 in equation (4) comes from, possibly
#' from percent(PDR)/and DOB(0/00). In Kim and Camillieri, Stable isotope breath 
#' test and gastric emptying, page 207, a factor of 0.1123 instead of 0.01123 
#' is used, without the factor 10. Which one is correct?
#'
#' @param  dob Delta-over-baseline vector in 0/00
#' @param  weight Body weight in kg; assumed 75 kg if missing
#' @param  height Body height in cm; assume 180 cm if missing
#' @param  mw Molecular weight,  83.023388 g/mol for acetate, 167 g/mol for octanoate.
#' Can also be given as string "acetate" or "octanoate".
#' @param  purity_percent Purity in percent
#' @param  mg_substrate Substrate in mg
#' @return PDR percent dose/h
#' @examples
#' filename = system.file("extdata", "350_20049_0_GERWithWeight.txt",
#'     package = "breathtestcore")
#' bid = read_breathid(filename)
#' bid$data$pdr1 = dob_to_pdr(bid$data$dob, weight=bid$weight, height=bid$height)
#'
#' plot(bid$data$minute, bid$data$pdr1, main="points: from breath_id; line: computed",
#' type="l")
#' points(bid$data$minute, bid$data$pdr,col="red",type="p",pch=16)
#' #
#' # Check how far our computed pdr is from the stored pdr
#' var(bid$data$pdr1-bid$data$pdr)
#' @export
dob_to_pdr = function(dob, weight = 75, height = 180, mw = 167,
                    purity_percent = 99.1, mg_substrate = 100) {
  # default assumptions
  if (is.na(weight) ||
      is.null(weight) || max(weight) < 20)
    weight = 75
  if (is.na(height) || is.null(height))
    height = 180
  if (max(height) < 2)
    height = height * 100 # correct for people giving height in meter
  if (is.character(mw))  {
    if (mw == "octanoate")
      mw = 167
    else
      if (mw == "acetate")
        mw = 83.0233388
      else
        stop("dob_to_pdr: mw must be 'octanoate' or 'acetate' or numeric")
  }
  surface = 0.024265 * weight ^ 0.5378 * height ^ 0.3964
  co2per_minute = 300 * surface #
  rpdb = 0.0112372 # isotope ratio in  reference
  # see above note why there is a factor of 10
  dob * co2per_minute * rpdb * 10 * mw / (mg_substrate * purity_percent)
}
