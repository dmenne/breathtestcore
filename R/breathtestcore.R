#' @title breathtestcore.
#'
#' @description Functions to read and analyze 13C breath test data for gastric emptying
#' @docType package
#' @import rstan
#' @import assertthat
#' @import ggplot2
#' @import stringr
#' @import dplyr
#' @import readr

#' @importFrom broom augment tidy
#' @importFrom MASS mvrnorm
#' @importFrom graphics plot plot.default
#' @importFrom purrr map_df
#' @importFrom stats rt rnorm na.omit rlnorm coef AIC deviance
#' @importFrom utils capture.output
#' @importFrom signal interp1
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom nlme nlme nlmeControl fixef nlsList
#' @useDynLib breathtestcore, .registration = TRUE
#' @export stanmodels 

#' @title Zurich sample set of 13C breath test data
#' @description 13C time series PDR data from normals and random patients
#' from the division of \href{http://www.gastroenterologie.usz.ch/Seiten/default.aspx}{Gastroenterology and Hepatology,
#' University Hospital Zurich}.
#' Most breath samples from normals were collected with bags and analyzed by
#' \href{http://kibion.com/content/uploads/2013/07/113-01_IRIS_Consumables_DE.pdf}{IRIS/Wagner}
#' infrared spectroscopy. Patient samples were recorded with the continuous
#' monitoring system
#' \href{http://www.exalenz.com/mainmenu/breathid-hp/urea-breath-test}{BreathID}.
#'
#' \itemize{
#'   \item patient_id Patient identifier, starting with \code{norm} for normals
#'   (healthy volunteers) and  \code{pat} for patients. Note that for normals
#'   there are two records for each subject, so only the combination of group and
#'   is a unique of the record.
#'   \item{group} \code{liquid_normal} for normals and liquid meal,
#'   \code{solid_normal} normals and solid meal, and \code{patient} for patients;
#'   patients are an unselected cross-section from the University Hospital of Zurich.
#'   \item{minute} Time in minutes
#'   \item{pdr} PDR as computed by breathtest device or from dob via function dob_to_pdr
#' }
#' @docType data
#' @keywords datasets
#' @name usz_13c
#' @usage data(usz_13c)
#' @format A data frame with 15574 rows and 4 variables
NULL
