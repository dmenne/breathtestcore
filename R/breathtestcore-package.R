#' @title breathtestcore
#' @description Stan fits to 13C breath test curves. Reads several formats
#'   of 13C   data (IRIS/Wagner, BreathID) and CSV.  Creates artificial
#'   sample data for testing.  Fits Maes/Ghoos, Bluck-Coward self-correcting
#'   formula using 'nls', 'nlme'. Methods to fit breath test curves with
#'   Bayesian Stan methods are refactored to package 'breathteststan'.
#'   For a Shiny GUI, see package 'dmenne/breathtestshiny' on github.
#'
#' @name breathtestcore-package
#' @aliases breathtestcore
# usethis namespace: start
#' @import assertthat
#' @import dplyr
#' @import ggplot2
#' @import multcomp
#' @import readr
#' @import stringr
#' @importFrom MASS mvrnorm
#' @importFrom broom augment tidy
#' @importFrom ggfittext geom_fit_text
#' @importFrom graphics plot plot.default
#' @importFrom methods is
#' @importFrom nlme nlme nlmeControl fixef nlsList
#' @importFrom purrr map map_df map_lgl modify_if flatten
#' @importFrom readxl read_excel
#' @importFrom signal interp1
#' @importFrom stats confint relevel quantile residuals var sigma
#' @importFrom stats rt rnorm na.omit rlnorm coef AIC deviance
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom tidyr spread
#' @importFrom tools file_ext
#' @importFrom utils capture.output
#' @importFrom xml2 read_xml xml_attrs xml_find_first xml_text xml_attr xml_find_all
# usethis namespace: end
NULL
