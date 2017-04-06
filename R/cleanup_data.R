#' @title Transforms 13C breath data into a clean format for fitting
#' @description Accepts various data formats of ungrouped or grouped 13C breath 
#' test time series, and transforms these into a data frame that can be used by
#' all fitting functions. 
#' @param data 
#' \itemize{
#'   \item{A data frame or tibble with at least two numeric columns with 
#' names \code{minute} and \code{pdr} which can be used to fit a single 13C record }
#'    \item{A list of data frames/tibbles that are concatenated}
#' }
#'
#' @return A tibble with 4 columns. Column \code{patient_id} is created with a dummy
#' entry of \code{pat_a} if no patient_id was present in the input data set. 
#' Column \code{group} is required if the patients are from different 
#' treatment groups or within-subject repeats, e.g. in crossover design. A dummy
#' group name "A" is added if no group column was available on input.
#' If \code{group}  is present, this is a hint to the analysis functions to do 
#' post-hoc breakdown or use it as a grouping in population-based methods.
#' Columns \code{minute} and \code{pdr} are the same as given on input.
#' An error is raise if dummy columns \code{patient_id} and \code{group} cannot be 
#' added in a unique way, i.e. when multiple values for a given minute cannot be 
#' disambiguated.
#' @export 
cleanup_data = function(data) {
  UseMethod("cleanup_data")
} 

#' @export
cleanup_data.data.frame = function(data){
  nc = ncol(data)
  # Keep CRAN quiet
  group = pdr = patient_id = minute = NULL 
  
  assert_that(nc >= 2)
  # When there are only two column, assume they are minute and time
  # implying that it is only one record
  if (nc == 2) {
    names(data) = c("minute", "pdr")
    # when there are multiple versions of the same time,
    # suspect multiple records
    if (max(table(data$minute)) > 1)
      stop("Only two columns, and multiple PDR values for given time. Forgot column pat_id?")
    return(data)
  }
  # When there are three or four columns, they must be named correctly
  if (nc == 3) {
    if (!all(names(data) == c("patient_id", "minute", "pdr"))) {
      stop("Three columns must be named patient_id, minute, pdr")
    }
  }
  # We do not use factors for easier combination of records
  data$patient_id = as.character(data$patient_id)  
  if (nc == 4) {
    if (!all(names(data) == c("patient_id", "group", "minute", "pdr"))) {
      stop("Four columns must be named patient_id, group, minute, pdr")
    }
    data$group = as.character(data$group)  
  }
  # Check if columns are numeric
  if (!(is.numeric(data$minute) && (is.numeric(data$pdr))))
    stop("Columns minute and pdr must be numeric")
  # Remove negative values, shift values a 0 slightly
  data = data %>% 
    filter(minute >= 0 ) %>% 
    mutate(
      minute = if_else(minute == 0, 0.01, minute)
    )
  
  # Add a dummy  patient_id if there is none
  has_patient_id = "patient_id" %in% names(data)
  if (!has_patient_id) {
    if (!all(with(data, table(minute)) == 1))
      stop("Multiple values at the same minute require a <<patient_id>> column")
    data$patient_id = "pat_a"
  }
  
  # Add a dummy  group if there is none
  has_group = "group" %in% names(data)
  if (!has_group) {
    # Check if combinations are unique
    if (!all(with(data, table(patient_id, minute)) == 1))
      stop("Multiple values for the same patient at the same minute require a <<group>> column")
    data$group = "A"
  }
  # Put things in a nice order
  data %>% 
    select(patient_id, group, minute, pdr)
}

#' @export 
cleanup_data.list = function(data){
  if (is.null(data)) return(NULL)
  if (is.null(names(data)))
    names(data) = LETTERS[1:length(data)]
  ret = data.frame()
  for (group in names(data))  {
    dd = cleanup_data(data[[group]])
    dd$group = group
    ret = rbind(ret, dd )
  }
  tibble::as_tibble(ret[,c("patient_id", "group", "minute", "pdr")])
}
  
#' @export 
cleanup_data.breathtest_data = function(data){
  id = data$patient_id
  if (is.null(id) || id == "0" | id == "" )
    id = str_sub(data["file_name"], 1, -5) 
  d = cbind(patient_id = id, data$data[,c("minute", "pdr")])
  cleanup_data(d)    
}

#library(assertthat)
#library(breathtestcore)
#library(testthat)
#library(purrr)


