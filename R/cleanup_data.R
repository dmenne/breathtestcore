#' @title Transforms 13C breath data into a clean format for fitting
#' @description Accepts various data formats of ungrouped or grouped 13C breath 
#' test time series, and transforms these into a data frame that can be used by
#' all fitting functions, e.g. \code{\link{nls_fit}}.
#' @param data 
#' \itemize{
#'   \item{A data frame, array or tibble with at least two numeric columns
#'    with  optional names \code{minute} and \code{pdr} to fit 
#'    a single 13C record.  }
#'    \item{ An data frame or tibble with three columns named \code{patient_id},
#'    \code{minute} and \code{pdr}. }
#'    \item{A list of data frames/tibbles that are concatenated. When the list has 
#'    named elements, the names are converted to group labels. When the list elements
#'    are not named, group name \code{A} is used for all items.}
#'    \item{A structure of class \code{\link{breathtest_data}}, for example when
#'    importing from a file or a database.}
#' }
#'
#' @return A tibble with 4 columns. Column \code{patient_id} is created with a dummy
#' entry of \code{pat_a} if no patient_id was present in the input data set. 
#' Column \code{group} is required if the patients are from different 
#' treatment groups or within-subject repeats, e.g. in crossover design. 
#' 
#' A dummy group name "A" is added if no group column was available on input.
#' If \code{group} is present, this is a hint to the analysis functions to do 
#' post-hoc breakdown or use it as a grouping in population-based methods.
#' A patient can have records in multiple groups, for example in a cross-over 
#' designs. 
#' 
#' Columns \code{minute} and \code{pdr} are the same as given on input, but negative
#' minute values are removed, and an entry at 0 minutes is shifted to 0.01 minutes 
#' because most fit methods cannot handle the singularity at t=0.
#' 
#' An error is raised if dummy columns \code{patient_id} and \code{group} cannot be 
#' added in a unique way, i.e. when multiple values for a given minute cannot be 
#' disambiguated.
#' @examples 
#' options(digits = 4)
#' # Full manual
#' minute = seq(0,30, by = 10)
#' data1 = data.frame(minute, 
#'    pdr = exp_beta(minute, dose = 100, m = 30,  k = 0.01, beta = 2))
#' # Two columns with data at t = 0
#' data1
#' # Four columns with data at t = 0.01
#' cleanup_data(data1)
#' 
#' # Use simulated data
#' data2 = list(
#'   Z = simulate_breathtest_data(seed = 10)$data,
#'   Y = simulate_breathtest_data(seed = 11)$data)
#' d = cleanup_data(data2)
#' str(d)
#' unique(d$patient_id)
#' unique(d$group)
#' # "Z" "Y"
#' 
#' # Mix multiple input formats
#' f1 = system.file("extdata", "350_20043_0_GER.txt", package = "breathtestcore")
#' f2 = system.file("extdata", "IrisMulti.TXT", package = "breathtestcore")
#' f3 = system.file("extdata", "IrisCSV.TXT", package = "breathtestcore")
#' # With a named list, the name is used as a group parameter
#' data = list(A = read_breathid(f1), B = read_iris(f2), C = read_iris_csv(f3)) 
#' d = cleanup_data(data)
#' str(d)
#' unique(d$patient_id)
#' # "350_20043_0_GER" "1871960"         "123456"
#' # File name is used a patient name if none is available
#' unique(d$group)
#' # "A" "B" "C"

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
  }
  # Check if columns are numeric
  if (!(is.numeric(data$minute) && (is.numeric(data$pdr))))
    stop("Columns minute and pdr must be numeric")

  # When there are three, they must be named correctly
  if (nc == 3) {
    if (!all(names(data) == c("patient_id", "minute", "pdr"))) {
      stop("Three columns must be named patient_id, minute, pdr")
    }
    if (max(table(data$minute, data$patient_id)) > 1)
      stop("Multiple data for one patient and minute. Forgot column group?")
  }
  if (nc == 4) {
    if (!all(names(data) == c("patient_id", "group", "minute", "pdr"))) 
      stop("Four columns must be named patient_id, group, minute, pdr")
    if (max(table(data$minute, data$patient_id, data$group)) > 1)
        stop("Multiple data for one patient, minute and group. Included the same patient's data twice?")
    data$group = as.character(data$group)  
  }
  # Remove negative values, shift values at 0 slightly.
  # Make pdr simple numeric, i.e. remove gradient
  data = data %>% 
    filter(minute >= 0 ) %>% 
    mutate(
      minute = pmax(minute, 0.01),
      pdr = as.vector(pdr)
    )

  # Add a dummy  patient_id if there is none
  has_patient_id = "patient_id" %in% names(data)
  if (!has_patient_id) {
    data$patient_id = "pat_a"
  }
  # We do not use factors for easier combination of records
  data$patient_id = as.character(data$patient_id)  
  
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
cleanup_data.matrix = function(data){
  if (ncol(data) > 2)
    stop("A matrix can only be used as data input when two columns <minute> and <pdr> are passed. Use a data frame otherwise")
  cleanup_data(as_data_frame(data))
}

#' @export 
cleanup_data.list = function(data){
  if (is.null(data)) return(NULL)
  has_names = !is.null(names(data))
  ret = data.frame()
  for (igroup in 1:length(data))  {
    dd = cleanup_data(data[[igroup]])
    if (has_names) {
      dd$group = names(data)[igroup]
    } else
      dd$group = "A" # default dummy name
    ret = rbind(ret, dd )
  }
 if (max(table(ret$minute, ret$patient_id, ret$group)) > 1)
    stop("Multiple data for one patient, minute and group. Included the same patient's data twice?")
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

