#' @title Transforms 13C breath data into a clean format for fitting
#' @description Accepts various data formats of ungrouped or grouped 13C breath 
#' test time series, and transforms these into a data frame that can be used by
#' all fitting functions, e.g. \code{\link{nls_fit}}.
#' If in doubt, pass data frame through \code{cleanup_data} before forwarding it 
#' to a fitting function. If the function cannot repair the format, it gives better
#' error messages than the \code{xxx_fit} functions.
#' @param data 
#' \itemize{
#'   \item{A data frame, array or tibble with at least two numeric columns
#'    with  optional names \code{minute} and \code{pdr} to fit 
#'    a single 13C record.  }
#'  \item{A data frame or tibble with three columns named \code{patient_id},
#'    \code{minute} and \code{pdr}. }
#'  \item{A matrix that can be converted to one of the above.}
#'  \item{A list of data frames/tibbles that are concatenated. When the list has 
#'    named elements, the names are converted to group labels. When the list elements
#'    are not named, group name \code{A} is used for all items.}
#'  \item{A structure of class \code{\link{breathtest_data}}, as imported from
#'    a file with \code{\link{read_any_breathtest}}}
#'  \item{A list of class \code{breathtest_data_list} as generated
#'  from read function such as \code{\link{read_breathid_xml}}}
#' }
#' @param ... optional. 
#' \describe{
#'   \item{use_filename_as_patient_id}{Always use filename instead of 
#'   patient name. Use this when patient id are not unique.}
#' }
#' 
#'
#' @return A tibble with 4 columns. Column \code{patient_id} is created with a dummy
#' entry of \code{pat_a} if no patient_id was present in the input data set. 
#' A column \code{group} is required in the input data if the patients are from different 
#' treatment groups or within-subject repeats, e.g. in crossover design. 
#' A dummy group name "A" is added if no group column was available in the input data set.
#' If \code{group} is present, this is a hint to the analysis functions to do 
#' post-hoc breakdown or use it as a grouping variable in population-based methods.
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
#' 
#' Comments are persistent; multiple comments are concatenated with newline separators.
#' 
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
#' # Results from simulate_breathtest_data can be passed directly to cleanup_data
#' cleanup_data(simulate_breathtest_data(3))
#' # .. which implicitly does
#' cleanup_data(simulate_breathtest_data(3)$data)
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
#' f1 = btcore_file("350_20043_0_GER.txt")
#' f2 = btcore_file("IrisMulti.TXT")
#' f3 = btcore_file("IrisCSV.TXT")
#' # With a named list, the name is used as a group parameter
#' data = list(A = read_breathid(f1), B = read_iris(f2), C = read_iris_csv(f3)) 
#' d = cleanup_data(data)
#' str(d)
#' unique(d$patient_id)
#' # "350_20043_0_GER" "1871960"         "123456"
#' # File name is used as patient name if none is available
#' unique(d$group)
#' # "A" "B" "C"
#' @importFrom purrr map_lgl
#' @export 
cleanup_data = function(data, ...) {
  UseMethod("cleanup_data")
} 

#' @export 
cleanup_data.data.frame = function(data, ... ){
  nc = ncol(data)
  # Keep CRAN quiet
  group = pdr = patient_id = minute = NULL 
  assert_that(nc >= 2)
  # https://github.com/dmenne/breathtestcore/issues/1
  data = tibble::set_tidy_names(data)
  # Remove duplicates, for example from uploading the same record twice
  data = dplyr::distinct(data)
  # When there are only two columns, assume they are minute and time
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
        warning("Multiple data for one patient, minute and group. Included the same patient's data twice?")
    data$group = as.character(data$group)  
  }
  # Remove negative values, shift values at 0 slightly.
  # Make pdr simple numeric, i.e. remove gradient
  comment = comment(data) # keep comment for later
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
  # REmove spaces
  data$patient_id = str_replace_all(str_trim(data$patient_id), " ", "_")

  # Add a dummy  group if there is none
  has_group = "group" %in% names(data)
  if (!has_group) {
    # Check if combinations are unique
    if (!all(with(data, table(patient_id, minute)) %in% 0:1))
      stop("Multiple values for the same patient at the same minute require a <<group>> column")
    data$group = "A"
  } else {
    data$group = str_replace_all(str_trim(data$group), " ", "_")
  }
  # Put columns in standard order
  data = data %>% 
    select(patient_id, group, minute, pdr)
  if (!is.null(comment))
    comment(data)  = comment # re-attach comment
  data
}

#' @export 
cleanup_data.matrix = function(data, ... ){
  if (ncol(data) > 2)
    stop("A matrix can only be used as data input when two columns <minute> and <pdr> are passed. Use a data frame otherwise")
  cleanup_data(as_data_frame(data), ...)
}

#' @export 
cleanup_data.breathtest_data_list = function(data, ... ){
  cleanup_data.list(data, ...)
}  

#' @export 
cleanup_data.list = function(data, ... ){
  if (is.null(data)) return(NULL)
  ret = data.frame()
  comment = list()
  for (igroup in seq_along(data))  {
    d1 = data[[igroup]]
    if (is(d1,"simulated_breathtest_data"))
      d1 = d1$data
      
    is_breathtest_data = inherits(d1, "breathtest_data")
    ### This is UGLY code! Should use S3 methods
    needs_group = 
      !("group" %in% names(d1)) && 
      ("patient_id" %in% names(d1))
    
    group = names(data)[igroup]
    if (is.null(group))    group = LETTERS[igroup]
    
    if (needs_group && !is_breathtest_data) {
      d1$group = group
      d1 = d1[c("patient_id", "group", "minute", "pdr")]
    }
    dd = cleanup_data(d1, ...)
    if (is_breathtest_data) {
      if (dot_lgl("use_filename_as_patient_id", ...))
        dd$patient_id = str_sub(d1["file_name"], 1, -5)
      dd$group = group
    }
    comment[[igroup]] = comment(d1)
    ret = rbind(ret, dd )
  }
 if (max(table(ret$minute, ret$patient_id, ret$group)) > 1)
    warning("Multiple data for one patient, minute and group. Included the same patient's data twice?")
  ret = tibble::as_tibble(ret[,c("patient_id", "group", "minute", "pdr")])
  comment = unique(comment)
  comment[map_lgl(comment, is.null)] = NULL
  if (length(comment) > 0)
    comment(ret) = paste(comment, collapse = "\n")
  ret
}
  
#' @export 
cleanup_data.breathtest_data = function(data, ... ){
  id = data$patient_id
  if (is.null(id) || 
      id == "0" || 
      id == "" || 
      dot_lgl("use_filename_as_patient_id", ...))
    id = str_sub(data["file_name"], 1, -5) 
  d = cbind(patient_id = id, data$data[,c("minute", "pdr")])
  cleanup_data(d, ...)    
}

#' @export 
cleanup_data.simulated_breathtest_data = function(data, ... ){
  # Data come from simulate_breathtest_data()
  cleanup_data(data$data, ...)
}

# internal function
dot_lgl = function(label, ...){
  !(is.null(list(...)[[label]]))  # && list(...)[[label]]
}
