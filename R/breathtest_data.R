#' @title Data structure with PDR data and descriptors for breath test records
#' @description Generates structure of class \code{breathtest_data} with 
#' required fields and optional fields. Optional fields by default are NA. 
#' This structure is used internally as an intermediate when reading in 
#' external file formats. All \code{read_xxx} functions return this structure, 
#' or a list of this structure (e.g. \code{\link{read_breathid_xml}}), and any
#'  converter to  a new format should do  the same to be used with 
#'  \code{\link{cleanup_data}}. 
#' To support a new format with, also update 
#' \code{\link{breathtest_read_function}} which returns the most likely function 
#' to read the file by reading a few lines in it.
#' @param patient_id required, string or number for unique identification
#' @param name optional
#' @param first_name optional
#' @param initials optional, 2 characters, 1 number
#' @param dob optional date of birth (not to be confused with "delta over baseline")
#' @param birth_year optional
#' @param gender optional \code{m} or \code{f}
#' @param study optional name of study; can be used in population fit
#' @param pat_study_id optional; patient number within study_ does not need to be globally unique
#' @param file_name required; file where data were read from, or other unique string_
#' when data are read again, this string is tested and record is skipped when
#' same filename is already in database, therefore uniqueness is important_ when some
#' record does not turn up in database after repeated reading, check if a record with
#' the same file name is already there, and rename the file to avoid collisions_
#' @param device breath_id or iris; default "generic"
#' @param substrate should contain string "ace" or "oct" or "okt", case insensitive_ will
#' be replaced by "acetate" or "octanoate". If empty, "ocatanoate" is assumed.
#' @param record_date required record date_
#' @param start_time optional
#' @param end_time optional
#' @param test_no required integer; unique test number converted to integer if factor
#' @param dose optional, default 100 mg
#' @param height optional, in cm; when pdr must be calculated, default values are
#' used; see \code{\link{dob_to_pdr}}
#' @param weight optional, in kg
#' @param t50  optional, only present if device computes this value
#' @param gec  optional, only present if device computes this value
#' @param tlag optional, only present if device computes this value
#' @param data data frame with at least 5 rows and columns \code{minute} or 
#' \code{time} and one or both of \code{dob} or \code{pdr}. 
#' If pdr is missing, and height, weight 
#' and substrate are given, computes pdr via function \code{\link{dob_to_pdr}}. 
#' When height and weight are missing,  defaults 180 cm and 75 kg are used instead.
#' @examples 
#' # Read a file with known format
#' iris_csv_file = btcore_file("IrisCSV.TXT")
#' iris_csv_data = read_iris_csv(iris_csv_file)
#' # Note that many filds are NA
#' str(iris_csv_data)
#' # Convert to a format that can be fed to one of the fit functions
#' iris_df = cleanup_data(iris_csv_data)
#' # Individual curve fit
#' coef(nls_fit(iris_df)) 

#' @export
breathtest_data = function(patient_id,
                           name = NA,
                           first_name = NA,
                           initials = NA,
                           dob = NA,
                           birth_year = NA,
                           gender = NA,
                           study = NA,
                           pat_study_id = NA,
                           file_name,
                           device = "generic",
                           substrate,
                           record_date,
                           start_time = record_date,
                           end_time = record_date,
                           test_no ,
                           dose = 100,
                           height = 180,
                           weight = 75,
                           t50 = NA,
                           gec = NA,
                           tlag = NA,
                           data = data) {
  if (!inherits(data, "data.frame"))
    stop("Function breathtest_data: data must be a data frame")
  if (nrow(data) < 5)
    stop("Function breathtest_data: data should have a least 5 rows")
  nd = names(data)
  if (!(nd[1] %in% c("time", "minute")))
    stop("Function breathtest_data: first data column must be <<time>> or <<minute>>. It is:<< ", nd[1], ">>")
  # rename data column to minute
  names(data)[1] = "minute"
  if (!sum(nd[-1] %in% c("pdr", "dob")) > 0)
    stop("function breathtest_data: data should have either dob or pdr or both")
  ##### add more substrates here
  ## Assume ocatanoate by default
  substrate = str_trim(substrate)
  if (substrate == "") {
    substrate = "octanoate"
  } else {
    substrates = c("octanoate", "acetate")
    substrate_pattern = c("o[ck]t", "acet")
    substrate = substrates[str_detect(tolower(substrate), substrate_pattern)][1]
  }
  if (length(substrate) == 0)
    substrate = "octanoate"
  if (!is.na(gender) & !match(gender, c("m", "f")))
    stop("function breathtest_data: gender should be 'm' or 'f'")
  # force NA if weight or height is not 0
  if (weight <= 30 || height < 1) {
    height = 180
    weight = 75
  }
  if (!"pdr" %in% nd)
    data$pdr = dob_to_pdr(data$dob, weight, height, mw = substrate)
  data$minute = as.double(data$minute)
  structure(
    list(
      patient_id = patient_id,
      name = name,
      first_name = first_name,
      initials = initials,
      dob = dob,
      birth_year = birth_year,
      gender = gender,
      study = study,
      pat_study_id = pat_study_id,
      file_name = file_name,
      device = device,
      substrate = substrate,
      record_date = as.character(record_date),
      start_time = as.character(start_time),
      end_time = as.character(end_time),
      test_no = as.integer(test_no),
      dose = 100,
      height = height,
      weight = weight,
      t50 = t50,
      gec = gec,
      tlag = tlag,
      data = data
    ),
    class = "breathtest_data"
  )
}
