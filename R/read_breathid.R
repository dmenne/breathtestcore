#' @title read BreathID file
#'
#' @description Reads 13c data from a BreathID file, and returns a stucture for
#' of class \code{breathtest_data}. BreathID files start with the following lines
#'
#' \code{Test and Patient parameters}
#'
#' @param filename name of txt-file to be read
#' @return structure of class \code{\link{breathtest_data}}
#' @author dieter menne, \email{dieter_menne@@menne-biomed_de}
#' @import stringr
#' @examples
#' filename = system.file("extdata", "350_20043_0_GER.txt", package = "breathtestcore")
#' bid = read_breathid(filename)
#' str(bid)
#' @export
read_breathid = function(filename) {
#  filename = 'C:/Users/Dieter/Documents/RPackages/breathtestcore/inst/extdata/350_20043_1_GER.txt'
  filename = as.character(filename)
  if (!file.exists(filename))
    stop(paste0("file ", filename, " does not exist_"))
  bid = readLines(filename)
  header = str_trim(bid[1])
  if (header != "Test and Patient parameters")
    stop(paste0("file ", filename, " is not a valid BreathID file_"))
  record_date = find_single_pattern(bid, "Date")
  record_date = strptime(record_date, "%m/%d/%y")
  ## note : end time and start time are reversed in the data file, we correct
  ## this here, no typo!!
  end_time = paste(record_date, find_single_pattern(bid, "Start time"))
  start_time = paste(record_date, find_single_pattern(bid, "End time"))                  
  
  patient_id = find_single_pattern(bid, "Patient #")
  test_no = as.integer(find_single_pattern(bid, "Test No."))
  substrate = find_single_pattern(bid, "Type")
  gender = find_single_pattern(bid, "Gender")
  if (nchar(gender) > 0)
    gender = str_sub(tolower(gender), 1, 1)
  dose = as.numeric(find_single_pattern(bid, "Dose"))
  height = as.numeric(find_single_pattern(bid, "Height"))
  weight = as.numeric(find_single_pattern(bid, "Weight"))
  t50 = as.numeric(find_single_pattern(bid, "T 1/2"))
  t_lag = as.numeric(find_single_pattern(bid, "T lag"))
  gec = as.numeric(find_single_pattern(bid, "GEC"))
  # Locate the data block
  db = try(str_trim(bid[which(str_detect(bid, "Time\\s*DOB")):length(bid)]), silent =
              TRUE)
  if (class(db) == "try-error")
    stop(paste0("file ", filename, " does not contain PDR data"))
  db = db[db != ""]
  if (length(db) < 2)
    stop(paste0("file ", filename, " does not contain PDR data"))
  data = utils::read.table(textConnection(db), header = TRUE)
  data = remove_na_columns(data)
  names(data) = tolower(names(data))
  breathtest_data(
    file_name = basename(filename),
    patient_id = patient_id,
    test_no = test_no,
    gender = gender,
    record_date = record_date,
    start_time = start_time,
    end_time = end_time,
    substrate = substrate,
    device = "BreathID",
    dose = dose,
    height = height,
    weight = weight,
    t50 = t50,
    t_lag = t_lag,
    gec = gec,
    data = data
  )
  
}

remove_na_columns = function(x) {
  x[, !unlist(lapply(x, function(y)
    all(is.na(y) | is.nan(y) | str_trim(y) == "")))]
}


find_single_pattern = function(bid, pattern, required = TRUE) {
  p = str_match(bid,paste0(pattern,"[^-]+-\\s*(\\S.*)"))[,2]
  p = str_trim(p[!is.na(p)])
  if (length(p) > 1)
    stop(paste0("no unique <<", pattern, ">> in breath_id file"))
  if (length(p) == 0) {
    if (required)
      stop(paste0("no <<" , pattern, ">> found in breath_id file "))
    else
      p = ""
  }
  p
}
