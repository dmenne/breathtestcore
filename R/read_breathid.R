#' @title Read BreathID file
#'
#' @description Reads 13c data from a BreathID file, and returns a structure 
#' of class \code{breathtest_data}. 
#'
#' @param filename name of txt-file to be read
#' @param text alternatively, text can be given as string
#' @return Structure of class \code{\link{breathtest_data}}
#' @examples
#' filename = btcore_file("350_20043_0_GER.txt")
#' # Show first lines
#' cat(readLines(filename, n = 10), sep="\n")
#' #
#' bid = read_breathid(filename)
#' str(bid)
#' @export
read_breathid = function(filename = NULL, text = NULL) {
#  filename = 'C:/Users/Dieter/Documents/RPackages/breathtestcore/inst/extdata/350_20043_1_GER.txt'
  if (is.null(text)) {
    filename = as.character(filename)
    if (!file.exists(filename))
      stop(paste("File ", filename, "does not exist_"))
    text = readLines(filename)
  } else
  {
    filename = 'from text'  
  }
  header = str_trim(text[1])
  if (header != "Test and Patient parameters")
    stop(paste("Header of file", filename, "is not a valid BreathID file"))
  record_date = find_single_pattern(text, "Date")
  record_date = strptime(record_date, "%m/%d/%y")
  ## note : end time and start time are reversed in the data file, we correct
  ## this here, no typo!!
  end_time = paste(record_date, find_single_pattern(text, "Start time"))
  start_time = paste(record_date, find_single_pattern(text, "End time"))                  
  
  patient_id = find_single_pattern(text, "Patient #")
  test_no = as.integer(find_single_pattern(text, "Test No."))
  substrate = find_single_pattern(text, "Type")
  gender = find_single_pattern(text, "Gender")
  if (nchar(gender) > 0)
    gender = str_sub(tolower(gender), 1, 1)
  dose = as.numeric(find_single_pattern(text, "Dose"))
  height = as.numeric(find_single_pattern(text, "Height"))
  weight = as.numeric(find_single_pattern(text, "Weight"))
  t50 = as.numeric(find_single_pattern(text, "T 1/2"))
  tlag = as.numeric(find_single_pattern(text, "T lag"))
  gec = as.numeric(find_single_pattern(text, "GEC"))
  # Locate the data block
  db = try(str_trim(text[which(str_detect(text, "Time\\s*DOB")):length(text)]), silent =
              TRUE)
  if (is(db, "try-error"))
    stop(paste0("file ", filename, " does not contain PDR data"))
  db = db[db != ""]
  if (length(db) < 2)
    stop(paste0("file ", filename, " does not contain PDR data"))
  tc = textConnection(db)
  data = utils::read.table(tc, header = TRUE)
  close(tc)
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
    tlag = tlag,
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
