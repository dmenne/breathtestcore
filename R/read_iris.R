#' @title Read 13C data from IRIS/Wagner Analysen
#'
#' @description Reads composite files with 13C data from IRIS/Wagner Analysen.
#' The composite files start as follows:
#' \preformatted{
#' "Testergebnis"
#' "Nummer","1330"
#' "Datum","10.10.2013"
#' "Testart"}
#'
#' @param filename name of IRIS/Wagner file in composite format
#' @param text alternatively, text can be given as string
#' @return list with \code{file_name, patient_name, patient_first_name,
#' test, identifikation}, and data frame \code{data} with \code{time}
#' and \code{dob}
#' @author dieter menne, \email{dieter_menne@@menne-biomed_de}
#' @examples
#' filename = system.file("extdata", "IrisMulti.TXT", package = "breathtestcore")
#' iris_data = read_iris(filename)
#' str(iris_data)
#' @export read_iris
read_iris = function(filename = NULL, text = NULL) {
  if (is.null(text)) {
    if (!file.exists(filename))
      stop(paste0("file ", filename, " does not exist."))
    text = readLines(filename)
  } else {
    filename = 'from text'
  }
  
  # check if this is the right format
  header = str_trim(text[1])
  if (header != "\"Testergebnis\"")
    stop(paste0(header, 
        "\nis not from a valid IRIS/Wagner data. First line should be <<Testergebnis>>"
      )
    )
  data_row = which(str_detect(text, "Daten"))
  if (length(data_row) == 0)
    stop("File does not contain data")
  
  record_date = find_pattern(text, "Datum")
  record_date = strptime(record_date, "%d.%m.%y")
  # try if there is a patient number. If not, try identification
  patient_id = try(find_pattern(text, "Patient"), silent = TRUE)
  if (class(patient_id) == "try-error")
    patient_id = find_pattern(text, "Identifikation")
  
  test_no = as.integer(find_pattern(text, "Nummer"))
  substrate = find_pattern(text, "Substrat")
  gender = find_pattern(text, "Geschlecht")
  if (nchar(gender) > 0) {
    gender = str_sub(tolower(gender), 1, 1)
    if (gender != "m")
      gender = "f" # make sure to avoid German names
  }
  dose = as.numeric(find_pattern(text, "Dosis"))
  # workaround for "Groesse" (with umlauts and scharf-s) and utf
  height = as.numeric(find_pattern(text, "Gr.*e.*", TRUE)) * 100
  weight = as.numeric(find_pattern(text, "Gewicht.*", TRUE))
  test = find_pattern(text, "Abk.*rzung")
  # there are multiple "name" fields; skip the first
  name = find_pattern(text[-(1:14)], "Name")
  first_name = find_pattern(text, "Vorname")
  initials = NA
  if (nchar(name) > 0 && nchar(first_name) > 0)
    initials =  paste0(str_sub(name, 1, 1),
                       str_sub(first_name, 1, 1))
  data = utils::read.csv(textConnection(text[-(1:data_row)]))
  data = try(data[, c("Testzeit..min.",
                      "DOB..o.oo.",
                      "Atom.ppm.Excess.13C..ppm.")])
  data = try(data[, c("Testzeit..min.", "DOB..o.oo.")])
  if (inherits(data, "try-error"))
    stop("invalid data columns in IRIS/Wagner data file")
  names(data) = c("time", "dob")
  # remove too small values
  data = data[data$dob >= -10, ]
  breathtest_data(
    file_name = basename(filename),
    patient_id = patient_id,
    name = name,
    first_name = first_name,
    initials =  initials,
    test_no = test_no,
    dose = dose,
    study = test,
    record_date = record_date,
    device = "Iris",
    height = height,
    weight = weight,
    substrate = substrate,
    data = data
  )
}


find_pattern = function(bid, pattern, required = TRUE) {
  p = str_match(bid, paste0('\\"', pattern, '\\",\\s*\\"(.*)\\"'))[, 2]
  p = p[!is.na(p)]
  if (length(p) > 1)
    stop(paste0("No unique <<", pattern, ">> in Iris file"))
  if (length(p) == 0) {
    if (required)
      stop(paste0("No <<" , pattern, ">> found in Iris file "))
    else
      p = ""
  }
  return(str_trim(p))
}
