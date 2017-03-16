#' @title Read 13C data from IRIS/Wagner Analysen in CSV Format
#'
#' @description Reads 13C data from IRIS/Wagner Analysen in CSV Format
#' The CSV files start as follows:
#' \preformatted{
#' "Name","Vorname","Test","Identifikation"
#'} 
#' This format does not have information about the substrate (acetate, octanoate),
#' the dose and body weight and height. By default, using acetate as substrate, 
#' dose = 100,  weight = 75, height = 180.
#' @param filename Name of IRIS/Wagner file in CSV format
#' @return list of class \code{breath_test_data} with file name, 
#' patient name, patient first name, test, identifikation,
#' and data frame \code{data} with \code{time} and \code{dob}
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @examples
#' filename = system.file("extdata", "IrisCSV.TXT", package = "breathtestcore")
#' iris_data = read_iris_csv(filename)
#' str(iris_data)
#' @export 
read_iris_csv = function(filename) {
  if (!file.exists(filename))
    stop(paste0("File ",filename," does not exist."))
  # Check if this is the right format
  header = readLines(filename, 1)
  if (str_detect(header,("^\"Name\",\"Vorname\",\"Test\",\"Identifikation\"")) != 1)
    stop(
      paste0(
        "File ",filename,
        " is not a valid IRIS CSV file. First line should be Name, Vorname,..."
      )
    )
  d = suppressWarnings(readr::read_csv(filename, col_types = "cccciddddddcc",
                        locale = readr::locale(encoding = "latin1")))
  readr::stop_for_problems(d)
  if (ncol(d) != 13)  
    stop(paste0("IRIS CSV file ", filename, " has unexpected columns. Should be 13"))
  if (nrow(d) < 5) 
    stop(paste0("IRIS CSV File ", filename, " has only ", nrow(d), " rows"))

  record_date = strptime(d$Datum[1],"%d.%m.%Y")
  patient_id = extract_id(d$Identifikation[1])
  name = d$Name[1]
  first_name = d$Vorname[1]
  initials = NA
  if (nchar(name) > 0 && nchar(first_name) > 0)
    initials =  paste0(str_sub(name,1,1),
                      str_sub(first_name,1,1))
  data = try(d[,c("Testzeit[min]","DOB [o/oo]")], silent = TRUE)
  if (inherits(data,"try-error"))
    stop("Invalid data columns in Iris data file")
  names(data) = c("time","dob")
  # remove too small values
  data = data[data$dob >= -10,]
  breathtest_data(
    file_name = basename(filename),
    patient_id = patient_id,
    name = name,
    first_name = first_name,
    initials =  initials,
    test_no = 9999,
    dose = 100,
    study = d$Test[1],
    record_date = record_date,
    height = 180,
    weight = 75,
    device = "Iris",
    substrate = 'acetate',
    data = as.data.frame(data)
  )
}


#' @title Extracts an ID from string IRIS CSV file
#'
#' @description First tries to extract only digits, separating these by underscore 
#' when there are multiple blocks. If this give a non-valid  id, returns the 
#' whole string without spaces and perios, hoping it makes sense.
#' For internal use, but may be overridden for exotic IDs
#' @param id One item from column Identifikation, e.g. "KEK-ZH-Nr.2013-1234"
#' @export
extract_id = function(id){
  id1 = paste(str_match_all(id, "([\\d]+)")[[1]][,2], collapse = "_")
  if (nchar(id1) >= 5) return(id1)
  tolower(str_replace_all(id, "[\\.\\-\\W]+", "_"))
}

