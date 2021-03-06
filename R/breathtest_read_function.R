#' @title Snoop method to read breath test file
#' @description Reads the first line of a file, and returns
#' the best matching function to read the breath test data in it.
#' To automatically read the file with the inferred file type,
#' use \code{\link{read_any_breathtest}}. For Excel files, only the
#' first sheet is read.
#' @param filename breath test data file from Iris/Wagner (extended or CSV), 
#' BreathID
#' @param text as alternative to filename, pass the text of the file directly.
#' This parameter is not used for Excel files.
#' @return Function to read the file or the text; NULL if no matching function 
#' was found 
#' @examples
#'  file = btcore_file("IrisCSV.TXT")
#'  # Get function to read this file. Returns \code{\link{read_iris_csv}}.
#'  read_fun = breathtest_read_function(file)
#'  str(read_fun(file))
#'  # or, simple (returns a list!)
#'  str(read_any_breathtest(file), 1 )
#'  
#' @importFrom tools file_ext
#' @export
#'
breathtest_read_function = function(filename = NULL, text = NULL) {
  if (is.null(text)) {
    if (!file.exists(filename)) {
      stop(paste("File", filename, "does not exist"))
    }
    ext = file_ext(filename)
    # Excel files are handled separately, text is not used
    if (ext %in% c("xlsx", "xls"))
      return(read_breathtest_excel)
    
    # Assume non-Exel ASCII    
    d = try(readLines(filename, n = 1), silent = TRUE)
    filename = "from text"
  } else {
    d = text[1] # Use first line
    ext = ""
  }
  d = stringr::str_trim(d)
  if (inherits(d, "try-error") || nchar(d) == 0)
    stop(paste("File", filename, "is empty"))
  if (d == "Test and Patient parameters") 
    return(read_breathid)
  if (d == '"Testergebnis"') 
    return(read_iris)
  if (str_detect(d, '"Name","Vorname","Test","Identifikation"'))
    return(read_iris_csv)
  if (ext == "xml" && stringr::str_detect(d, "<Tests Device="))
    return(read_breathid_xml)
  #  if (all(str_detect(d, c("record", "minute", "pdr|dob"))))
#    return(read_generic_csv)
  return(NULL)
}

