#' Snoop method to read breath test file
#'
#' @description Reads the first line of a file, and returns
#' the best matching function to read the breath test data in it.
#' @param filename breath test data file from Iris/Wagner, BreathID or generic CSV, 
#' @param text as alternative to filename, the text may be given as string
#'
#' @return function to read the file or the text; NULL if no matching function 
#' was found 
#' @export
#'
breathtest_read_function = function(filename = NULL, text = NULL) {
  if (is.null(text)) {
    if (!file.exists(filename)) {
      stop(paste("File", filename, "does not exist"))
    }
    d = try(readLines(filename, n = 1), silent = TRUE)
    filename = "from text"
  } else {
    d = text[1] # Use first line
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
#  if (all(str_detect(d, c("record", "minute", "pdr|dob"))))
#    return(read_generic_csv)
  return(NULL)
}

