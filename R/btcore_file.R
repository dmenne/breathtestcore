#' Path to example breath test data file
#'
#' @param filename example file in \code{extdata} directory without path. 
#' Case sensitive on Unix. When filename is missing, returns the names of the 
#' available sample files.
#' @param full.names When \code{filename} is NULL, return full path names
#'
#' @return full filename to example file to use in read_xxx
#' @examples
#'   head(btcore_file())
#'   filename = btcore_file("IrisMulti.TXT")
#'   data = read_iris(filename)
#' @export
btcore_file = function(filename = NULL, full.names = FALSE){
  if (is.null(filename)) 
    return(dir(system.file("extdata", package = "breathtestcore")))
  system.file("extdata", filename, package = "breathtestcore")  
}
