#' Path to example breath test data file
#'
#' @param filename example file in \code{extdata} directory without path. 
#' Case sensitive on Unix.
#'
#' @return full filename to example file to use in read_xxx
#' @examples
#'   filename = btcore_file("IrisMulti.TXT")
#'   data = read_iris(filename)
#' @export
btcore_file = function(filename){
  system.file("extdata", filename, package = "breathtestcore")  
}
