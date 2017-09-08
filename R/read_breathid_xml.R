#' @title Read new BreathID/Examens XML file
#'
#' @description Reads 13c data from an XML BreathID file, and returns a stucture 
#' of class \code{breathtest_data_list}, which is a list with elements of 
#' class \code{breathtest_data}. 
#'
#' @param filename name of xml-file to be read
#' @param text alternatively, text can be given as string
#' @return List of class \code{breathtest_data_list} of structures of 
#' class \code{\link{breathtest_data}}; an XML file can contain multiple data sets.
#' @examples
#' filename = btcore_file("NewBreathID_01.xml")
#' # Show first lines
#' cat(readLines(filename, n = 10), sep="\n")
#' bid = read_breathid_xml(filename)
#' # List with length 1
#' str(bid, 1)
#' filename = btcore_file("NewBreathID_multiple.xml")
#' bids = read_breathid_xml(filename)
#' str(bids, 1) # 3 elements - the others in the file have no data
#' # Create hook function to deselect first record
#' choose_record = function(records) {
#'   r  = rep(TRUE, length(records))
#'   r[1] = FALSE
#'   r
#' }
#' options(breathtestcore.choose_record = choose_record)
#' bids = read_breathid_xml(filename)
#' str(bids, 1) # 2 elements, first deselected
#' 
#' 
#' @importFrom xml2 read_xml xml_attrs xml_find_first xml_text xml_attr xml_find_all
#' @export
read_breathid_xml = function(filename = NULL, text = NULL) {
  if (is.null(text)) {
    filename = as.character(filename)
    if (!file.exists(filename))
      stop(paste("File ", filename, "does not exist_"))
    text = read_file(filename)
  } else
  {
    filename = 'from text'  
  }

  xml = try(xml2::read_xml(text), silent = TRUE)
  if (inherits(xml, "try-error"))
    stop(paste("File ", filename, "is not a valid XML BreathID file"))
  device =  xml_attrs(xml_find_first(xml, "/Tests"))
  
  
  xmls = xml_find_all(xml, "Test")
  ret = lapply(xmls, read_breathid_xml_record, filename, device)
  not_null = !unlist(lapply(ret, is.null))
  ret = ret[not_null]
  # Hook to select records
  ch = options("breathtestcore.choose_record")$breathtestcore.choose_record
  if (length(ret) > 1 & !is.null(ch)) {
    pt = paste("Patient", purrr::map_chr(ret, "patient_id"), 
          purrr::map_chr(ret, "record_date"),
          purrr::map_chr(ret, "start_time"))
    ret = ret[ch(pt)]
  }
  class(ret) = "breathtest_data_list"
  ret
}

# Local function to read one record
read_breathid_xml_record = function(xml_0, filename, device){ 
  # local function xml_num
  xml_num = function(xml_0, path){
    as.numeric(unlist(str_split(xml_text(xml_find_first(xml_0, path)),",")))
  }
  
  data = na.omit(data.frame(
    minute = xml_num(xml_0,".//DOBListTimes"),
    dob = xml_num(xml_0, ".//DOBListValues")
  ))

  if (nrow(data) == 0 ) # No data
    return(NULL)
  attr(data, "na.action") = NULL # Remove na.omit
  # e.g. "19Jul2017 11:02"
  
  tryCatch({
    start_time_str = xml_text(xml_find_first(xml_0, "StartTime"))
    end_time_str = xml_text(xml_find_first(xml_0, "EndTime"))
    lct <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    record_date = strptime(start_time_str, "%d%b%Y")
    Sys.setlocale("LC_TIME", lct)
    
    start_time =  str_extract(start_time_str, "\\d\\d:\\d\\d$")
    end_time =  str_extract(end_time_str, "\\d\\d:\\d\\d$")
    invisible(NULL)
  }, error = function(e){
    stop("No valid date/time in XML file: ", filename)
  }
  )
  
  patient_id = xml_text(xml_find_first(xml_0, "ID"))
  test_no = as.integer(xml_attr(xml_find_first(xml_0, "/*/Test"), "Number"))
  breathtest_data(
    file_name = basename(filename),
    patient_id = patient_id,
    test_no = test_no,
    record_date = record_date,
    start_time = start_time,
    end_time = end_time,
    substrate = "acetate", #### Problem !!!
    device = paste0("BreathID_", device),
    data = data
  )
}

if (FALSE) {
  library(xml2)
  library(stringr)
  library(readr)
  filename = 'C:/Users/Dieter/Documents/RPackages/breathtestcore/inst/extdata/NewBreathID_multiple.xml'
  text = NULL
  
  b = read_breathid_xml(filename)
  str(b,1)

}
