#' @title Zurich sample set of 13C breath test data
#'
#' @description 13C time series PDR data from normals and random patients
#' from the division of \href{http://www.gastroenterologie.usz.ch/Seiten/default.aspx}{Gastroenterology and Hepatology,
#' University Hospital Zurich}.
#' Most breath samples from normals were collected with bags and analyzed by
#' \href{http://kibion.com/content/uploads/2013/07/113-01_IRIS_Consumables_DE.pdf}{IRIS/Wagner}
#' infrared spectroscopy. Patient samples were recorded with the continuous
#' monitoring system
#' \href{http://www.exalenz.com/mainmenu/breathid-hp/urea-breath-test}{BreathID}.
#'
#' \describe{
#'   \item{patient_id}{Patient identifier, starting with \code{norm} for normals
#'   (healthy volunteers) and  \code{pat} for patients. Note that for normals
#'   there are two records for each subject, so only the combination of patient_id 
#'   and group is a unique identifier of the time series record.}
#'   \item{group}{\code{liquid_normal} for normals and liquid meal,
#'   \code{solid_normal} normals and solid meal, and \code{patient} for patients;
#'   patients are an unselected cross-section from the University Hospital of Zurich.}
#'   \item{minute}{Time in minutes}
#'   \item{pdr}{PDR as computed by breathtest device or from dob via function dob_to_pdr}
#' }
#' @docType data
#' @keywords datasets
#' @name usz_13c
#' @usage data(usz_13c)
#' @examples 
#' data(usz_13c)
#' \dontrun{
#' str(usz_13c)
#' # Plot all records; this needs some time
#' pdf("usz_13c.pdf", height= 30)
#' # null_fit makes data plotable without fitting a model
#' plot(null_fit(usz_13c))
#' dev.off()
#' }
#' # Plot a subset
#' suppressPackageStartupMessages(library(dplyr))
#' usz_part =  usz_13c  %>% 
#'   filter(patient_id %in% c("norm_001","norm_002", "pat_001", "pat_002"))
#' plot(null_fit(usz_part))
#' @format A data frame with 15574 rows and 4 variables
#' 
#' 
#' @import assertthat
#' @import ggplot2
#' @import stringr
#' @import dplyr
#' @import readr
#' @importFrom broom augment tidy
#' @importFrom MASS mvrnorm
#' @importFrom graphics plot plot.default
#' @importFrom purrr map_df
#' @importFrom stats rt rnorm na.omit rlnorm coef AIC deviance
#' @importFrom utils capture.output
#' @importFrom signal interp1
#' @importFrom methods is
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom nlme nlme nlmeControl fixef nlsList
NULL


#' @title 13C breath test data with MRI emptying for comparison
#'
#' @description 13C time series PDR data from normals and three different meals 
#' in a cross-over design from the division of 
#' \href{http://www.gastroenterologie.usz.ch/Seiten/default.aspx}{Gastroenterology and Hepatology,
#' University Hospital Zurich}. See
#' \href{http://onlinelibrary.wiley.com/doi/10.1111/nmo.12025/abstract}{Kuyumcu et al.,
#' Gastric secretion does not affect...}.
#' 
#' Data are formatted as described in \code{\link{usz_13c}}. In addition, half
#' emptying times from MRI measurements are attached to the data as attribute
#' \code{mri_t50}. The example below shows how to analyze the data and present half
#' emptying times from MRI and 13C in diagrams.
#' 
#'
#' @docType data
#' @keywords datasets
#' @name usz_13c_d
#' @usage data(usz_13c_d)
#' @examples 
#' \donttest{
#' library(dplyr)
#' library(ggplot2)
#' data(usz_13c_d)
#' mri_t50 = attr(usz_13c_d, "mri_t50")
#' d = usz_13c_d %>% 
#'   cleanup_data() %>% # recommended to test for validity
#'   nlme_fit()
#' plot(d) +
#'   geom_vline(data = mri_t50, aes(xintercept = t50), linetype = 2)

#' dd = mri_t50 %>% 
#'   inner_join(
#'     coef(d) %>% filter(parameter=="t50", method == "maes_ghoos"),
#'     by = c("patient_id", "group")) %>% 
#'   mutate(
#'     t50_maes_ghoos = value
#'  )
#' 
#' ggplot(dd, aes(x=t50, y = t50_maes_ghoos, color = group)) +  
#'   geom_point() +
#'   facet_wrap(~group) +
#'   geom_abline(slope = 1, intercept = 0) +
#'   xlim(45,205) +
#'   ylim(45,205) 
#' }
NULL


#' @title Exotic 13C breath test data 
#'
#' @description 13C time series PDR data from three different groups in a randomized
#' (= not-crossover) design. This are unpublished data from
#' \href{http://www.gastroenterologie.usz.ch/Seiten/default.aspx}{Gastroenterology and Hepatology,
#' University Hospital Zurich}. 
#' 
#' Data are formatted as described in \code{\link{usz_13c}}. These time series present
#' a challenge for algorithms.
#' 
#' @docType data
#' @keywords datasets
#' @name usz_13c_a
#' @usage data(usz_13c_a)
#' @examples 
#' \donttest{
#' library(dplyr)
#' library(ggplot2)
#' data(usz_13c_a)
#' d = usz_13c_a %>% 
#'   cleanup_data() %>% # recommended to test for validity
#'   nlme_fit()
#' plot(d)
#' }
NULL

