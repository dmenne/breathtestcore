#' @title Decimate densely sampled 13C time series
#' @description When data of a record are more closely spaced \code{sample_minutes}, 
#' these are spline-subsampled to \code{sample_minutes}. Values up to 1/5 of 
#' maximal time are sampled more densely.
#' Too dense sampling leads to non-convergent \code{nlme} fits and to long runs
#' with Stan-based fits. 
#' The function is used internally by package \code{breathtestcore} and is exported 
#' for use  by package \code{breathteststan}.
#' 
#' @param data Data frame with columns \code{patient_id, group, minute, pdr}.
#' @param sample_minutes Required average density. When points are more closely
#' space, data are subsamples. No upsampling occurs when data are more sparse.
#' @export
#' 
subsample_data = function(data, sample_minutes){
  # Ugly CRAN hack
  patient_id = group = pat_group = . = minute = NULL
  # Check if data have been validated by cleanup_data
  assert_that(are_equal(names(data), c("patient_id", "group", "minute", "pdr")))
  
  # Combine patient and group
  data = data %>%
    ungroup() %>%
    mutate(
      pat_group = paste(patient_id, group, sep = "/")
    )
  
  # Do subsampling if data are too dense
  spacing = data %>%
    group_by(pat_group) %>%
    summarize(
      spacing = round(mean(diff(minute)))
    )
  
  data = data %>%
    left_join(spacing, by = "pat_group") %>%
    group_by(patient_id, group, pat_group) %>%
    do({
      if (.$spacing[1] > sample_minutes) {
        data.frame(minute = .$minute, pdr = .$pdr)
      } else {
        dense_sample_minutes = round(sample_minutes/3)
        dense_to_minute = max(round(max(.$minute)/(5 * sample_minutes),1))*sample_minutes
        minute = round(c(seq(min(.$minute), dense_to_minute, dense_sample_minutes), 
                 seq(dense_to_minute+sample_minutes, max(.$minute), by = sample_minutes)))
        data.frame(minute = minute,
                   pdr = signal::interp1(.$minute, .$pdr, minute, "spline"))
      }
    }) %>%
    ungroup() %>% 
    na.omit()
  
}