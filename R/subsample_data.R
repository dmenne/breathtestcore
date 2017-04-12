# Local functions used by stan_fit and nlme_fit
subsample_data = function(data, sample_minutes){
  # Ugly CRAN hack
  patient_id = group = pat_group = . = NULL
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
      if (.$spacing[1] > sample_minutes){
        data.frame(minute = .$minute, pdr = .$pdr)
      } else {
        minute = seq(min(.$minute), max(.$minute), by = sample_minutes)
        data.frame(minute = minute,
                   pdr = signal::interp1(.$minute, .$pdr, minute, "spline"))
      }
    }) %>%
    ungroup()
  
}