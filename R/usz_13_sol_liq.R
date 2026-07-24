#' Subset of usz_13c with solid/liquid meals
#'
#' Get subset of clinical data with records for solid and liquid meals of the same patient
#'
#' @param sample_minutes When median inter-sample period is less than this value,
#' raw data are resampled to the given period. Resampling is required, because
#' some methods do not converge when there are to many serially correlated data.
#'
#' @returns A `tibble` of records from patients and normals with two meals,
#' possibly resampled, with columns `patient_id`, `group` (`liquid_normal, solid_normal,
#' solid_patient, liquid_patient`) and `pdr`
#'
#' @export
#' @examples
#'   fit = usz_13c_sol_liq() |>
#'     nlme_fit()
#'   coef(fit) |>
#'     filter(parameter == "t50", method == "maes_ghoos")
#'   fit |>
#'     plot()

usz_13c_sol_liq = function(sample_minutes = 15) {
  data(usz_13c)
  both = usz_13c |>
    distinct(patient_id, group) |>
    group_by(patient_id) |>
    filter(n() == 2)
  sol_liq = usz_13c %>%
    inner_join(both, by = join_by(patient_id, group)) |>
    subsample_data(sample_minutes) |>
    cleanup_data()
  attr(
    sol_liq,
    "comment"
  ) = "Breath test data usz_13c from package dmenne/breathtecore, only data with both solids and liquid from normals and patients"
  sol_liq
}


if (FALSE) {}
