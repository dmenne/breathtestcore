#' Simulate 13C breath time series data
#' 
#' Generates simulated breath test data, optionally with errors
#' @param n_records Number of records
#' @param m_mean,m_std Mean and between-record standard deviation of parameter m giving metabolized fraction.
#' @param k_mean,k_std Mean and between-record standard deviation of parameter k, in units of 1/minutes.
#' @param beta_mean,beta_std Mean and between-record standard deviations of lag parameter beta
#' @param noise Standard deviation of normal noise when \code{student_t_df = NULL}; scaling of noise when student_t_df >= 2.
#' @param student_t_df When NULL (default), Gaussian noise is added; when >= 2, Student_t distributed noise is added, which generates more realistic outliers. Values from 2 to 5 are useful, when higher values are used the result comes close to that of Gaussian noise. Values below 2 are truncated to 2.
#' @param missing When 0 (default), all curves have the same number of data points. When > 0, this is the fraction of points that were removed randomly to simulate missing 
#' @param seed optional seed; not set if seed = NULL (default)
#' @param dose Octanoate/acetate dose, almost always 100 mg, which is also the default
#' @param step_minute Inter-sample interval for breath test
#' @param max_minute Maximal time in minutes.
#'
#' @return A list with 3 elements:
#' \describe{
#'   \item{record}{Data frame with columns
#'     \code{record(chr), m, k, beta} giving the effective parameters 
#'     for the individual record.}
#'   \item{data}{Data frame with columns
#'     \code{record(chr), minute(dbl), pdr(dbl)} giving the
#'      time series and grouping parameters.}
#'  }
#'  A comment is attached to the return value that can be used as a title
#' @examples
#' set.seed(4711)
#' library(ggplot2)
#' pdr = simulate_breathtest_data(n_records = 4)
#' ggplot(pdr$data, aes(x = minute, y = pdr)) + geom_point() +
#'   facet_wrap(~record) + ggtitle(comment(pdr$data))
#'
#' @importFrom stats rt rnorm
#' @import assertthat
#' @import dplyr
#' @export
#'
simulate_breathtest_data = function(
  n_records = 10,
  m_mean = 40, m_std = m_mean/4,
  k_mean = 0.01,  k_std = k_mean/4,
  beta_mean = 2, beta_std = beta_mean/5,
  noise = 1,
  student_t_df = NULL,
  missing = 0,
  seed = NULL,
  dose = 100,
  step_minute = 15,
  max_minute = 150) {

  # Hack to avoid notes
  pdr = . = NULL
  if (!is.null(seed))
    set.seed(seed)
  assert_that(n_records >= 1)
  assert_that(m_std >= 0)
  assert_that(k_std >= 0)
  assert_that(beta_std >= 0)

  assert_that(m_mean > 3*m_std)
  assert_that(k_mean > 3*k_std)
  assert_that(beta_mean > 3*beta_std)
  
  assert_that(max_minute %/% step_minute > 5)

  time  = seq(0, max_minute, by = step_minute)
  # Record
  rec = data_frame(
    record = sprintf("rec_%02d", 1:n_records),
    m = round(rnorm(n_records, m_mean, m_std)),
    k = pmax(rnorm(n_records, k_mean, k_std), k_mean/3),
    beta = pmax(rnorm(n_records, beta_mean, beta_std), beta_mean/3)
  )

  # Noise term
  if (is.null(student_t_df) || student_t_df == 0 ) {
    if (noise == 0)
      warning("With noise == 0, non-linear fits might fail.")
    noise_d = rnorm(nrow(rec)*length(time), 0, noise)
  } else {
    if (student_t_df < 2) {
      student_t_df = 2
      warning("Degrees of freedom of Student-t noise was set to 2")
    }
    noise_d = noise*rt(nrow(rec)*length(time), student_t_df)
  }
  # Data
  data = rec %>%
    rowwise() %>%
    do(
      data_frame(
        record = .$record,
        minute = time,
        pdr = exp_beta(time = time,  dose = dose , m = .$m, k = .$k, beta = .$beta)
      ))  %>%
    ungroup() %>%
    mutate(
      pdr = pdr + noise_d,
      pdr = round(pmax(pdr, abs(sample(noise_d))))
    )
  # Remove missing
  if (missing != 0) {
    missing1 = min(max(abs(missing), 0),0.5)
    if (missing1 != missing)
      warning("Fraction of missing was set to ", missing1)
    data = data[-sample.int(nrow(data), nrow(data)*missing1),]
  }

  # Add descriptor as comment
  kb = sprintf("beta = %.2f\U00B1%.2f", beta_mean,  beta_std)
  sf = ifelse(is.null(student_t_df) || student_t_df == 0, "Gaussian",
              paste0("Student-t ", student_t_df," df"))
  comment = sprintf("%.0f records,  m = %.0f\U00B1%.0f, k =  %.3f\U00B1%.3f, %s,\n%s noise amplitude = %.0f, %.0f%% missing",
                    n_records, m_mean, m_std, k_mean, k_std, kb,
                    sf, noise, 100*missing)
  comment(data) = comment
  list(record = rec, data = data)
}


if (FALSE) {
  n_records = 10
  m_mean = 40
  m_std = 10
  k_mean = 0.01
  k_std = 0.003
  beta_mean = 2
  beta_std = 0.2
  noise = 10
  missing = 0.1
  student_t_df = 2
  dose = 100
  max_minute = 200
#  library(dplyr)
#  library(breathtestcore)
#  library(assertthat)
}

