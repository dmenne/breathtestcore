#' Simulate 13C breath time series data
#' 
#' Generates simulated breath test data, optionally with errors. If none of the three
#' standard deviations \code{m_std, k_std, beta_std} is given, an empirical covariance
#' matrix from USZ breath test data is used. If any of the standard deviations is given,
#' default values for the others will be used.
#' @param n_records Number of records
#' @param m_mean,m_std Mean and between-record standard deviation of parameter m giving metabolized fraction.
#' @param k_mean,k_std Mean and between-record standard deviation of parameter k, in units of 1/minutes.
#' @param beta_mean,beta_std Mean and between-record standard deviations of lag parameter beta
#' @param noise Standard deviation of normal noise when \code{student_t_df = NULL}; scaling of noise when student_t_df >= 2.
#' @param cov Covariance matrix, default NULL, i.e. not used. If given, overrides 
#' standard deviation settings.
#' @param student_t_df When NULL (default), Gaussian noise is added; when >= 2, Student_t distributed noise is added, which generates more realistic outliers. Values from 2 to 5 are useful, when higher values are used the result comes close to that of Gaussian noise. Values below 2 are truncated to 2.
#' @param missing When 0 (default), all curves have the same number of data points. When > 0, this is the fraction of points that were removed randomly to simulate missing 
#' @param seed Optional seed; not set if seed = NULL (default)
#' @param dose Octanoate/acetate dose, almost always 100 mg, which is also the default
#' @param first_minute First sampling time. Do not use 0 here, some algorithms do not 
#' converge when data near 0 are passed.
#' @param step_minute Inter-sample interval for breath test
#' @param max_minute Maximal time in minutes.
#'
#' @return A list of class simulated_breathtest_data with 2  elements:
#' \describe{
#'   \item{record}{Data frame with columns
#'     \code{patient_id(chr), m, k, beta, t50} giving the effective parameters 
#'     for the individual patient record.}
#'   \item{data}{Data frame with columns
#'     \code{patient_id(chr), minute(dbl), pdr(dbl)} giving the
#'      time series and grouping parameters.}
#'  }
#'  A comment is attached to the return value that can be used as a title for plotting.
#' @examples
#' library(ggplot2)
#' pdr = simulate_breathtest_data(n_records = 4, seed = 4711, missing = 0.3,
#'        student_t_df = 2, noise = 1.5) # Strong outliers
#' #
#' str(pdr, 1)  
#' #
#' pdr$record # The "correct" parameters
#' #
#' # Explicit plotting
#' ggplot(pdr$data, aes(x = minute, y = pdr)) + geom_point() +
#'   facet_wrap(~patient_id) + ggtitle(comment(pdr$data))
#' #
#' # Or use cleanup_data and null_fit for S3 plotting
#' plot(null_fit(cleanup_data(pdr$data)))
#' @export
#'
simulate_breathtest_data = function(
  n_records = 10,
  m_mean = 40, m_std = NULL,
  k_mean = 0.01,  k_std = NULL,
  beta_mean = 2, beta_std = NULL,
  noise = 1,
  cov = NULL,
  student_t_df = NULL,
  missing = 0,
  seed = NULL,
  dose = 100,
  first_minute = 5,
  step_minute = 15,
  max_minute = 155) {
  
  # Covariance matrix does not work for n_records <= 3
  use_cov = (n_records > 3) & (!is.null(cov) |  
    ((is.null(m_std) & is.null(k_std) & is.null(beta_std))))
  # Use empirical covariance structure from USZ data set when not explicitly given
  cov = NULL
  if (use_cov & is.null(cov)) cov = 
    structure(c(188, -0.026, -2.04, -0.026, 7.74e-06, 0.000477, -2.04, 0.000477, 0.182),
    .Dim = c(3L, 3L), .Dimnames = list(c("m", "k","beta"), c("m", "k", "beta")))

  pdr = . = NULL # Hack to avoid notes
  if (!is.null(seed))
    set.seed(seed)

  assert_that(n_records >= 1)
  if (!use_cov) {
    # default values if not explicitly given
    if (is.null(m_std)) m_std = m_mean/4
    if (is.null(k_std)) k_std = k_mean/4
    if (is.null(beta_std)) beta_std = beta_mean/5
    
    assert_that(m_std >= 0)
    assert_that(k_std >= 0)
    assert_that(beta_std >= 0)
  
    assert_that(m_mean > 3*m_std)
    assert_that(k_mean > 3*k_std)
    assert_that(beta_mean > 3*beta_std)
  }
  
  assert_that(max_minute %/% step_minute > 5)

  minute  = seq(first_minute, max_minute, by = step_minute)
  # Record
  rec = tibble(patient_id = sprintf("rec_%02d", 1:n_records))
  if (!use_cov) {
    rec = cbind(rec, 
      m = round(rnorm(n_records, m_mean, m_std)),
      k = pmax(rnorm(n_records, k_mean, k_std), k_mean/3),
      beta = pmax(rnorm(n_records, beta_mean, beta_std), beta_mean/3)
    )
  } else {
    cc = MASS::mvrnorm(n_records, mu = c(m_mean, k_mean, beta_mean), 
                       Sigma = cov, empirical = TRUE)
    rec = cbind(rec, cc)
  }
  attr(rec, "cov") = cov
  rec$t50_maes_ghoos = t50_maes_ghoos(rec)  

  # Noise term
  if (is.null(student_t_df) || student_t_df == 0 ) {
    if (noise == 0)
      warning("With noise == 0, non-linear fits might fail.")
    noise_d = rnorm(nrow(rec)*length(minute), 0, noise)
  } else {
    if (student_t_df < 2) {
      student_t_df = 2
      warning("Degrees of freedom of Student-t noise was set to 2")
    }
    noise_d = noise*rt(nrow(rec)*length(minute), student_t_df)
  }
  # Data
  data = rec %>%
    rowwise() %>%
    do(
      tibble(
        patient_id = .$patient_id,
        minute = minute,
        pdr = breathtestcore::exp_beta(minute = minute,  
                  dose = dose , m = .$m, k = .$k, beta = .$beta)
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
  sf = ifelse(is.null(student_t_df) || student_t_df == 0, "Gaussian",
              paste0("Student-t ", student_t_df," df"))
  if (!use_cov) {
    kb = sprintf("beta = %.2f\U00B1%.2f", beta_mean,  beta_std)
    comment = sprintf("%.0f records,  m = %.0f\U00B1%.0f, k =  %.3f\U00B1%.3f, %s,\n%s noise amplitude = %.0f, %.0f%% missing",
                    n_records, m_mean, m_std, k_mean, k_std, kb,
                    sf, noise, 100*missing)
  } else {
    comment = sprintf("%.0f records,  m = %.0f, k =  %.3f, beta =  %.1f, cov-matrix, 
 %s noise amplitude = %.0f, %.0f%% missing",
                      n_records, m_mean, k_mean, beta_mean, 
                      sf, noise, 100*missing)
    
  }
  comment(data) = comment
  ret = list(record = rec, data = data)
  class(ret) = c("simulated_breathtest_data", "list")
  ret
}

