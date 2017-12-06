context("Coefficient differences by group")

suppressPackageStartupMessages(library(dplyr))
data("usz_13c")
data = usz_13c %>%
  dplyr::filter( patient_id %in%
   c("norm_001", "norm_002", "norm_004", "norm_007", "pat_004", "pat_012", "pat_023")) %>%
   cleanup_data()
fit = nls_fit(data)
cm = comment(fit$data)

test_that("Result with default parameters is tbl_df with required columns",{
  cf = coef_diff_by_group(fit)
  expect_is(cf, "tbl_df")
  expect_is(cf, "coef_diff_by_group")
  expect_identical(ncol(cf), 7L)
  expect_identical(nrow(cf), 54L)
  expect_lt(min(cf$p.value), 5e-8)
  expect_equal(length(unique(cf$groups)), 6)
  expect_equal(comment(cf), cm)
})

test_that("Result with Dunnett contrast only returns 3 groups",{
  cf = coef_diff_by_group(fit, mcp_group = "Dunnett")
  expect_is(cf, "tbl_df")
  expect_is(cf, "coef_diff_by_group")
  expect_identical(ncol(cf), 7L)
  expect_identical(nrow(cf), 27L)
  expect_lt(min(cf$p.value), 5.e-8)
  expect_equal(unique(cf$groups),
       c("liquid_patient - liquid_normal", "solid_normal - liquid_normal",
         "solid_patient - liquid_normal" ))
})

test_that("Only Dunnett and Tukey are allowed for mcp_group",{
  expect_error(coef_diff_by_group(fit, mcp_group = "blub"))
})

test_that("Fit must be of class breathtestfit",{
  expect_error(coef_diff_by_group(NULL))
})

test_that("Correct Dunnett contrast when reference value is given",{
  coef(fit)$group
  cf = coef_diff_by_group(fit, mcp_group = "Dunnett", reference_group = "solid_patient")
  expect_equal(unique(cf$groups),
     c("liquid_normal - solid_patient",
       "liquid_patient - solid_patient",
       "solid_normal - solid_patient"))
})

test_that("nlme_fit can be used to compute coefficients",{
  skip_on_cran()
  fit = nlme_fit(data)
  cf = coef_diff_by_group(fit)
  expect_is(cf, "tbl_df")
  expect_is(cf, "coef_diff_by_group")
})


digs = function(x){
  nchar(stringr::str_replace_all(paste(abs(x)), "[0\\.]",""))
}

test_that("Options digits is served",{
  options(digits = 4)
  cf = coef_diff_by_group(fit)
  expect_is(cf, "tbl_df")
  expect_lte(digs(cf[[1,"estimate"]]) ,4L)
  expect_lte(digs(cf[[1,"conf.low"]]), 4L)
  expect_lte(digs(cf[[1,"conf.high"]]),4L)
})


test_that("NULL returned if there is only one group", {
  data = usz_13c %>%
    dplyr::filter( patient_id %in%
                   c("pat_001", "pat_002","pat_003")) %>%
  cleanup_data()
  fit = nls_fit(data)
  cf = coef_diff_by_group(fit)
  expect_null(cf)
})
