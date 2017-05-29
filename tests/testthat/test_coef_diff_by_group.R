context("Coeffient differences by group")

library(dplyr)
data("usz_13c")
data = usz_13c %>%
  dplyr::filter( patient_id %in%
   c("norm_001", "norm_002", "norm_003", "norm_004", "pat_001", "pat_002","pat_003")) %>%
   cleanup_data()
fit = nls_fit(data)

test_that("Result with default parameters is tbl_df with required columns",{
  cf = coef_diff_by_group(fit)
  expect_is(cf, "tbl_df")
  expect_identical(ncol(cf), 7L)
  expect_identical(nrow(cf), 27L)
  expect_lt(min(cf$p.value), 5e-8)
  expect_equal(unique(cf$groups),
     c("patient - liquid_normal", "solid_normal - liquid_normal", "solid_normal - patient"))
})

test_that("Result with Dunnett contrast only returns 2 groups",{
  cf = coef_diff_by_group(fit, mcp_group = "Dunnett")
  expect_is(cf, "tbl_df")
  expect_identical(ncol(cf), 7L)
  expect_identical(nrow(cf), 18L)
  expect_lt(min(cf$p.value), 5.e-8)
  expect_equal(unique(cf$groups),
       c("patient - liquid_normal", "solid_normal - liquid_normal" ))
})

test_that("Only Dunnett and Tukey are allowed for mcp_group",{
  expect_error(coef_diff_by_group(fit, mcp_group = "blub"))
})

test_that("Fit must be of class breathtestfit",{
  expect_error(coef_diff_by_group(NULL))
})

test_that("Correct Dunnett contrast when reference value is given",{
  cf = coef_diff_by_group(fit, mcp_group = "Dunnett", reference_group = "patient")
  expect_equal(unique(cf$groups),
               c("liquid_normal - patient", "solid_normal - patient"  ))
})

test_that("nlme_fit can be used to compute coefficients",{
  skip_on_cran()
  fit = nlme_fit(data)
  cf = coef_diff_by_group(fit)
  expect_is(cf, "tbl_df")
})

