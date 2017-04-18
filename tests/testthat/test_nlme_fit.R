context("Population fit with nlme")

test_that("Data that cannot be fitted with nls_list also fail with nlme", {
  # with this seed, cf[10] does not fit with nls_list
  data = cleanup_data(simulate_breathtest_data(seed = 100)$data)
  fit = nlme_fit(data)
  expect_null(fit$coef)
})

test_that("One-group nlme fit returns valid result", {
  data("usz_13c")
  data = usz_13c %>%
    dplyr::filter( patient_id %in%
      c("pat_001","pat_002","pat_003","pat_004","pat_005","pat_006",
       "norm_001", "norm_002", "norm_003", "norm_004", "norm_005", "norm_006"),
            group != "liquid_normal") %>%
    cleanup_data()
  fit = nlme_fit(data)
  expect_is(fit, "breathtestfit")
  expect_is(fit, "breathtestnlmefit")
  expect_identical(names(fit), c("coef", "data"))
  cf = coef(fit)
  expect_equal(nrow(cf), 96)
  expect_identical(names(cf), c("patient_id", "group", "parameter", "method", "value"))
  expect_is(AIC(fit), "numeric" )
  # Check if subsampling done
  expect_equal(nrow(fit$data), 197)  
  expect_identical(names(fit$data), c("patient_id", "group", "minute", "pdr"))
})

test_that("Two-group nlme fit returns valid result", {
  data("usz_13c")
  data = usz_13c %>%
    dplyr::filter( patient_id %in%
          c("norm_001", "norm_002", "norm_003", "norm_004", "norm_005", "norm_006")) %>%
    cleanup_data()
  fit = nlme_fit(data)
  expect_identical(names(fit), c("coef", "data"))
  cf = coef(fit)
  expect_equal(nrow(cf), 96)
  expect_identical(names(cf), c("patient_id", "group", "parameter", "method", "value"))
  expect_is(AIC(fit), "numeric" )

  # Check if subsampling done
  expect_equal(nrow(fit$data), 168) # denser sampling early  
  expect_identical(names(fit$data), c("patient_id", "group", "minute", "pdr"))
})

