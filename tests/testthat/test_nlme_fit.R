test_that("Some data that cannot be fitted with nls_list also fail with nlme", {
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
  comment(data) = "comment"
  fit = nlme_fit(data)
  expect_s3_class(fit, "breathtestfit")
  expect_s3_class(fit, "breathtestnlmefit")
  expect_equal(comment(fit$data), "comment")
  expect_identical(names(fit), c("coef", "data", "nlme_fit"))
  cf = coef(fit)
  expect_equal(comment(cf), "comment")
  expect_equal(nrow(cf), 104)
  expect_identical(names(cf), c("patient_id", "group", "parameter", "method", "value"))
<<<<<<< HEAD
  expect_type(AIC(fit), "double" )
  expect_type(sigma(fit), "double" )
=======
  expect_type(AIC(fit), "numeric" )
  expect_type(sigma(fit), "numeric" )
>>>>>>> a48b9f194c1205de7cc75fd095d7dcc4294e1530
  expect_gt(sigma(fit), 0.5)
  # Check if subsampling done
  expect_equal(nrow(fit$data), 225)  
  expect_identical(names(fit$data), c("patient_id", "group", "minute", "pdr"))
  # Check summary
  s = summary(fit)
  expect_equal(comment(s), "comment")
  expect_identical(nrow(s), 13L)
  expect_identical(names(s),  c("patient_id", "group", "value"))
})

test_that("Two-group nlme fit returns valid result", {
  data("usz_13c")
  data = usz_13c %>%
    dplyr::filter( patient_id %in%
          c("norm_001", "norm_002", "norm_003", "norm_004", "norm_005", "norm_006")) %>%
    cleanup_data()
  fit = nlme_fit(data)
  expect_identical(names(fit), c("coef", "data", "nlme_fit"))
  cf = coef(fit)
  expect_equal(nrow(cf), 72)
  expect_identical(names(cf), c("patient_id", "group", "parameter", "method", "value"))
<<<<<<< HEAD
  expect_type(AIC(fit), "double" )
=======
  expect_type(AIC(fit), "numeric" )
>>>>>>> a48b9f194c1205de7cc75fd095d7dcc4294e1530
  expect_gt(sigma(fit), 0)
  expect_equal(unique(cf$group), c("liquid_normal", "solid_normal"))
  
  # Check if subsampling done
  expect_equal(nrow(fit$data), 123) # denser sampling early  
  expect_identical(names(fit$data), c("patient_id", "group", "minute", "pdr"))
})


test_that("Three-group nlme fit returns valid result", {
  data("usz_13c")
  data = usz_13c %>%
    dplyr::filter( patient_id %in%
     c("norm_001", "norm_002", "norm_003", "pat_001", "pat_003", "pat_016")) %>%
    breathtestcore::cleanup_data()
  fit_nlme = breathtestcore::nlme_fit(data)
  expect_identical(names(fit_nlme), c("coef", "data", "nlme_fit"))
  
  cf = coef(fit_nlme)
  expect_equal(nrow(cf), 64)
  expect_gt(sigma(fit_nlme), 0)
  expect_equal(unique(cf$group), c("liquid_normal", "solid_normal", "solid_patient"))
})  
