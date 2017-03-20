context("recognize breathtest file format")

test_that("Bluck-Coward data for testing of parameter functions" , {
  
  cf = data.frame(
    method = rep(c("nls", "bayesian")),
    group = rep(c("lean", "obese"),each=2),
    k =    c(0.576,0.606,0.529,0.608),
    beta = c(5.24, 5.79, 5.95, 7.54),
    t12 =  c(3.67, 3.63, 4.23, 3.99),
    t12in = c(2.076, 2.110, 2.422, 2.466),
    tlag = c(2.88, 2.88, 3.34, 3.26),
    tlagin = c(1.632, 1.724, 1.92, 2.101)
  )
  t50maes = t50maes(cf)
  expect_lt(max(abs(t50maes - cf$t12)/cf$t12), 0.014) 
  
  t50bluck_coward = t50bluck_coward(cf3)
  expect_lt(max(abs(t50bluck_coward - cf$t12in)/cf$t12in), 0.035)
  
  t_lag_maes = t_lag_maes(cf3)
  expect_lt(max(abs(t_lag_maes - cf$tlag)/cf$tlag), 0.02)
  
  t_lag_bluck_coward = t_lag_bluck_coward(cf3)
  # Bluck-Cowards estimate not very exact
  expect_lt(max(abs(t_lag_bluck_coward - cf$tlagin)/cf$tlagin), 0.09)
})

