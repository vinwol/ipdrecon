# Tests for R script ipd_rogula.R

testthat::test_that("Valid input test", {
  digitized_data <- read.csv('survival_lung_veteran_digitized.csv', header = FALSE)
  time <- digitized_data[,1]
  prob <- digitized_data[,2]
  n <- 137
  cens.t <- NA
  ipd <- get_ipd_rogula(n, time, prob, cens.t)
  testthat::expect_equal(round(as.numeric(ipd[1,]),4), round(c(0.5300353,1),4))
  cens.t <- c(54.1, 125.7, 555.3) 
  ipd <- get_ipd_rogula(n, time, prob, cens.t)
  testthat::expect_equal(round(as.numeric(ipd[1,]),4), round(c(0.5300353,1),4))
})



