# Tests for R script ipd_guyot.R

testthat::test_that("Valid input test", {
  digitized_data <- read.csv('survival_lung_veteran_digitized.csv', header = FALSE)
  time <- digitized_data[,1]
  prob <- digitized_data[,2]
  trisk <- seq(from=0, to=550, by=50)
  nrisk <- c(137,84,55,34,25,18,13,11,6,5,4,4)
  tot_events <- 126
  lower <- get_lower_indices(time, trisk) 
  upper <- get_upper_indices(time, trisk) 
  res <- get_ipd_guyot(time,
                       prob,
                       trisk,
                       nrisk,
                       lower,
                       upper,
                       tot_events)
  ipd <- res[[1]]
  testthat::expect_equal(round(as.numeric(ipd[1,]),4), round(c(0.5300353,1),4))
})



