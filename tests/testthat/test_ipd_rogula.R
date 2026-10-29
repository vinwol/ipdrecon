# Tests for R script ipd_rogula.R

testthat::test_that("Valid input test", {
  digitized_data <- read.csv('survival_lung_veteran_digitized.csv', header = FALSE)
  time <- digitized_data[,1]
  prob <- digitized_data[,2]
  n <- 137
  cens.t <- NA
  ipd <- get_ipd_rogula(n, time, prob, cens.t)
  testthat::expect_equal(round(as.numeric(ipd[[1]][1,]),4), round(c(0.5300353,1),4))
  cens.t <- c(54.1, 125.7, 555.3) 
  ipd <- get_ipd_rogula(n, time, prob, cens.t)
  testthat::expect_equal(round(as.numeric(ipd[[1]][1,]),4), round(c(0.5300353,1),4))
})

# Tests for the row order of digitized points.
# Digitization tools return points in click order, not in time order.

# Lung veteran curve in file order (96 points, sorted by time, one tied time).
get_lung_veteran_curve <- function() {
  digitized_data <- read.csv('survival_lung_veteran_digitized.csv', header = FALSE)
  return(data.frame(time = digitized_data[,1], prob = digitized_data[,2]))
}

# Fixed row permutations of 1..n: reversed order and two seeded shuffles.
get_row_permutations <- function(n) {
  set.seed(1)
  shuffle_seed_1 <- sample(n)
  set.seed(2)
  shuffle_seed_2 <- sample(n)
  return(list(reversed = rev(seq_len(n)),
              seed_1 = shuffle_seed_1,
              seed_2 = shuffle_seed_2))
}

# Runs get_ipd_rogula() with validator output silenced.
run_rogula <- function(n, time, prob, cens_t = NA) {
  utils::capture.output(res <- get_ipd_rogula(n, time, prob, cens_t))
  return(res)
}

# Median survival time of the Kaplan-Meier curve of the reconstructed IPD.
get_km_median <- function(ipd) {
  fit <- survival::survfit(survival::Surv(time, event) ~ 1, data = ipd)
  return(unname(summary(fit)$table["median"]))
}

testthat::test_that("File-order reconstruction has 137 patients, 134 events and median 80.03534", {
  curve <- get_lung_veteran_curve()
  res <- run_rogula(137, curve$time, curve$prob)
  ipd <- res[[1]]
  testthat::expect_equal(nrow(ipd), 137)
  testthat::expect_equal(sum(ipd$event), 134)
  testthat::expect_equal(get_km_median(ipd), 80.03534, tolerance = 1e-6)
  testthat::expect_equal(round(as.numeric(ipd[1,]),4), round(c(0.5300353,1),4))
  testthat::expect_equal(res[[2]], curve)
})

testthat::test_that("Shuffled rows give the file-order reconstruction", {
  curve <- get_lung_veteran_curve()
  res_file_order <- run_rogula(137, curve$time, curve$prob)
  res_file_order_cens <- run_rogula(137, curve$time, curve$prob, c(54.1, 125.7, 555.3))
  permutations <- get_row_permutations(nrow(curve))
  for (name in names(permutations)) {
    idx <- permutations[[name]]
    testthat::expect_equal(run_rogula(137, curve$time[idx], curve$prob[idx]),
                           res_file_order, info = name)
    # Censoring times are sorted on their own, so their order does not matter either.
    testthat::expect_equal(run_rogula(137, curve$time[idx], curve$prob[idx], c(555.3, 54.1, 125.7)),
                           res_file_order_cens, info = paste(name, "with censoring times"))
  }
})

testthat::test_that("Tied times are ordered by probability descending", {
  curve <- get_lung_veteran_curve()
  # The first two points share one time (a vertical drop). Swap them.
  idx <- c(2, 1, 3:nrow(curve))
  res <- run_rogula(137, curve$time[idx], curve$prob[idx])
  testthat::expect_equal(res[[2]][1:2, "prob"], curve$prob[1:2])
  testthat::expect_equal(res, run_rogula(137, curve$time, curve$prob))
})

testthat::test_that("A rising probability is carried forward, not swapped with its neighbour", {
  # In time order the probabilities are 0.9, 0.8, 0.85, 0.6. The rise at time 30 is a
  # digitization error. The correction replaces it by the preceding value 0.8.
  time <- c(10, 20, 30, 40)
  prob <- c(0.9, 0.8, 0.85, 0.6)
  idx <- c(3, 1, 4, 2)
  res <- run_rogula(20, time[idx], prob[idx])
  testthat::expect_equal(res[[2]], data.frame(time = c(10, 20, 30, 40), prob = c(0.9, 0.8, 0.8, 0.6)))
  res_corrected <- run_rogula(20, time, c(0.9, 0.8, 0.8, 0.6))
  testthat::expect_equal(res[[1]], res_corrected[[1]])
  testthat::expect_equal(nrow(res[[1]]), 20)
})

testthat::test_that("Patient-count check stops when n is not a whole number", {
  curve <- get_lung_veteran_curve()
  # rep() truncates the 137.5 patients of the shell table to 137 rows.
  testthat::expect_error(run_rogula(137.5, curve$time, curve$prob),
                         "contains 137 patients, but n is 137.5", fixed = TRUE)
})

testthat::test_that("Patient-count check stays silent for a whole n", {
  curve <- get_lung_veteran_curve()
  testthat::expect_error(run_rogula(137, curve$time, curve$prob), NA)
  testthat::expect_error(run_rogula(137, curve$time, curve$prob, c(54.1, 125.7, 555.3)), NA)
})



