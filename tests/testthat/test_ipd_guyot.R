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

# Tests for the row order of digitized points.
# Digitization tools return points in click order, not in time order.

# Lung veteran curve in file order (96 points, sorted by time, one tied time)
# with its numbers at risk.
get_lung_veteran_guyot_input <- function() {
  digitized_data <- read.csv('survival_lung_veteran_digitized.csv', header = FALSE)
  return(list(time = digitized_data[,1],
              prob = digitized_data[,2],
              trisk = seq(from=0, to=550, by=50),
              nrisk = c(137,84,55,34,25,18,13,11,6,5,4,4),
              tot_events = 126))
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

# Runs get_ipd_guyot() as the README does: lower and upper come from the helpers,
# called on the same time vector that is passed on. Validator output is silenced.
run_guyot <- function(input,
                      time = input$time,
                      prob = input$prob,
                      lower = get_lower_indices(time, input$trisk),
                      upper = get_upper_indices(time, input$trisk)) {
  utils::capture.output(
    res <- get_ipd_guyot(time, prob, input$trisk, input$nrisk, lower, upper, input$tot_events)
  )
  return(res)
}

# Median survival time of the Kaplan-Meier curve of the reconstructed IPD.
get_km_median <- function(ipd) {
  fit <- survival::survfit(survival::Surv(time, event) ~ 1, data = ipd)
  return(unname(summary(fit)$table["median"]))
}

testthat::test_that("File-order reconstruction has 137 patients, 126 events and median 80.03534", {
  input <- get_lung_veteran_guyot_input()
  res <- run_guyot(input)
  ipd <- res[[1]]
  testthat::expect_equal(nrow(ipd), 137)
  testthat::expect_equal(sum(ipd$event), 126)
  testthat::expect_equal(get_km_median(ipd), 80.03534, tolerance = 1e-6)
  testthat::expect_equal(round(as.numeric(ipd[1,]),4), round(c(0.5300353,1),4))
  testthat::expect_equal(res[[3]], data.frame(time = input$time, prob = input$prob))
})

testthat::test_that("Shuffled rows with helper indices give the file-order reconstruction", {
  input <- get_lung_veteran_guyot_input()
  res_file_order <- run_guyot(input)
  permutations <- get_row_permutations(length(input$time))
  for (name in names(permutations)) {
    idx <- permutations[[name]]
    res <- run_guyot(input, time = input$time[idx], prob = input$prob[idx])
    testthat::expect_equal(res[[1]], res_file_order[[1]], info = paste(name, "IPD"))
    testthat::expect_equal(res, res_file_order, info = paste(name, "all four elements"))
  }
})

testthat::test_that("Shuffled rows with seed 1 give 137 patients, 126 events and median 80.03534", {
  input <- get_lung_veteran_guyot_input()
  idx <- get_row_permutations(length(input$time))$seed_1
  ipd <- run_guyot(input, time = input$time[idx], prob = input$prob[idx])[[1]]
  testthat::expect_equal(nrow(ipd), 137)
  testthat::expect_equal(sum(ipd$event), 126)
  testthat::expect_equal(get_km_median(ipd), 80.03534, tolerance = 1e-6)
})

testthat::test_that("Shuffled rows with indices from the sorted file order give the file-order reconstruction", {
  input <- get_lung_veteran_guyot_input()
  res_file_order <- run_guyot(input)
  lower_sorted <- get_lower_indices(input$time, input$trisk)
  upper_sorted <- get_upper_indices(input$time, input$trisk)
  permutations <- get_row_permutations(length(input$time))
  for (name in names(permutations)) {
    idx <- permutations[[name]]
    res <- run_guyot(input,
                     time = input$time[idx],
                     prob = input$prob[idx],
                     lower = lower_sorted,
                     upper = upper_sorted)
    testthat::expect_equal(res, res_file_order, info = name)
  }
})

testthat::test_that("df_input holds the sorted pairs, each time with its own probability", {
  input <- get_lung_veteran_guyot_input()
  idx <- get_row_permutations(length(input$time))$seed_1
  # Indices of the sorted file order, so that only the pairing of time and prob is tested.
  df_input <- run_guyot(input,
                        time = input$time[idx],
                        prob = input$prob[idx],
                        lower = get_lower_indices(input$time, input$trisk),
                        upper = get_upper_indices(input$time, input$trisk))[[3]]
  testthat::expect_equal(df_input, data.frame(time = input$time, prob = input$prob))
})

testthat::test_that("Tied times are ordered by probability descending", {
  input <- get_lung_veteran_guyot_input()
  # The first two points share one time (a vertical drop). Swap them.
  idx <- c(2, 1, 3:length(input$time))
  res_file_order <- run_guyot(input)
  res <- run_guyot(input, time = input$time[idx], prob = input$prob[idx])
  testthat::expect_equal(res[[3]][1:2, "prob"], input$prob[1:2])
  testthat::expect_equal(res, res_file_order)
})

testthat::test_that("Patient-count check stops when the reconstruction differs from nrisk[1]", {
  input <- get_lung_veteran_guyot_input()
  # Digitization offsets 0.010 * (+1, 0, -1, 0, ...) and 0.012 * (...) make the
  # algorithm assign more patients than exist at time zero.
  offset_pattern <- rep(c(1, 0, -1, 0), length.out = length(input$prob))
  fixtures <- data.frame(amplitude = c(0.010, 0.012), patients = c(140, 141))
  for (i in seq_len(nrow(fixtures))) {
    noisy_prob <- pmin(input$prob + fixtures$amplitude[i] * offset_pattern, 1)
    expected_message <- sprintf("contains %d patients, but nrisk[1] is 137", fixtures$patients[i])
    testthat::expect_error(run_guyot(input, prob = noisy_prob), expected_message, fixed = TRUE)
  }
})

testthat::test_that("Patient-count check stays silent on a consistent reconstruction", {
  input <- get_lung_veteran_guyot_input()
  testthat::expect_error(run_guyot(input), NA)
  # A small offset that keeps the count at 137.
  offset_pattern <- rep(c(1, 0, -1, 0), length.out = length(input$prob))
  noisy_prob <- pmin(input$prob + 0.008 * offset_pattern, 1)
  testthat::expect_error(run_guyot(input, prob = noisy_prob), NA)
  testthat::expect_equal(nrow(run_guyot(input, prob = noisy_prob)[[1]]), 137)
})



