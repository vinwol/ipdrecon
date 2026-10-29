# Test for R script util_ipd.R

get_clean_digitized_data_lung_veteran <- function() {
  time <-
    c(
      0.530035335689035,
      0.530035335689035,
      1.59010600706712,
      3.18021201413427,
      4.24028268551235,
      6.89045936395759,
      7.95053003533569,
      11.13074204947,
      11.660777385159,
      12.7208480565371,
      14.8409893992933,
      15.9010600706714,
      18.0212014134275,
      19.0812720848056,
      20.1413427561837,
      20.6713780918728,
      22.2614840989399,
      23.8515901060071,
      25.4416961130742,
      27.0318021201413
    )
  prob <- c(
    0.999998788213682,
    0.986281367088853,
    0.979420232953802,
    0.971186144919949,
    0.964325010784898,
    0.94237107805358,
    0.913562070118804,
    0.898468059736218,
    0.891606925601167,
    0.876516550577537,
    0.861424963767589,
    0.847702695497487,
    0.820841561362436,
    0.818888840417436,
    0.803797253607488,
    0.790077408910023,
    0.774987033886393,
    0.768124687965023,
    0.753031889368756,
    0.731080380210075
  )
  df <- data.frame(time = time, prob = prob)
  return(df)
}

testthat::test_that("Probability range", {
  digitized_data <- get_clean_digitized_data_lung_veteran()
  surv_time <- digitized_data[,1]
  trisk <- seq(from=0, to=25, by=5)
  testthat::expect_equal(get_lower_indices(surv_time, trisk), c(1,6,8,12,15,19))
  testthat::expect_equal(get_upper_indices(surv_time, trisk), c(5,7,11,14,18,20))
})

# Tests for the row order of digitized points.
# Digitization tools return points in click order, not in time order.

# Reads the full lung veteran curve in file order (96 points, sorted by time).
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

testthat::test_that("Index helpers keep their values for sorted time", {
  curve <- get_lung_veteran_curve()
  trisk <- seq(from=0, to=550, by=50)
  testthat::expect_equal(get_lower_indices(curve$time, trisk),
                         c(1,34,53,68,76,81,86,88,93,94,95,95))
  testthat::expect_equal(get_upper_indices(curve$time, trisk),
                         c(33,52,67,75,80,85,87,92,93,94,94,96))
})

testthat::test_that("Index helpers return the sorted-time positions for any row order", {
  trisk <- seq(from=0, to=25, by=5)
  short_curve <- get_clean_digitized_data_lung_veteran()
  short_permutations <- get_row_permutations(nrow(short_curve))
  for (name in names(short_permutations)) {
    shuffled_time <- short_curve$time[short_permutations[[name]]]
    testthat::expect_equal(get_lower_indices(shuffled_time, trisk), c(1,6,8,12,15,19), info = name)
    testthat::expect_equal(get_upper_indices(shuffled_time, trisk), c(5,7,11,14,18,20), info = name)
  }
  curve <- get_lung_veteran_curve()
  trisk <- seq(from=0, to=550, by=50)
  permutations <- get_row_permutations(nrow(curve))
  for (name in names(permutations)) {
    shuffled_time <- curve$time[permutations[[name]]]
    testthat::expect_equal(get_lower_indices(shuffled_time, trisk),
                           c(1,34,53,68,76,81,86,88,93,94,95,95), info = name)
    testthat::expect_equal(get_upper_indices(shuffled_time, trisk),
                           c(33,52,67,75,80,85,87,92,93,94,94,96), info = name)
  }
})

testthat::test_that("prepare_time_interval_data keeps its values for sorted time", {
  short_curve <- get_clean_digitized_data_lung_veteran()
  trisk <- seq(from=0, to=25, by=5)
  nrisk <- c(50, 40, 30, 20, 10, 5)
  df <- prepare_time_interval_data(short_curve$time, trisk, nrisk,
                                   c(1,6,8,12,15,19), c(5,7,11,14,18,20))
  testthat::expect_equal(df$time_lower, c(0.5300, 6.8905, 11.1307, 15.9011, 20.1413, 25.4417))
  testthat::expect_equal(df$time_upper, c(4.2403, 7.9505, 14.8410, 19.0813, 23.8516, 27.0318))
})

testthat::test_that("prepare_time_interval_data reads positions in time-ascending order for any row order", {
  short_curve <- get_clean_digitized_data_lung_veteran()
  trisk <- seq(from=0, to=25, by=5)
  nrisk <- c(50, 40, 30, 20, 10, 5)
  expected <- prepare_time_interval_data(short_curve$time, trisk, nrisk,
                                         c(1,6,8,12,15,19), c(5,7,11,14,18,20))
  permutations <- get_row_permutations(nrow(short_curve))
  for (name in names(permutations)) {
    shuffled_time <- short_curve$time[permutations[[name]]]
    # Positions of the sorted file order.
    testthat::expect_equal(
      prepare_time_interval_data(shuffled_time, trisk, nrisk,
                                 c(1,6,8,12,15,19), c(5,7,11,14,18,20)),
      expected, info = paste(name, "with literal indices"))
    # Positions from the helpers, called on the shuffled vector.
    testthat::expect_equal(
      prepare_time_interval_data(shuffled_time, trisk, nrisk,
                                 get_lower_indices(shuffled_time, trisk),
                                 get_upper_indices(shuffled_time, trisk)),
      expected, info = paste(name, "with helper indices"))
  }
})

testthat::test_that("fix_non_decreasing_survival_probabilities returns aligned pairs in time order", {
  fixed <- fix_non_decreasing_survival_probabilities(c(0, 10, 5, 20), c(1.0, 0.7, 0.8, 0.6))
  testthat::expect_equal(fixed, data.frame(time = c(0, 5, 10, 20), prob = c(1.0, 0.8, 0.7, 0.6)))
})

testthat::test_that("fix_non_decreasing_survival_probabilities carries a rising probability forward", {
  fixed <- fix_non_decreasing_survival_probabilities(c(0, 5, 10, 20), c(1.0, 0.7, 0.8, 0.6))
  testthat::expect_equal(fixed, data.frame(time = c(0, 5, 10, 20), prob = c(1.0, 0.7, 0.7, 0.6)))
})

testthat::test_that("fix_non_decreasing_survival_probabilities sorts tied times by probability descending", {
  fixed <- fix_non_decreasing_survival_probabilities(c(0, 5, 5, 10), c(1.0, 0.7, 0.9, 0.5))
  testthat::expect_equal(fixed, data.frame(time = c(0, 5, 5, 10), prob = c(1.0, 0.9, 0.7, 0.5)))
})

testthat::test_that("fix_non_decreasing_survival_probabilities leaves a valid curve unchanged for any row order", {
  curve <- get_lung_veteran_curve()
  permutations <- get_row_permutations(nrow(curve))
  for (name in names(permutations)) {
    idx <- permutations[[name]]
    testthat::expect_equal(fix_non_decreasing_survival_probabilities(curve$time[idx], curve$prob[idx]),
                           curve, info = name)
  }
})

testthat::test_that("fix_non_decreasing_survival_probabilities checks lengths and accepts empty input", {
  testthat::expect_error(fix_non_decreasing_survival_probabilities(c(0, 5, 10), c(1.0, 0.7)),
                         "Input vectors 'time' and 'prob' must have the same length.", fixed = TRUE)
  empty <- fix_non_decreasing_survival_probabilities(numeric(0), numeric(0))
  testthat::expect_equal(nrow(empty), 0)
  testthat::expect_equal(names(empty), c("time", "prob"))
})

testthat::test_that("fix_non_increasing_survival_times returns aligned pairs in probability order", {
  fixed <- fix_non_increasing_survival_times(c(5, 0, 20, 10), c(0.7, 1.0, 0.6, 0.8))
  testthat::expect_equal(fixed, data.frame(time = c(0, 10, 10, 20), prob = c(1.0, 0.8, 0.7, 0.6)))
})

testthat::test_that("fix_non_increasing_survival_times carries a decreasing time forward", {
  fixed <- fix_non_increasing_survival_times(c(0, 10, 5, 20), c(1.0, 0.8, 0.7, 0.6))
  testthat::expect_equal(fixed, data.frame(time = c(0, 10, 10, 20), prob = c(1.0, 0.8, 0.7, 0.6)))
})

testthat::test_that("fix_non_increasing_survival_times sorts tied probabilities by time ascending", {
  fixed <- fix_non_increasing_survival_times(c(0, 12, 8, 20), c(1.0, 0.8, 0.8, 0.6))
  testthat::expect_equal(fixed, data.frame(time = c(0, 8, 12, 20), prob = c(1.0, 0.8, 0.8, 0.6)))
})

testthat::test_that("fix_non_increasing_survival_times leaves a valid curve unchanged for any row order", {
  curve <- get_lung_veteran_curve()
  permutations <- get_row_permutations(nrow(curve))
  for (name in names(permutations)) {
    idx <- permutations[[name]]
    testthat::expect_equal(fix_non_increasing_survival_times(curve$time[idx], curve$prob[idx]),
                           curve, info = name)
  }
})

testthat::test_that("fix_non_increasing_survival_times checks lengths and accepts empty input", {
  testthat::expect_error(fix_non_increasing_survival_times(c(0, 5, 10), c(1.0, 0.7)),
                         "Input vectors 'time' and 'prob' must have the same length!", fixed = TRUE)
  empty <- fix_non_increasing_survival_times(numeric(0), numeric(0))
  testthat::expect_equal(nrow(empty), 0)
  testthat::expect_equal(names(empty), c("time", "prob"))
})
