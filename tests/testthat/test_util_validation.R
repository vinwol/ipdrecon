# Tests for R script util_validation.R

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

get_unclean_digitized_data_lung_veteran_wrong_time <- function() {
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
      10.8409893992933,
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
    0.880841561362436,
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

get_unclean_digitized_data_lung_veteran_wrong_prob <- function() {
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
    0.840841561362436,
    0.818888840417436,
    0.803797253607488,
    0.790077408910023,
    0.774987033886393,
    0.868124687965023,
    0.753031889368756,
    0.731080380210075
  )
  df <- data.frame(time = time, prob = prob)
  return(df)
}

testthat::test_that("Numeric input", {
  data <- c(1,2,3)
  testthat::expect_output(check_valid_numbers(data), "Input consists of valid numbers.")
  data <- c(1,2,'3')
  testthat::expect_error(check_valid_numbers(data))
})

testthat::test_that("Probability range", {
  data <- c(0,0.1,0.5,1)
  testthat::expect_output(check_probabilities_for_range(data), "Survival probabilities are in the range 0 to 1.")
  data <- c(0,0.1,0.5,1.1,1.0)
  testthat::expect_error(check_probabilities_for_range(data))
})

testthat::test_that("Time of risk and number at risk information", {
  trisk <- seq(from=0, to=550, by=50)
  nrisk <- c(137,84,55,34,25,18,13,11,6,5,4,4)
  testthat::expect_output(check_risk_information(trisk, nrisk), "Time of risk and number of risk information is ok.")
  trisk <- seq(from=0, to=600, by=50)
  nrisk <- c(137,84,55,34,25,18,13,11,6,5,4,4)
  testthat::expect_error(check_risk_information(trisk, nrisk))
})

testthat::test_that("Check survival probabilities", {
  data <- get_clean_digitized_data_lung_veteran()
  testthat::expect_output(find_non_decreasing_probabilities(data$time, data$prob), 'Survival probabilities are descending.')
  data <- get_unclean_digitized_data_lung_veteran_wrong_prob()
  testthat::expect_output(find_non_decreasing_probabilities(data$time, data$prob), 'Non descending survival probabilities were detected!')
})

testthat::test_that("Check survival times", {
  data <- get_clean_digitized_data_lung_veteran()
  testthat::expect_output(find_non_increasing_survival_times(data$time, data$prob), 'Survival times are increasing.')
  data <- get_unclean_digitized_data_lung_veteran_wrong_time()
  testthat::expect_output(find_non_increasing_survival_times(data$time, data$prob), 'Non increasing survival times were detected!')
})





