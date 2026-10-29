#' This function provides a data.frame with all the relevant time interval information.
#'
#' @param time A vector of survival times. The vector does not need to be sorted.
#' The values in `lower` and `upper` are positions in the time-ascending order of `time`.
#' @param trisk A vector of times at which patients are at risk.
#' @param nrisk A vector of the number of patients at risk.
#' @param lower A vector that contains the lower time limits for each interval. 
#' It defines the starting point of each interval where calculations for survival estimates and events will be made. 
#' @param upper A The `upper` parameter is also a vector that contains the upper time limits for each interval. 
#' It defines the end point of each interval where calculations will stop. 
#' @return A data.frame with all the relevant time interval information. 
#' @export
prepare_time_interval_data <- function(time, trisk, nrisk, lower, upper) {
  # Positions in 'lower' and 'upper' refer to the time-ascending order.
  # order() keeps NA values (placed last), which sort() would drop.
  time <- time[order(time)]
  interval <- seq(1,length(trisk),1)
  time_lower <- round(time[lower],4)
  time_upper <- round(time[upper],4)
  df <- data.frame(interval = interval, 
                   lower = lower, 
                   upper = upper, 
                   time_lower = time_lower, 
                   time_upper = time_upper,
                   trisk = trisk, 
                   nrisk = nrisk)
  return(df)
}

#' This function uses a for loop to find the minimum index of elements in the `time` vector,
#' which are the survival times, that are greater than or equal to each element in the `trisk` vector, 
#' which are the temporal intervals.
#'
#' @details
#' The returned indices are positions in the time-ascending order of `time`,
#' which is the order that `get_ipd_guyot()` uses.
#' Pass them to `get_ipd_guyot()` together with the same `time`.
#'
#' @param time A vector of survival times. The vector does not need to be sorted.
#' @param trisk A vector of times at which patients are at risk.
#' @return A vector that contains the lower time limits for each interval. 
#' It defines the starting point of each interval where calculations for survival 
#' estimates and events will be made. 
#' The values are positions in the time-ascending order of `time`.
#' @export
get_lower_indices <- function(time, trisk) {
  # Positions refer to the time-ascending order.
  # order() keeps NA values (placed last), which sort() would drop.
  time <- time[order(time)]
  # Initialize an empty vector to store results 
  min_indices <- numeric(length(trisk))
  # Loop over each element in trisk.
  for (i in 1:length(trisk)) {
    # Find the index of the first element in time that is greater than or equal to trisk[i].
    min_index <- Inf
    for (j in 1:length(time)) {
      if (time[j] >= trisk[i]) {
        min_index <- j
        # Exit the loop once we find the index.
        break
      }
    }
    # Store the index in the results vector.
    min_indices[i] <- min_index
  }
  return(min_indices)
}  

#' This function uses a for loop to find the maximum index of elements in the `time` vector,
#' which are the survival times, that are less than or equal to each element in the `trisk` vector, 
#' which are the temporal intervals.
#'
#' @details
#' The returned indices are positions in the time-ascending order of `time`,
#' which is the order that `get_ipd_guyot()` uses.
#' Pass them to `get_ipd_guyot()` together with the same `time`.
#'
#' @param time A vector of survival times. The vector does not need to be sorted.
#' @param trisk A vector of times at which patients are at risk.
#' @return A vector that contains the upper time limits for each interval. 
#' It defines the end point of each interval where calculations will stop. 
#' The values are positions in the time-ascending order of `time`.
#' @export
get_upper_indices <- function(time, trisk) {
  # Positions refer to the time-ascending order.
  # order() keeps NA values (placed last), which sort() would drop.
  time <- time[order(time)]
  # Create a vector of threshold values by combining trisk and Inf.
  thresholds <- c(trisk[-1], Inf)
  # Initialize an empty vector to store results.
  max_indices <- numeric(length(thresholds))
  # Loop over each element in the thresholds vector.
  for (i in 1:length(thresholds)) {
    # Find the index of the last element in time that is less than the current threshold.
    max_index <- Inf
    for (j in 1:length(time)) {
      if (time[j] < thresholds[i]) {
        max_index <- j
      } else {
        # Exit the loop once we find the index.
        break
      }
    }
    # Store the index in the results vector.
    max_indices[i] <- max_index
  }
  return(max_indices)
}  

#' This function sorts the pairs (`time`, `prob`) by survival probability in descending order,
#' with ties in probability ordered by survival time in ascending order, so that each probability keeps its own time.
#' Then it fixes survival times in one pass by replacing a time that is smaller than the preceding one with the preceding value.
#'
#' @param time A vector of survival times. Each time is paired with the probability at the same position in `prob`.
#' The vector does not need to be sorted.
#' @param prob A vector of survival probabilities.
#' @return A data.frame with the columns `time` and `prob`, one row per input element,
#' with the rows in probability order (descending, ties by time ascending).
#' Each probability keeps its own time, except that a time smaller than the preceding one is replaced by the preceding value.
#' @export
fix_non_increasing_survival_times <- function(time, prob) {
  if (length(time) != length(prob)) { 
    stop("Input vectors 'time' and 'prob' must have the same length!") 
  } 
  # Sort the pairs by probability descending, ties by time ascending.
  # Each time keeps its own probability. The radix method is required
  # for a vector valued 'decreasing'.
  ord <- order(prob, time, decreasing = c(TRUE, FALSE), method = "radix")
  new_time <- time[ord]
  new_prob <- prob[ord]
  non_increasing_indices <- which(diff(new_time) < 0) 
  new_time[non_increasing_indices + 1] <- new_time[non_increasing_indices] 
  df_modified <- data.frame(time = new_time, prob = new_prob)
  return(df_modified)
}    

#' This function sorts the pairs (`time`, `prob`) by survival time in ascending order,
#' with ties in time ordered by survival probability in descending order, so that each time keeps its own probability.
#' Then it fixes survival probabilities in one pass by replacing a probability that is larger than the preceding one with the preceding value.
#'
#' @param time A vector of survival times. Each time is paired with the probability at the same position in `prob`.
#' The vector does not need to be sorted.
#' @param prob A vector of survival probabilities.
#' @return A data.frame with the columns `time` and `prob`, one row per input element,
#' with the rows in time order (ascending, ties by probability descending).
#' Each time keeps its own probability, except that a probability larger than the preceding one is replaced by the preceding value.
#' @export
fix_non_decreasing_survival_probabilities <- function(time, prob) {
  if (length(time) != length(prob)) {
    stop("Input vectors 'time' and 'prob' must have the same length.")
  }
  # Sort the pairs by time ascending, ties by probability descending.
  # Each time keeps its own probability. The radix method is required
  # for a vector valued 'decreasing'.
  ord <- order(time, prob, decreasing = c(FALSE, TRUE), method = "radix")
  new_time <- time[ord]
  new_prob <- prob[ord]
  non_decreasing_indices <- which(diff(new_prob) > 0)
  new_prob[non_decreasing_indices + 1] <- new_prob[non_decreasing_indices]
  df_modified <- data.frame(time = new_time, prob = new_prob)
  return(df_modified)
}
