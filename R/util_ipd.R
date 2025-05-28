#' This function provides a data.frame with all the relevant time interval information.
#'
#' @param time A vector of survival probabilities.
#' @param trisk A vector of times at which patients are at risk.
#' @param nrisk A vector of the number of patients at risk.
#' @param lower A vector that contains the lower time limits for each interval. 
#' It defines the starting point of each interval where calculations for survival estimates and events will be made. 
#' @param upper A The `upper` parameter is also a vector that contains the upper time limits for each interval. 
#' It defines the end point of each interval where calculations will stop. 
#' @return A data.frame with all the relevant time interval information. 
#' @export
prepare_time_interval_data <- function(time, trisk, nrisk, lower, upper) {
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
#' @param time A vector of survival probabilities.
#' @param trisk A vector of times at which patients are at risk.
#' @return A vector that contains the lower time limits for each interval. 
#' It defines the starting point of each interval where calculations for survival 
#' estimates and events will be made. 
#' @export
get_lower_indices <- function(time, trisk) {
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
#' @param time A vector of survival probabilities.
#' @param trisk A vector of times at which patients are at risk.
#' @return A vector that contains the upper time limits for each interval. 
#' It defines the end point of each interval where calculations will stop. 
#' @export
get_upper_indices <- function(time, trisk) {
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

#' This function sorts first the input by survival probabilities in descending order, 
#' then fixes survival times by replacing non-increasing ones with predecessor values.
#'
#' @param time A vector of survival times.
#' @param prob A vector of survival probabilities.
#' @return A data.frame of modified survival times and probabilities. 
#' @export
fix_non_increasing_survival_times <- function(time, prob) {
  if (length(time) != length(prob)) { 
    stop("Input vectors 'time' and 'prob' must have the same length!") 
  } 
  df <- data.frame(time = time, prob = prob) 
  df <- df %>% dplyr::arrange(dplyr::desc(prob)) 
  new_time <- df$time 
  non_increasing_indices <- which(diff(new_time) < 0) 
  new_time[non_increasing_indices + 1] <- new_time[non_increasing_indices] 
  df_modified <- data.frame(time = new_time, prob = prob) 
  return(df_modified)
}    

#' This function sorts first the input by survival times in ascending order, 
#' then fixes survival probabilities by replacing non-decreasing ones with predecessor values.
#'
#' @param time A vector of survival times.
#' @param prob A vector of survival probabilities.
#' @return A data.frame of modified survival probabilities and times. 
#' @export
fix_non_decreasing_survival_probabilities <- function(time, prob) {
  if (length(time) != length(prob)) {
    stop("Input vectors 'time' and 'prob' must have the same length.")
  }
  df <- data.frame(time = time, prob = prob)
  df <- df %>% dplyr::arrange(time)
  new_prob <- df$prob
  non_decreasing_indices <- which(diff(new_prob) > 0)
  new_prob[non_decreasing_indices + 1] <- new_prob[non_decreasing_indices]
  df_modified <- data.frame(time = time, prob = new_prob)
  return(df_modified)
}
