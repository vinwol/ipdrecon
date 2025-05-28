#' This function does some basic validations of the input parameters for reconstructing
#' IPD data by the method proposed by Guyot et al.
#'
#' @param time A vector of the x coordinates of the survival times.
#' @param prob A vector of the y coordinates corresponding to 'time', between 0 and 1. 
#' These represent survival probabilities at the times in 'time'.
#' @param trisk A vector of times at which patients are at risk.
#' @param nrisk A vector of the number of patients at risk.
#' @param lower A vector that contains the lower time limits for each interval. 
#' It defines the starting point of each interval where calculations for survival estimates and events will be made. 
#' @param upper A The `upper` parameter is also a vector that contains the upper time limits for each interval. 
#' It defines the end point of each interval where calculations will stop. 
#' @param tot_events The number total events (optional).
#' @export
validation_input_guyot <- function(time, prob, trisk, nrisk, lower, upper, tot_events = NA) {
  defined <- ls()
  passed <- names(as.list(match.call())[-1])
  if (any(!defined %in% passed)) {
    stop('Not all parameters provided!')
  }
  if (!is.na(tot_events)) {
    if (!is.numeric(tot_events)) {
      stop('The total number of events is not numeric!')
    }  
  }
  # Check for same length.
  check_risk_information(trisk, nrisk)
  # Check indices are valid numbers.
  if (!is.numeric(lower) || any(is.na(lower)) || any(is.infinite(lower))) {
    cat('\n',lower)
    stop("lower does not consist of valid numbers!")
  } else {
    cat('\nlower consists of valid numbers.') 
  }
  if (!is.numeric(upper) || any(is.na(upper)) || any(is.infinite(upper))) {
    cat('\n',upper)
    stop("upper does not consist of valid numbers!")
  } else {
    cat('\nupper consists of valid numbers.') 
  }
  # Check length: lower, upper, trisk, and nrisk must have same length.
  if (length(lower) != length(upper)) {
    stop("Length of lower and upper is not the same!")
  }
  if (length(lower) != length(trisk)) {
    stop("Length of lower and upper is not the same as the length of trisk!")
  }    
  # Check that probabilities are in the range [0-1].
  check_probabilities_for_range(prob)
  # Check if probabilities are not increasing.
  find_non_decreasing_probabilities(time, prob)
  # Check if survival times are not decreasing.
  find_non_increasing_survival_times(time, prob)
}

#' This function does some basic validations of the input parameters for reconstructing
#' IPD data by the method proposed by Rogula et al.
#'
#' @param n The number at risk at time zero.
#' @param time A vector of the x coordinates of the bottoms of the drops in survival.
#' @param prob A vector of the y coordinates corresponding to 'time', between 0 and 1. 
#' These represent survival proportions at the times in t.
#' @param cens_t A vector of times at which patients were censored (optional).
#' @export
validation_input_rogula <- function(n, time, prob, cens_t = NA) {
  defined <- ls()
  passed <- names(as.list(match.call())[-1])
  if (any(!defined %in% passed)) {
    stop('Not all parameters provided!')
  }
  if (length(cens_t) > 1) {
    if (!is.numeric(cens_t)) {
      stop('The censoring time vector is not numeric!')
    }  
  }
  # Check that probabilities are in the range [0-1].
  check_probabilities_for_range(prob)
  # Check if probabilities are not increasing.
  find_non_decreasing_probabilities(time, prob)
  # Check if survival times are not decreasing.
  find_non_increasing_survival_times(time, prob)
}

#' This function does some basic checks for numeric input.
#'
#' @param input A vector with different values which are supposed to be numeric.
#' @export
check_valid_numbers <- function(input) {
  if (!is.numeric(input) || any(is.na(input)) || any(is.infinite(input))) {
    cat('\n',input)
    stop("Input does not consist of valid numbers!")
  } else {
    cat('\nInput consists of valid numbers.') 
  }
}

#' This function checks if the survival probabilities are in the range of 0 to 1.
#'
#' @param prob A vector with values representing the survival probabilities.
#' @export
check_probabilities_for_range <- function(prob) {
  if (!is.numeric(prob) || any(is.na(prob)) || any(is.infinite(prob))) {
    stop("Survival probabilities do not consist of valid numbers!")
  }
  min <- min(prob)
  max <- max(prob)
  if (min < 0 || min > 1 || max < 0 || max > 1) {
    stop('Survival probabilities are not in the range 0 to 1!')
  } else {
    cat('\nSurvival probabilities are in the range 0 to 1.')
  }
}

#' This function does some basic checks on the time of risk and number of risk information.
#'
#' @param time_of_risk A vector with time points at which patient numbers at risk are provided.
#' @param number_at_risk A vector with number of patients at risk.
#' @export
check_risk_information <- function(time_of_risk, number_at_risk) {
  # Must be valid numbers and have same length.
  if (!is.numeric(time_of_risk) || any(is.na(time_of_risk)) || any(is.infinite(time_of_risk))) {
    cat('\n',time_of_risk)
    stop("Time of risk information does not consist of valid numbers!")
  }
  if (!is.numeric(number_at_risk) || any(is.na(number_at_risk)) || any(is.infinite(number_at_risk))) {
    cat('\n',number_at_risk)
    stop("Number at risk information does not consist of valid numbers!")
  }
  if (length(time_of_risk) != length(number_at_risk)) {
    stop("Vector of patients at risk and vector of risk time do not have same length!")
  }
  cat('\nTime of risk and number of risk information is ok.')
}

#' This function checks if there are any non decreasing survival probabilities.
#' It sorts by survival times in ascending order first and then checks if the survival probabilities are descending.
#'
#' @param time A vector with survival time.
#' @param prob A vector with survival probabilities.
#' @export
find_non_decreasing_probabilities <- function(time, prob) {
  cat('\nChecking if survival probabilities are descending:')
  cat('\nWe sort first by survival time and then check the probabilities.')
  df <- data.frame(time=time,prob=prob)
  # Sort by survival time in ascending order and then check if survival 
  # probabilities are descending.
  df <- df %>% dplyr::arrange(time)
  time_vec <- df$time
  prob_vec <- df$prob
  diff_vec <- diff(df$prob)
  flag <- TRUE
  for (index in 1:length(diff_vec)) {
    if (diff_vec[index] > 0) {
      flag <- FALSE
      if (index == 1 && index == length(prob_vec)) {
        cat('\nCase of non-decreasing survival probabilities:\n')
        cat(paste0('index=',index,', time=',time_vec[index],', prob=',prob_vec[index]))
        cat('\n')
      } else if (index == 1 && index < length(prob_vec)) {
        cat('\nCase of non-decreasing survival probabilities:\n')
        cat(paste0('index=',index,', time=',time_vec[index],', prob=',prob_vec[index]))
        cat('\n')
        cat(paste0('index=',index+1,', time=',time_vec[index+1],', prob=',prob_vec[index+1]))
        cat('\n')
      } else if (index > 1 && index < length(prob_vec)) {
        cat('\nCase of non-decreasing survival probabilities:\n')
        cat(paste0('index=',index-1,', time=',time_vec[index-1],', prob=',prob_vec[index-1]))
        cat('\n')
        cat(paste0('index=',index,', time=',time_vec[index],', prob=',prob_vec[index]))
        cat('\n')
        cat(paste0('index=',index+1,', time=',time_vec[index+1],', prob=',prob_vec[index+1]))
        cat('\n')
      } else if (index == length(prob_vec) && length(prob_vec) > 1) {
        cat('\nCase of non-decreasing survival probabilities:\n')
        cat(paste0('index=',index-1,', time=',time_vec[index-1],', prob=',prob_vec[index-1]))
        cat('\n')
        cat(paste0('index=',index,', time=',time_vec[index],', prob=',prob_vec[index]))
        cat('\n')
      } else {
        cat('\nCase of non-decreasing survival probabilities:\n')
        cat(paste0('index=',index,', time=',time_vec[index],', prob=',prob_vec[index]))
        cat('\n')
      } 
    } 
  } 
  if (flag) {
    cat('\nSurvival probabilities are descending.')
  } else {
    cat('\nNon descending survival probabilities were detected!')
  }
}  

#' This function checks if there are any non increasing survival time.
#' It sorts by survival probabilities in descending order first and then checks if the survival time are ascending.
#'
#' @param time A vector with survival time.
#' @param prob A vector with survival probabilities.
#' @export
find_non_increasing_survival_times <- function(time, prob) {
  cat('\nChecking if survival time are ascending:')
  cat('\nWe sort first by survival probabilities and then check the survival time.')
  df <- data.frame(time=time, prob=prob)
  # Sort by survival probabilities in descending order and then check if 
  # survival time are ascending.
  df <- df %>% dplyr::arrange(dplyr::desc(prob))
  time_vec <- df$time
  prob_vec <- df$prob
  diff_vec <- diff(df$time)
  flag <- TRUE
  for (index in 1:length(diff_vec)) {
    if (diff_vec[index] < 0) {
      flag <- FALSE
      if (index == 1 && index == length(time_vec)) {
        cat('\nCase of non-increasing survival times:\n')
        cat(paste0('index=',index,', time=',time_vec[index],', prob=',prob_vec[index]))
        cat('\n')
      } else if (index == 1 && index < length(time_vec)) {
        cat('\nCase of non-increasing survival times:\n')
        cat(paste0('index=',index,', time=',time_vec[index],', prob=',prob_vec[index]))
        cat('\n')
        cat(paste0('index=',index+1,', time=',time_vec[index+1],', prob=',prob_vec[index+1]))
        cat('\n')
      } else if (index > 1 && index < length(time_vec)) {
        cat('\nCase of non-increasing survival times:\n')
        cat(paste0('index=',index-1,', time=',time_vec[index-1],', prob=',prob_vec[index-1]))
        cat('\n')
        cat(paste0('index=',index,', time=',time_vec[index],', prob=',prob_vec[index]))
        cat('\n')
        cat(paste0('index=',index+1,', time=',time_vec[index+1],', prob=',prob_vec[index+1]))
        cat('\n')
      } else if (index == length(time_vec) && length(time_vec) > 1) {
        cat('\nCase of non-increasing survival times:\n')
        cat(paste0('index=',index-1,', time=',time_vec[index-1],', prob=',prob_vec[index-1]))
        cat('\n')
        cat(paste0('index=',index,', time=',time_vec[index],', prob=',prob_vec[index]))
        cat('\n')
      } else {
        cat('\nCase of non-increasing survival times:\n')
        cat(paste0('index=',index,', time=',time_vec[index],', prob=',prob_vec[index]))
        cat('\n')
      } 
    } 
  } 
  if (flag) {
    cat('\nSurvival times are increasing.')
  } else {
    cat('\nNon increasing survival times were detected!')
  }
}
