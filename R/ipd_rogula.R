#' Function to reconstruct individual patient data (IPD) from Kaplan-Meier (KM) survival curves
#' based on B. Rogula et al. A Method for Reconstructing Individual Patient Data From Kaplan-Meier 
#' Survival Curves That Incorporate Marked Censoring Times, 2022.
#' The function returns the reconstructed individual patient data (IPD) based on 
#' digitized data from Kaplan-Meier (KM) survival curves. 
#' As an input a vector of digitized censoring times and will incorporate them into 
#' the IPD exactly as specified.
#' If the KM curve is horizontal at its end, either (1) the coordinates of the end 
#' of the curve can be included in time and prob, 
#' OR (2) the x-coordinate of the end of the curve can be included in cens_t.
#'
#' @param n The number of patients at risk at time zero.
#' @param time A vector of the x coordinates of the bottoms of the drops in survival.
#' @param prob A vector of the y coordinates corresponding to 't', between 0 and 1. 
#' These represent survival proportions at the times in t.
#' @param cens_t A vector of times at which patients were censored (optional).
#' @return A list with two data.frames, one with the reconstructed IPD with two columns: 
#' (1) time: survival time, (2) event: indicator of event, where 1=event and 0=censored.
#' The other one contains survival times and probabilities, potentially modified to
#' fix non-decreasing survival probabilities.
#' @export
get_ipd_rogula <- function(n, 
                           time, 
                           prob, 
                           cens_t = NA) {
  
  delta <- 0.1
  if (max(prob) > (1 + delta)) {
    stop('Survival probabilities should not be bigger than 1!')
  } else {
    # We apply a basic fix.
    prob[prob > 1] <- 1
  }
  
  # Correct negative times.
  time[time < 0] <- 0
  
  # Sort provided data.
  time <- sort(time) # survival time.
  prob <- sort(prob, decreasing=T) # survival probability.
  cens_t <- sort(cens_t) # censoring time.
  
  # We apply a basic fix if survival probabilities are increasing.
  df_input <- fix_non_decreasing_survival_probabilities(time, prob)
  time <- df_input$time
  prob <- df_input$prob
  
  # Validation.
  validation_input_rogula(n, time, prob, cens_t)
  
  # Add (time=0, prob=1).
  time <- c(0,time)
  prob <- c(1,prob)
  
  # Create shell table for reconstructed IPD.
  maxFU <- max(time, cens_t, na.rm=T)
  IPD <- data.frame(time = rep(maxFU, n), event = 0)
  
  # Keep track of the last patient for which IPD was reconstructed for.
  p <- 0 # Number of Patient.
  
  # Looping over the extracted time points:
  for (i in 2:length(time)) {
    
    if (is.na(cens_t[1]) == F) {
      # Determine the number of patients censored between t[i-1] and t[i].
      n_cens <- min(sum(cens_t >= time[i-1] & cens_t < time[i]), n-p)
      # Add censoring events to the IPD.
      if (n_cens > 0) {
        IPD$time[(p+1):(p+n_cens)] <- cens_t[cens_t >= time[i-1] & cens_t < time[i]][1:n_cens]
        IPD$event[(p+1):(p+n_cens)] <- 0
      }
      p <- p + n_cens
    }
    
    # Find which number of events at t[i] results in a survival proportion at t[i] closest to S[i].
    # Keep testing an increasing number of events until the survival proportion at t[i] falls below S[i].
    if (p < n) {
      n_died <- -1
      diff <- 1
      while (diff > 0 & p+n_died+1 <= n) {
        diff_last <- diff
        n_died <- n_died + 1
        # Add events to the reconstructed IPD
        if (n_died > 0) {
          IPD$time[p+n_died] <- time[i]
          IPD$event[p+n_died] <- 1
        }
        # Find resulting survival estimate at t[i].
        est_prob <- summary(survival::survfit(survival::Surv(time, event) ~ 1, data = IPD), time=time[i])$surv
        diff <- est_prob - prob[i]
      }
      # Check whether n_died or n_died-1 resulted in a closer fit at t[i].
      # If n_died-1 resulted in a closer fit, remove one event from the reconstructed IPD.
      if (n_died > 0 & abs(diff_last) < abs(diff))  {
        IPD$time[p+n_died] <- maxFU
        IPD$event[p+n_died] <- 0
        n_died <- n_died - 1
      }
      # Update p.
      p <- p + n_died
    }
  } # end for loop: for (i in 2:length(t)) {...}
  
  # Check for censoring info to add at the end.
  if (is.na(cens_t[1]) == F) {
    # Add censored observations after last time in t.
    n_cens <- min(sum(cens_t >= max(time)), n-p)
    if (n_cens > 0) {
      IPD$time[(p+1):(p+n_cens)] <- cens_t[cens_t >= max(time)][1:n_cens]
      IPD$event[(p+1):(p+n_cens)] = 0
    }
  }
  return(list(IPD, df_input))
}
