# Package ipdrecon

R package to reconstruct individual patient data from published KM survival curves. Mainly a wrapper around the algorithm by Guyot et al and the algorithm by Rogula et al.

# Package Installation

Install this package by: remotes::install_git("https://github.com/vinwol/ipdrecon")

# Package Website

URL: https://vinwol.github.io/ipdrecon/

# Reference of Package Functions

URL: https://pages.github.com/vinwol/ipdrecon/reference/index.html

# Digitalization Tool

WebPlotDigitizer: https://apps.automeris.io/wpd/
</br>
User Manual: https://automeris.io/WebPlotDigitizer/userManual.pdf
</br>
Tutorial: https://automeris.io/WebPlotDigitizer/tutorial.html

# Basic Usage

The following shows an example how to use the method by Guyot et al for reconstructing
IPD data from digitalized survival curves.

```r
library(ipdrecon)
digitized_data <- read.csv('./data/survival_lung_veteran_digitized.csv', header = FALSE)
time <- digitized_data[,1]
prob <- digitized_data[,2]
trisk <- seq(from=0, to=550, by=50)
nrisk <- c(137,84,55,34,25,18,13,11,6,5,4,4)
tot_events <- 126
lower <- ipdrecon::get_lower_indices(time, trisk) 
upper <- ipdrecon::get_upper_indices(time, trisk) 
res <- ipdrecon::get_ipd_guyot(time, prob, trisk, nrisk, lower, upper, tot_events)
ipd <- res[[1]]
```

The following shows an example how to use the method by Rogula et al for reconstructing
IPD data from digitalized survival curves.

```r
library(ipdrecon)
digitized_data <- read.csv('./data/survival_lung_veteran_digitized.csv', header = FALSE)
time <- digitized_data[,1]
prob <- digitized_data[,2]
n <- 137
cens.t <- NA 
ipd <- ipdrecon::get_ipd_rogula(n, time, prob, cens.t)
```

# Input Validation

The input parameters for the method by Guyot et al can be validated in the following way:

```r
library(ipdrecon)
digitized_data <- read.csv('./data/survival_lung_veteran_digitized.csv', header = FALSE)
time <- digitized_data[,1]
prob <- digitized_data[,2]
trisk <- seq(from=0, to=550, by=50)
nrisk <- c(137,84,55,34,25,18,13,11,6,5,4,4)
tot_events <- 126
lower <- ipdrecon::get_lower_indices(time, trisk) 
upper <- ipdrecon::get_upper_indices(time, trisk) 
ipdrecon::validation_input_guyot(time, prob, trisk, nrisk, lower, upper, tot_events)
```

The input parameters for the method by Rogula et al can be validated in the following way:

```r
library(ipdrecon)
digitized_data <- read.csv('./data/survival_lung_veteran_digitized.csv', header = FALSE)
time <- digitized_data[,1]
prob <- digitized_data[,2]
n <- 137
cens.t <- NA 
ipdrecon::validation_input_rogula(n, time, prob, cens.t)
```

# Issue Detection

With the function *find_non_decreasing_probabilities()* it is possible to detect if there are any non decreasing survival probabilities.
The function sorts by survival times in ascending order first and then checks if the survival probabilities are descending.

```{r message = FALSE, eval = FALSE}
library(ipdrecon)
digitized_data <- read.csv('./data/survival_lung_veteran_digitized.csv', header = FALSE)
time <- digitized_data[,1]
prob <- digitized_data[,2]
ipdrecon::find_non_decreasing_probabilities(time, prob)
```

The function *find_non_increasing_survival_times()* checks if there are any non increasing survival times.
It sorts by survival probabilities in descending order first and then checks if the survival times are ascending.

```{r message = FALSE, eval = FALSE}
library(ipdrecon)
digitized_data <- read.csv('./data/survival_lung_veteran_digitized.csv', header = FALSE)
time <- digitized_data[,1]
prob <- digitized_data[,2]
ipdrecon::find_non_increasing_survival_times(time, prob)
```

# Issue Correction

The function *fix_non_increasing_survival_times()* sorts first the input by survival probabilities in descending order, 
then fixes survival times by replacing non-increasing ones with predecessor values.

```r
library(ipdrecon)
digitized_data <- read.csv('./data/survival_lung_veteran_digitized.csv', header = FALSE)
time <- digitized_data[,1]
prob <- digitized_data[,2]
ipd_fixed <- ipdrecon::fix_non_increasing_survival_times(time, prob)
```

The function *fix_non_decreasing_survival_probabilities()* sorts first the input by survival times in ascending order, 
then fixes survival probabilities by replacing non-decreasing ones with predecessor values.

```r
library(ipdrecon)
digitized_data <- read.csv('./data/survival_lung_veteran_digitized.csv', header = FALSE)
time <- digitized_data[,1]
prob <- digitized_data[,2]
ipd_fixed <- ipdrecon::fix_non_decreasing_survival_probabilities(time, prob)
```




