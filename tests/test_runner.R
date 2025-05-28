library(testthat)
library(dplyr)
library(survival)

source('./R/ipd_guyot.R')
source('./R/ipd_rogula.R')
source('./R/util_ipd.R')
source('./R/util_point_translation.R')
source('./R/util_validation.R')

testthat::test_dir('./tests/testthat/')

