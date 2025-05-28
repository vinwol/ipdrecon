library(covr)

covr::file_coverage("./R/util_ipd.R", "./tests/testthat/test_util_ipd.R")
covr::file_coverage("./R/util_point_translation.R", "./tests/testthat/test_util_point_translation.R")
covr::file_coverage("./R/util_validation.R", "./tests/testthat/test_util_validation.R")
covr::file_coverage("./R/ipd_guyot.R", "./tests/testthat/test_ipd_guyot.R")
covr::file_coverage("./R/ipd_rogula.R", "./tests/testthat/test_ipd_rogula.R")

