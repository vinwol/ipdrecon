# Test for R script util_point_translation.R

testthat::test_that("Pixel translation", {
  x_ref <- matrix(c(0, 30, 447.17, 2683.03), ncol = 2, byrow = TRUE)
  y_ref <- matrix(c(0, 1, 1811.74, 80.68), ncol = 2, byrow = TRUE)
  #plot_area <- c(447.17, 2683.03, 1811.74, 80.68) # c(left, right, top, bottom) 
  #data_range <- c(0, 30, 0, 1) # c(min_x, max_x, min_y, max_y) 
  # Examples:
  x <- 436
  y <- 90
  testthat::expect_equal(round(pixel_to_data_point(x, y, x_ref, y_ref),5), c(0.14988, 0.99462))
})