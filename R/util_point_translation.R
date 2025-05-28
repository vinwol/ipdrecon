#' This function uses linear interpolation to translate from on screen pixel space into data space given reference values.
#'
#' @param pixel_x A pixel value in x dimension.
#' @param pixel_y A pixel value in y dimension.
#' @param x_ref A 2x2 matrix which contains the reference values in x dimension in data and pixel space.
#' @param y_ref A 2x2 matrix which contains the reference values in y dimension in data and pixel space.
#' @return A vector with the data values inx and y dimension
#' @export
pixel_to_data_point <- function(pixel_x, pixel_y, x_ref, y_ref) { 
  ref_x1 <- x_ref[2,1] # plot_area[1]
  ref_x2 <- x_ref[2,2] # plot_area[2]
  ref_y1 <- y_ref[2,2] # plot_area[3]
  ref_y2 <- y_ref[2,1] # plot_area[4]
  data_x1 <- x_ref[1,1] # data_range[1]
  data_x2 <- x_ref[1,2] # data_range[2]
  data_y1 <- y_ref[1,1] # data_range[3]
  data_y2 <- y_ref[1,2] # data_range[4]
  # Calculate scaling factors for x and y axes. 
  x_scale <- (data_x2 - data_x1) / (ref_x2 - ref_x1) 
  y_scale <- (data_y1 - data_y2) / (ref_y1 - ref_y2) 
  # Perform linear interpolation. 
  data_x <- data_x1 - (pixel_x - ref_x1) * x_scale 
  data_y <- data_y1 - (pixel_y - ref_y2) * y_scale 
  return(c(data_x, data_y)) 
} 
