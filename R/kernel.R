#' Create a 2D Gaussian motion kernel as a SpatRaster
#'
#' @param radius Numeric, std of the distribution in meters
#' @param GSD Numeric, ground sampling distance in meters per pixel
#' @param size_std Numeric, how many std to extend kernel on each side
#'
#' @return A matrix defining the kernel
#' @export
compute_kernel <- function(radius, GSD, size_std = 1.5) {
  # Kernel size in pixels
  half_size <- ceiling(size_std * radius / GSD)
  size <- 2 * half_size + 1

  # Coordinates centered on 0
  coords <- seq(-half_size, half_size)
  grid <- expand.grid(x = coords, y = coords)

  # Gaussian
  sigma <- radius / GSD
  vals <- exp(-(grid$x^2 + grid$y^2)/(2 * sigma^2))
  vals <- vals / sum(vals)  # normalize

  # dimension de la grille
  nx <- length(unique(grid$x))
  ny <- length(unique(grid$y))
  # transformer vals en matrice (attention à byrow)
  matrix <- matrix(vals, nrow = ny, ncol = nx, byrow = FALSE)
  return(matrix)
}
