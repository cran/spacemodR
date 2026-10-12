# ===============================
# Tools for soil interpolation: grid, covariates and kernel smoothing
# ===============================

#' Build a regular raster grid around sampling points
#'
#' @description
#' Creates an empty `SpatRaster` covering the bounding box of `points`,
#' enlarged by `margin` on each side, with square cells of size `res`. The
#' extent is aligned on multiples of `res`.
#'
#' @param points An `sf` / `sfc` object (or any object accepted by
#'   [sf::st_bbox()]) in a projected CRS (metres).
#' @param res Numeric. Cell size in metres (ground sampling distance).
#' @param margin Numeric. Margin in metres added around the points.
#'
#' @return An empty `SpatRaster`.
#' @examples
#' data(soil_metaleurop)
#' soil_grid(soil_metaleurop, res = 25, margin = 1000)
#' @export
soil_grid <- function(points, res = 25, margin = 1000) {
  bb <- sf::st_bbox(points)
  e <- terra::ext(floor((bb[["xmin"]] - margin) / res) * res,
                  ceiling((bb[["xmax"]] + margin) / res) * res,
                  floor((bb[["ymin"]] - margin) / res) * res,
                  ceiling((bb[["ymax"]] + margin) / res) * res)
  terra::rast(e, resolution = res, crs = sf::st_crs(points)$wkt)
}

#' Distance and wind covariates around a contamination source
#'
#' @description
#' Builds covariate rasters describing the position of every cell relative to
#' a point source of contamination (e.g. a smelter chimney). They can be used
#' as external drift in [krige_soil()]:
#' \describe{
#'   \item{`log10_dist`}{the log10 of the distance (m) to the nearest source.
#'     Atmospheric deposition typically decreases as a power law of the
#'     distance, i.e. linearly in log-log scale.}
#'   \item{`wind`}{(only if `wind_rose` is given) the frequency of the wind
#'     sector in which the cell lies, seen from the source. Cells lying
#'     downwind of the dominant winds receive more deposition.}
#' }
#'
#' @param template A `SpatRaster` defining the grid.
#' @param source An `sf` / `sfc` POINT object: location(s) of the source(s).
#'   Must be a single point when `wind_rose` is used.
#' @param wind_rose Optional `data.frame` with columns `direction` (centre of
#'   the sector, in degrees clockwise from the North, as seen from the source)
#'   and `freq` (wind frequency, any unit). Each cell gets the `freq` of the
#'   closest sector direction.
#' @param min_dist Numeric. Distances below this value are set to `min_dist`
#'   (avoids `log10(0)` at the source). Default: the cell size.
#'
#' @return A `SpatRaster` with layers `log10_dist` and, optionally, `wind`.
#'
#' @examples
#' data(soil_metaleurop)
#' grid <- soil_grid(soil_metaleurop, res = 100)
#' smelter <- sf::st_sfc(sf::st_point(c(701156, 7036799)), crs = 2154)
#' cov <- source_covariates(grid, smelter)
#' terra::plot(cov)
#' @export
source_covariates <- function(template, source, wind_rose = NULL, min_dist = NULL) {
  if (!inherits(template, "SpatRaster")) stop("`template` must be a SpatRaster.")
  source <- sf::st_geometry(source)
  if (sf::st_crs(source) != sf::st_crs(terra::crs(template))) {
    source <- sf::st_transform(source, terra::crs(template))
  }
  if (is.null(min_dist)) min_dist <- min(terra::res(template))

  base <- terra::rast(template, nlyrs = 1)
  xy <- terra::xyFromCell(base, seq_len(terra::ncell(base)))
  src <- sf::st_coordinates(source)[, 1:2, drop = FALSE]

  dist <- apply(.cross_dist(xy, src), 1, min)
  out <- base
  terra::values(out) <- log10(pmax(dist, min_dist))
  names(out) <- "log10_dist"

  if (!is.null(wind_rose)) {
    if (nrow(src) != 1) stop("`wind_rose` can only be used with a single source point.")
    if (!all(c("direction", "freq") %in% names(wind_rose))) {
      stop("`wind_rose` must have columns `direction` and `freq`.")
    }
    azimuth <- (atan2(xy[, 1] - src[1, 1], xy[, 2] - src[1, 2]) * 180 / pi) %% 360
    # circular angular difference to each sector centre
    diff <- abs(outer(azimuth, wind_rose$direction %% 360, "-"))
    diff <- pmin(diff, 360 - diff)
    wind <- base
    terra::values(wind) <- wind_rose$freq[max.col(-diff, ties.method = "first")]
    names(wind) <- "wind"
    out <- c(out, wind)
  }
  out
}

#' Gaussian kernel smoothing of soil concentrations
#'
#' @description
#' A simple, assumption-light alternative to kriging: the value of each cell is
#' the weighted mean of the sampled values, the weights being a Gaussian
#' function of the distance (Nadaraya-Watson estimator):
#' \deqn{\hat z(s) = \frac{\sum_i K(s - s_i) z_i}{\sum_i K(s - s_i)}}
#'
#' It re-uses [compute_kernel()] (the dispersal kernel of the package): the
#' points are first rasterised (sum of values and number of points per cell),
#' both rasters are convolved with the kernel using [terra::focal()], and the
#' ratio gives the smoothed map.
#'
#' Unlike kriging, the result does not honour the data and the smoothing
#' strength (`radius`) must be chosen by the user, e.g. by cross-validation
#' with [cv_soil()].
#'
#' @inheritParams krige_soil
#' @param radius Numeric. Standard deviation (m) of the Gaussian kernel.
#' @param size_std Numeric. Half-size of the kernel window, in number of
#'   `radius` (passed to [compute_kernel()]). Cells with no point within the
#'   window are `NA`.
#'
#' @return A `SpatRaster` with the layer `<value>_log10` (or `<value>` if
#'   `log10 = FALSE`) and, if `log10 = TRUE`, the back-transformed layer
#'   `<value>`.
#'
#' @seealso [compute_kernel()], [krige_soil()], [cv_soil()]
#' @examples
#' data(soil_metaleurop)
#' r <- kernel_smooth_soil(soil_metaleurop, "cd", radius = 200, res = 50)
#' terra::plot(r)
#' @export
kernel_smooth_soil <- function(points, value, radius, template = NULL,
                               log10 = TRUE, res = 25, margin = 1000,
                               size_std = 3, mask = NULL) {
  d <- .prepare_soil_points(points, value, log10, verbose = FALSE)
  if (is.null(template)) template <- soil_grid(points, res, margin)
  out <- .kernel_smooth_engine(d$xy, d$z, template, radius, size_std)
  names(out) <- if (log10) paste0(value, "_log10") else value
  if (log10) {
    conc <- 10^out
    names(conc) <- value
    out <- c(out, conc)
  }
  if (!is.null(mask)) out <- terra::mask(out, terra::vect(mask))
  out
}

#' @noRd
.kernel_smooth_engine <- function(xy, z, template, radius, size_std = 3) {
  gsd <- terra::res(template)
  if (abs(gsd[1] - gsd[2]) > 1e-6 * gsd[1]) stop("Kernel smoothing needs square cells.")
  kernel <- compute_kernel(radius = radius, GSD = gsd[1], size_std = size_std)

  base <- terra::rast(template, nlyrs = 1)
  num <- terra::rasterize(xy, base, values = z, fun = "sum", background = 0)
  den <- terra::rasterize(xy, base, values = 1, fun = "sum", background = 0)
  num_s <- terra::focal(num, w = kernel, fun = "sum", na.rm = TRUE)
  den_s <- terra::focal(den, w = kernel, fun = "sum", na.rm = TRUE)
  den_s[den_s <= 0] <- NA
  num_s / den_s
}

#' Nadaraya-Watson Gaussian smoothing evaluated exactly at target points
#'
#' Same estimator as `.kernel_smooth_engine()` (square window of half-size
#' `size_std * radius`, as in [compute_kernel()]) without rasterisation.
#' @noRd
.kernel_smooth_points <- function(xy, z, target_xy, radius, size_std = 3) {
  half <- size_std * radius
  dx <- abs(outer(target_xy[, 1], xy[, 1], "-"))
  dy <- abs(outer(target_xy[, 2], xy[, 2], "-"))
  w <- exp(-(dx^2 + dy^2) / (2 * radius^2)) * (dx <= half & dy <= half)
  den <- rowSums(w)
  out <- as.vector(w %*% z) / den
  out[den <= 0] <- NA
  out
}
