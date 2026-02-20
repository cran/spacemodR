# ===============================
# Constructor: habitat
# ===============================

#' Create a Habitat object
#'
#' Create a spatial Habitat object based on an optional \code{sf} data.frame.
#' If no geometry is provided, creates an empty Habitat object.
#' The object has columns:
#' \itemize{
#'   \item \code{habitat}: logical, TRUE/FALSE
#'   \item \code{weight}: numeric
#'   \item \code{geometry}: sfc geometry
#' }
#'
#' @param geometry An object of class \code{sf} or \code{sfc}. Optional, default is empty.
#' @param habitat Logical vector indicating habitat presence. Default is logical(0).
#' @param weight Numeric vector of weights. Default is numeric(0).
#'
#' @return An object of class \code{habitat}, inheriting from \code{sf} and \code{data.frame}.
#' @export
#'
#' @examples
#' library(sf)
#'
#' # Empty habitat
#' h <- habitat()
#' h
#'
#' # Habitat with geometries
#' geom <- st_sfc(
#'   st_point(c(0, 0)),
#'   st_point(c(1, 1)),
#'   crs = 4326
#' )
#'
#' hab <- habitat(
#'   geometry = geom,
#'   habitat = c(TRUE, FALSE),
#'   weight = c(0.8, 0)
#' )
#' hab
habitat <- function(geometry = NULL,
                    habitat = NULL,
                    weight = NULL) {

  # ---- empty object if no geometry ----
  if (is.null(geometry)) {
    sf_obj <- sf::st_sf(
      habitat = logical(0),
      weight = numeric(0),
      geometry = sf::st_sfc()
    )
    class(sf_obj) <- c("habitat", class(sf_obj))
    return(sf_obj)
  }

  # ---- handle sf or sfc ----
  if (inherits(geometry, "sf")) {
    geom <- sf::st_geometry(geometry)
    n <- nrow(geometry)
  } else if (inherits(geometry, "sfc")) {
    geom <- geometry
    n <- length(geometry)
  } else {
    stop("`geometry` must be an object of class 'sf' or 'sfc'")
  }

  # ---- checks ----
  if (is.null(habitat)) habitat <- rep(TRUE, n)
  if (is.null(weight)) weight <- rep(1, n)

  if(length(habitat)==1) habitat <- rep(habitat, n)
  if(length(weight)==1) weight <- rep(weight, n)

  if (!length(habitat) == n) stop("Length of habitat must match geometry")
  if (!length(weight) == n) stop("Length of weight must match geometry")
  if (!is.logical(habitat)) stop("`habitat` must be logical (TRUE/FALSE)")
  if (!is.numeric(weight)) stop("`weight` must be numeric")

  # ---- build sf object ----
  sf_obj <- sf::st_sf(
    habitat = habitat,
    weight = weight,
    geometry = sf::st_sfc(geom)
  )
  class(sf_obj) <- c("habitat", class(sf_obj))
  sf_obj
}

#' @export
print.habitat <- function(x, ...) {
  cat("Habitat object\n")
  cat("Number of features:", nrow(x), "\n")
  cat("CRS:", sf::st_crs(x)$input, "\n")
  NextMethod()
}

# ===============================
# Generic functions
# ===============================

#' Add habitat zones to a Habitat object
#'
#' @param hab A Habitat object
#' @param sf_data An sf object containing geometries to add
#' @param weight A weight to apply on the habitat feature. Can then be used as
#'  resistance for dispersal functions for instance.
#' @param ... Additional arguments
#' @return A Habitat object with new geometries added as habitat
#' @export
add_habitat <- function(hab, sf_data, weight = 1, ...) {
  UseMethod("add_habitat")
}

#' Add non-habitat zones to a Habitat object
#'
#' @param hab A Habitat object
#' @param sf_data An sf object containing geometries to add
#' @param ... Additional arguments
#' @return A Habitat object with new geometries added as non-habitat
#' @export
add_nohabitat <- function(hab, sf_data, ...) {
  UseMethod("add_nohabitat")
}


# ===============================
# S3 methods for Habitat
# ===============================

#' @export
add_habitat.habitat <- function(hab, sf_data, weight = NULL, ...) {
  if (!inherits(sf_data, "sf")) stop("sf_data must be an sf object")
  new_block <- habitat(
    geometry = sf_data,
    habitat = TRUE,
    weight = weight
  )
  if(nrow(hab)==0){
    out <- new_block
  } else{
    # rbind.sf check crs.
    class(hab) <- setdiff(class(hab), "habitat")
    class(new_block) <- setdiff(class(new_block), "habitat")
    out <- rbind(hab, new_block)
    class(out) <- c("habitat", class(out))
  }
  return(out)
}

#' @export
add_nohabitat.habitat <- function(hab, sf_data, ...) {
  if (!inherits(sf_data, "sf")) stop("sf_data must be an sf object")
  new_block <- habitat(
    geometry = sf_data,
    habitat = FALSE,
    weight = 0
  )
  if(nrow(hab)==0){
    out <- new_block
  } else{
    # rbind.sf check crs.
    class(hab) <- setdiff(class(hab), "habitat")
    class(new_block) <- setdiff(class(new_block), "habitat")
    out <- rbind(hab, new_block)
    class(out) <- c("habitat", class(out))
  }
  return(out)
}


# ===============================
# Rasterize Habitat
# ===============================

#' Create a raster from a habitat object
#'
#' @param ground_raster SpatRaster (reference grid).
#' @param habitat habitat object inherited from \code{sf} data.frame class.
#'
#' @return SpatRaster (weight and NA in nogo)
#'
#' @export
habitat_raster <- function(ground_raster, habitat){
  if (!inherits(habitat, "habitat")) {
    stop("habitat must be a 'habitat' object")
  }
  # drop habitat to keep sf class
  class(habitat) <- setdiff(class(habitat), "habitat")

  out <- terra::rast(ground_raster)
  out[] <- 0

  if (nrow(habitat) > 0) {
    v <- terra::vect(habitat)
    out_hab  <- terra::rasterize(v, out, field = "weight", background = 0)
    out_go <- terra::rasterize(v, out, field = "habitat", background = TRUE)
    out_hab[out_go[]==FALSE] <- NA
    out <- out_hab
  }
  return(out)
}


