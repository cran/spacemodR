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
#'   \item \code{priority}: numeric, stacking order used by \code{\link{habitat_raster}}
#'   \item \code{touches}: logical, pixels filled by the feature in \code{\link{habitat_raster}}
#'   \item \code{geometry}: sfc geometry
#' }
#'
#' @param geometry An object of class \code{sf} or \code{sfc}. Optional, default is empty.
#' @param habitat Logical vector indicating habitat presence. Default is logical(0).
#' @param weight Numeric vector of weights. Default is numeric(0).
#' @param priority Numeric vector giving the stacking order of the features when
#'   they are rasterized with \code{\link{habitat_raster}}: where features overlap,
#'   the one with the highest priority gives its value to the pixel. Default is
#'   \code{1} for every feature, in which case the order of the features decides
#'   (last on top).
#' @param touches Logical vector. If \code{TRUE} (default), a feature fills every
#'   pixel it touches when rasterized with \code{\link{habitat_raster}}. If
#'   \code{FALSE}, a polygon only fills the pixels whose centre it covers and a line
#'   only the pixels of a thin line. See \code{\link{add_habitat}} for guidance.
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
                    weight = NULL,
                    priority = NULL,
                    touches = NULL) {

  # ---- empty object if no geometry ----
  if (is.null(geometry)) {
    sf_obj <- sf::st_sf(
      habitat = logical(0),
      weight = numeric(0),
      priority = numeric(0),
      touches = logical(0),
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
  if (is.null(priority)) priority <- rep(1, n)
  if (is.null(touches)) touches <- rep(TRUE, n)

  if(length(habitat)==1) habitat <- rep(habitat, n)
  if(length(weight)==1) weight <- rep(weight, n)
  if(length(priority)==1) priority <- rep(priority, n)
  if(length(touches)==1) touches <- rep(touches, n)

  if (!length(habitat) == n) stop("Length of habitat must match geometry")
  if (!length(weight) == n) stop("Length of weight must match geometry")
  if (!length(priority) == n) stop("Length of priority must match geometry")
  if (!length(touches) == n) stop("Length of touches must match geometry")
  if (!is.logical(habitat)) stop("`habitat` must be logical (TRUE/FALSE)")
  if (!is.numeric(weight)) stop("`weight` must be numeric")
  if (!is.numeric(priority) || anyNA(priority)) {
    stop("`priority` must be numeric, without NA")
  }
  if (!is.logical(touches) || anyNA(touches)) {
    stop("`touches` must be logical (TRUE/FALSE)")
  }

  # ---- build sf object ----
  sf_obj <- sf::st_sf(
    habitat = habitat,
    weight = weight,
    priority = priority,
    touches = touches,
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

#' Plot a Habitat object
#'
#' @description
#' Visually inspect a \code{habitat} object. Favorable habitat zones are colored
#' in green, and absolute barriers (non-habitat) are colored in red.
#'
#' @param x A \code{habitat} object.
#' @param ... Additional arguments passed to \code{plot} (e.g., \code{main}, \code{border}).
#'
#' @return \code{x}, invisibly. Called for its side effect: a plot of the
#' habitat zones.
#'
#' @export
#'
#' @examples
#' library(sf)
#' square <- function(a, b) st_polygon(list(cbind(c(a, b, b, a, a), c(a, a, b, b, a))))
#' good_polygons <- st_sf(geometry = st_sfc(square(1, 9)))
#' roads <- st_sf(geometry = st_sfc(square(4, 6)))
#' hab <- habitat() |>
#'   add_habitat(good_polygons, weight = 1) |>
#'   add_nohabitat(roads)
#' plot(hab)
plot.habitat <- function(x, ...) {
  # Handle empty objects gracefully
  if (nrow(x) == 0) {
    message("Empty habitat object. Nothing to plot.")
    return(invisible(x))
  }
  # Ensure sf's plotting method is available
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for plotting.")
  }
  # Define colors: Green for TRUE (habitat), Red for FALSE (no-habitat)
  fill_colors <- ifelse(x$habitat, "#81C784", "#E57373") # Muted green and red
  # Set up the title if not provided by the user
  args <- list(...)
  if (!"main" %in% names(args)) {
    args$main <- "Habitat Landscape\n(Green: Habitat, Red: Barrier)"
  }
  # Plot geometries
  do.call(plot, c(list(sf::st_geometry(x), col = fill_colors), args))
  # Return the object invisibly to allow piping if needed
  invisible(x)
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
#' @param priority Stacking order of the added layer when the habitat is
#'  rasterized with \code{\link{habitat_raster}}: where layers overlap, the one
#'  with the highest priority gives its value to the pixel. A single number for
#'  the whole layer, or one value per feature. By default (\code{NULL}) the layer
#'  is placed on top of everything already in \code{hab} (highest priority so
#'  far + 1), so that layers stack in the order they are added.
#' @param touches Logical, a single value for the whole layer or one value per
#'  feature. If \code{TRUE} (default), the features fill every pixel they touch
#'  when rasterized with \code{\link{habitat_raster}}; if \code{FALSE}, a polygon
#'  only fills the pixels whose centre it covers, and a line only the pixels of
#'  a thin line. See Details.
#' @param ... Additional arguments
#' @return A Habitat object with new geometries added as habitat
#'
#' @details
#' \code{touches} controls which pixels a feature occupies, not how hard it is
#' to cross: the permeability of a structure is carried by its \code{weight}
#' (or by \code{\link{add_nohabitat}} for an absolute barrier).
#'
#' \itemize{
#'   \item \code{touches = TRUE} keeps thin structures (hedgerows, rivers, roads,
#'   whether drawn as lines or as narrow polygons) continuous: every pixel they
#'   cross is filled, so they cannot vanish between pixel centres nor be crossed
#'   diagonally between two pixels. The price is a footprint larger than the
#'   feature: it grows by up to one pixel in width.
#'   \item \code{touches = FALSE} keeps areas unbiased, which is preferable for
#'   a layer made of contiguous polygons covering the whole landscape (a land
#'   cover map): with \code{touches = TRUE}, each polygon would spread on the
#'   pixels of its neighbours and the border pixels would go to the feature on
#'   top of the stack.
#' }
#'
#' A common set-up is therefore a land cover layer added with
#' \code{touches = FALSE} and a low \code{priority}, with the thin structures
#' that must stay continuous added on top with \code{touches = TRUE}.
#'
#' @export
#'
#' @examples
#' library(sf)
#' square <- function(a, b) {
#'   st_sf(geometry = st_sfc(st_polygon(list(
#'     cbind(c(a, b, b, a, a), c(a, a, b, b, a))))))
#' }
#' hedge <- st_sf(geometry = st_sfc(st_linestring(cbind(c(0, 10), c(0, 10)))))
#'
#' # the hedge is declared first but kept on top of the field with `priority`
#' hab <- habitat() |>
#'   add_habitat(hedge, weight = 1, priority = 10) |>
#'   add_habitat(square(2, 8), weight = 0.2, priority = 1)
#' hab
add_habitat <- function(hab, sf_data, weight = 1, priority = NULL,
                        touches = TRUE, ...) {
  UseMethod("add_habitat")
}

#' Add non-habitat zones to a Habitat object
#'
#' @param hab A Habitat object
#' @param sf_data An sf object containing geometries to add
#' @param priority Stacking order of the added layer when the habitat is
#'  rasterized with \code{\link{habitat_raster}}: the non-habitat zone only
#'  excludes the pixels where no layer of higher priority lies on top of it.
#'  Same default as in \code{\link{add_habitat}}: on top of everything already
#'  in \code{hab}.
#' @param touches Logical, a single value for the whole layer or one value per
#'  feature. If \code{TRUE} (default), every pixel touched by the features is
#'  excluded, so that the barrier is watertight: it cannot vanish between pixel
#'  centres nor be crossed diagonally between two pixels. If \code{FALSE}, a
#'  polygon only excludes the pixels whose centre it covers and a line only the
#'  pixels of a thin line. See \code{\link{add_habitat}} for details.
#' @param ... Additional arguments
#' @return A Habitat object with new geometries added as non-habitat
#' @export
add_nohabitat <- function(hab, sf_data, priority = NULL, touches = TRUE, ...) {
  UseMethod("add_nohabitat")
}


# ===============================
# S3 methods for Habitat
# ===============================

# Priority of a habitat object; objects built before the `priority` column
# existed stack in the order of their features
.habitat_priority <- function(hab) {
  if (is.null(hab$priority)) seq_len(nrow(hab)) else hab$priority
}

# Same for the `touches` column: every touched pixel is filled
.habitat_touches <- function(hab) {
  if (is.null(hab$touches)) rep(TRUE, nrow(hab)) else hab$touches
}

# Bind a new block of features on a habitat object
.bind_habitat <- function(hab, sf_data, habitat, weight, priority, touches) {
  if (!inherits(sf_data, "sf")) stop("sf_data must be an sf object")
  if (is.null(priority)) {
    # default: on top of what is already there
    priority <- if (nrow(hab) == 0) 1 else max(.habitat_priority(hab)) + 1
  }
  new_block <- habitat(
    geometry = sf_data,
    habitat = habitat,
    weight = weight,
    priority = priority,
    touches = touches
  )
  if(nrow(hab)==0){
    out <- new_block
  } else{
    hab$priority <- .habitat_priority(hab)
    hab$touches <- .habitat_touches(hab)
    hab <- hab[, names(new_block)]
    # rbind.sf check crs.
    class(hab) <- setdiff(class(hab), "habitat")
    class(new_block) <- setdiff(class(new_block), "habitat")
    out <- rbind(hab, new_block)
    class(out) <- c("habitat", class(out))
  }
  return(out)
}

#' @export
add_habitat.habitat <- function(hab, sf_data, weight = NULL, priority = NULL,
                                touches = TRUE, ...) {
  .bind_habitat(hab, sf_data, habitat = TRUE, weight = weight,
                priority = priority, touches = touches)
}

#' @export
add_nohabitat.habitat <- function(hab, sf_data, priority = NULL, touches = TRUE, ...) {
  .bind_habitat(hab, sf_data, habitat = FALSE, weight = 0,
                priority = priority, touches = touches)
}


# ===============================
# Rasterize Habitat
# ===============================

#' Rasterize a Habitat Object
#'
#' @description
#' Converts a spatial \code{habitat} object into a \code{terra::SpatRaster}. The resulting
#' raster stores the \code{weight} values of the habitat geometries. Areas explicitly
#' defined as non-habitat (\code{habitat = FALSE}) are assigned \code{NA}.
#'
#' @param ground_raster A \code{terra::SpatRaster} object used as a reference template.
#'   It provides the extent, resolution, and coordinate reference system (CRS).
#' @param habitat A \code{habitat} object (inheriting from \code{sf}).
#'
#' @details
#' The function maps the continuous or categorical weights of the habitat geometries onto a raster grid.
#'
#' \strong{Background handling:} By default, pixels not covered by any geometry are assigned
#' a weight of \code{0} and are considered passable (habitat = \code{TRUE}).
#'
#' \strong{Exclusion zones:} Geometries added via \code{add_nohabitat()} act as absolute
#' barriers. Any pixel intersecting these geometries will be assigned \code{NA}, overriding
#' any underlying weight.
#'
#' \strong{Stacking order:} Where several features overlap, the pixel takes the value of
#' the feature with the highest \code{priority} (see \code{\link{add_habitat}}); features
#' are stacked from the lowest to the highest priority and the top one is read. With equal
#' priorities, the order of the features in the \code{habitat} object decides (last on
#' top). As \code{add_habitat()} and \code{add_nohabitat()} give by default an increasing
#' priority to each new layer, layers stack in the order they were added unless
#' \code{priority} is set explicitly. The same rule applies to exclusion zones: a
#' non-habitat feature excludes a pixel only if no feature of higher priority covers it.
#'
#' \strong{Pixels filled by a feature:} The \code{touches} value of each feature (see
#' \code{\link{add_habitat}}) sets which pixels it occupies. With \code{touches = TRUE}
#' (the default of \code{add_habitat()} and \code{add_nohabitat()}) a line or a polygon
#' fills every pixel it touches. A thin structure such as a hedgerow, a river or a road
#' is then a chain of pixels sharing an edge: a corridor stays connected for 4-neighbour
#' movements and a barrier cannot be crossed diagonally. With \code{touches = FALSE}, a
#' polygon fills only the pixels whose centre it covers and a line only the pixels of a
#' thin line, which keeps areas unbiased but can break thin structures. Combine a high
#' \code{priority} with \code{touches = TRUE} to keep a thin structure continuous above
#' the layers it runs through. Points, lines and polygons can be mixed in the same
#' \code{habitat} object.
#'
#' Note that a raster holds one value per pixel: a structure narrower than a pixel is
#' either widened to a full pixel or dropped, and two structures sharing a pixel cannot
#' both be kept. When this matters, the only real solution is a finer
#' \code{ground_raster}.
#'
#' @return A \code{terra::SpatRaster} containing habitat weights, with \code{NA} values
#'   in explicit non-habitat zones.
#'
#' @importFrom terra rast vect rasterize mask
#' @export
#'
#' @examples
#' library(terra)
#' library(sf)
#'
#' # Create a dummy reference raster
#' ref_grid <- rast(nrows=10, ncols=10, xmin=0, xmax=10, ymin=0, ymax=10)
#'
#' # Create habitat zones
#' good_hab <- st_sf(geometry = st_sfc(st_polygon(list(cbind(c(1, 9, 9, 1, 1), c(1, 1, 9, 9, 1))))))
#' bad_hab  <- st_sf(geometry = st_sfc(st_polygon(list(cbind(c(4, 6, 6, 4, 4), c(4, 4, 6, 6, 4))))))
#'
#' # Build habitat object
#' my_habitat <- habitat() |>
#'   add_habitat(good_hab, weight = 0.8) |>
#'   add_nohabitat(bad_hab)
#'
#' # Rasterize
#' hab_rast <- habitat_raster(ref_grid, my_habitat)
#' plot(hab_rast)
#'
#' # A hedgerow kept continuous across the exclusion zone, whatever the order
#' # in which the layers are added
#' hedge <- st_sf(geometry = st_sfc(st_linestring(cbind(c(0, 10), c(0.2, 9.7)))))
#' my_habitat <- habitat() |>
#'   add_habitat(hedge, weight = 1, priority = 10) |>
#'   add_habitat(good_hab, weight = 0.8, priority = 1, touches = FALSE) |>
#'   add_nohabitat(bad_hab, priority = 2)
#' plot(habitat_raster(ref_grid, my_habitat))
habitat_raster <- function(ground_raster, habitat){
  if (!inherits(habitat, "habitat")) {
    stop("`habitat` must be a 'habitat' object")
  }
  # drop habitat to keep sf class
  class(habitat) <- setdiff(class(habitat), "habitat")

  template <- terra::rast(ground_raster)

  if (nrow(habitat) == 0) {
    out <- terra::init(template, 0)
    names(out) <- "weight"
    return(out)
  }

  # stack from the lowest to the highest priority (ties keep the feature order)
  habitat <- habitat[order(.habitat_priority(habitat)), ]
  touches <- .habitat_touches(habitat)

  # terra cannot mix geometry types in a SpatVector, and `touches` applies to a
  # whole call: rasterize each run of consecutive features sharing the same
  # type and `touches`, and pile the runs up
  geom_type <- sub("^MULTI", "", as.character(sf::st_geometry_type(habitat)))
  runs <- rle(paste(geom_type, touches))
  run_id <- rep(seq_along(runs$lengths), runs$lengths)

  out_hab <- NULL
  out_go <- NULL
  for (i in seq_along(runs$lengths)) {
    in_run <- run_id == i
    v <- terra::vect(habitat[in_run, ])
    r_hab <- terra::rasterize(v, template, field = "weight",
                              touches = touches[in_run][1])
    r_go <- terra::rasterize(v, template, field = "habitat",
                             touches = touches[in_run][1])
    out_hab <- if (is.null(out_hab)) r_hab else terra::cover(r_hab, out_hab)
    out_go <- if (is.null(out_go)) r_go else terra::cover(r_go, out_go)
  }
  # background
  out_hab <- terra::ifel(is.na(out_hab), 0, out_hab)
  out_go <- terra::ifel(is.na(out_go), 1, out_go)
  out <- terra::mask(out_hab, out_go, maskvalues = 0, updatevalue = NA)
  names(out) <- "weight"

  return(out)
}
