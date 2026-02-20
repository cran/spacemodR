# This script provides tools to download and manipulate geospatial data
# for French departments, particularly to identify departments intersecting
# a region of interest (ROI).

#' @noRd
.dpt_url <- "https://www.data.gouv.fr/fr/datasets/r/90b9341a-e1f7-4d75-a73c-bbc010c7feeb"

#' Identify codes of departments intersecting a region of interest
#'
#' This function takes a region of interest (ROI) as an `sf` or `sfc` object
#' and returns the codes of departments intersecting this region.
#'
#' @param roi A spatial object of type `sf` or `sfc` representing the region of interest.
#' Use only the first geometry (polygon).
#' @return A character vector containing the codes of departments intersecting the region of interest.
#' @examples
#' library(sf)
#' roi <- sf::st_as_sfc(sf::st_bbox(
#'   c(xmin = 600000, ymin = 6600000, xmax = 650000, ymax = 6650000),
#'   crs = 2154)
#' )
#' departments <- get_departements_for_roi(roi)
#' @export
get_departements_for_roi <- function(roi) {
  response <- httr::GET(.dpt_url)
  dpts <- sf::st_read(
    rawToChar(httr::content(response, "raw")),
    quiet = TRUE
  ) |>
    sf::st_transform(2154)
  # Reproject the region of interest to Lambert-93 (EPSG:2154)
  roi <- sf::st_transform(roi, 2154)
  # Use the first polygon of the region of interest
  roi_geom <- roi[1, ]
  # Identify departments intersecting the region of interest
  hits <- sf::st_intersects(dpts, roi_geom, sparse = FALSE)[, 1]
  # Return the codes of intersected departments
  dpts$code[hits]
}
