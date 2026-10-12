#' @noRd
.eunis_service_url <- paste0(
  "https://bio.discomap.eea.europa.eu/arcgis/rest/services/Ecosystem/",
  "EcosystemTypeMap_v3_1_Terrestrial/MapServer")

#' Efficiently retrieve EUNIS land cover data from the EEA Ecosystem Type Map
#'
#' @description
#' This function retrieves EUNIS (European Nature Information System) land
#' cover data for a Region of Interest (ROI), as an alternative to the French
#' OCS-GE nomenclature used by \code{\link{get_ocsge_data_fgb}}. It queries the
#' European Environment Agency (EEA) "Ecosystem Type Map v3.1, terrestrial
#' part" ArcGIS MapServer, a 100 m resolution, EEA-39-wide classified raster
#' available at EUNIS Level 1 (9 broad classes) or Level 2 (46 detailed
#' classes).
#'
#' @param roi An \code{\link[sf]{sf}} (or \code{sfc}) object defining the
#' Region Of Interest. It can be in any projection, but will be transformed to
#' EPSG:3035 (ETRS89-LAEA Europe) internally, the native grid of the source
#' service.
#' @param level Character. \code{"L1"} (9 broad classes, default) or
#' \code{"L2"} (46 detailed classes). See Details for a caveat on \code{"L2"}.
#' @param resolution Numeric. Requested pixel size in meters. Default is 100,
#' the native resolution of the source raster; values below 100 only
#' upsample without adding real detail. Use a coarser value for large ROIs to
#' stay under the service's image-size limit (see Details).
#' @param service_url Character. The ArcGIS MapServer base URL. Exposed mainly
#' for testing against a different endpoint.
#'
#' @return A categorical \code{\link[terra]{SpatRaster}} in EPSG:3035, with
#' cell values pointing to a factor level table (\code{\link[terra]{levels}})
#' giving the EUNIS \code{code} for that level (see \code{\link{ref_eunis}}
#' for the matching labels and colors). Cells outside the mapped terrestrial
#' extent (e.g. sea) are \code{NA}.
#'
#' @details
#' Unlike the OCS-GE FlatGeobuf source, this EUNIS layer has no raw-value
#' raster endpoint (no ArcGIS ImageServer is published for it) -- only a
#' symbolized MapServer \code{export}. This function requests that export
#' image at the native resolution with nearest-neighbor resampling (so pixel
#' colors are not blended at class boundaries), then decodes each pixel's
#' exact RGB color back into an EUNIS code using the fixed legend palette
#' shipped as \code{\link{ref_eunis}}.
#'
#' \strong{Limitations:}
#' \itemize{
#'   \item The service caps a single export at 4096 x 4096 pixels. At the
#'   default 100 m resolution that is about 410 x 410 km; larger ROIs must
#'   use a coarser \code{resolution} or be split and fetched in tiles.
#'   \item At \code{level = "L2"}, two pairs of classes are rendered with the
#'   identical color by the source service and cannot be told apart from the
#'   image alone: \code{D5}/\code{F6} and \code{D6}/\code{F7}. Affected pixels
#'   are returned with a combined code (e.g. \code{"D5/F6"}) rather than
#'   silently assigned to one of the two. \code{level = "L1"} has no such
#'   collision.
#'   \item Areas outside the mapped terrestrial extent (e.g. open sea) are
#'   detected from the export's alpha channel and returned as \code{NA}, not
#'   guessed from color.
#' }
#'
#' @note This function requires a working internet connection.
#'
#' @examples
#' \dontrun{
#'   library(sf)
#'
#'   my_roi <- st_as_sf(data.frame(
#'     lon = c(2.3, 2.4, 2.4, 2.3, 2.3),
#'     lat = c(48.8, 48.8, 48.9, 48.9, 48.8)
#'   ), coords = c("lon", "lat"), crs = 4326) |> st_combine() |> st_cast("POLYGON")
#'
#'   eunis_data <- get_eunis_data(roi = my_roi, level = "L1")
#'   terra::plot(eunis_data)
#' }
#'
#' @seealso \code{\link{ref_eunis}} for the EUNIS code/label/color lookup
#' table, and \code{\link{get_ocsge_data_fgb}} for the equivalent OCS-GE
#' fetcher.
#'
#' @importFrom sf st_transform st_bbox
#' @export
get_eunis_data <- function(roi,
                            level = c("L1", "L2"),
                            resolution = 100,
                            service_url = NULL) {

  level <- match.arg(level)
  if (is.null(service_url)) service_url <- .eunis_service_url

  if (!inherits(roi, "sf") && !inherits(roi, "sfc")) {
    stop("`roi` must be an 'sf' or 'sfc' spatial object.")
  }

  # 1. PROJECTION - EPSG:3035 is the service's native grid
  roi_3035 <- sf::st_transform(roi, 3035)
  bbox <- sf::st_bbox(roi_3035)

  width_m  <- bbox[["xmax"]] - bbox[["xmin"]]
  height_m <- bbox[["ymax"]] - bbox[["ymin"]]
  if (width_m <= 0 || height_m <= 0) {
    stop("`roi` bounding box is degenerate (zero width or height).")
  }

  ncol <- ceiling(width_m / resolution)
  nrow <- ceiling(height_m / resolution)
  if (ncol > 4096 || nrow > 4096) {
    stop(sprintf(
      "ROI too large for a single export at %gm resolution (%d x %d px; service limit is 4096 x 4096 px).\nIncrease `resolution` or split the ROI.",
      resolution, ncol, nrow
    ))
  }

  # snap the bbox so exported pixels align on exact `resolution` steps
  xmax <- bbox[["xmin"]] + ncol * resolution
  ymax <- bbox[["ymin"]] + nrow * resolution

  layer_id <- if (level == "L1") 1L else 0L

  query <- list(
    bbox = paste(bbox[["xmin"]], bbox[["ymin"]], xmax, ymax, sep = ","),
    bboxSR = 3035,
    imageSR = 3035,
    size = paste(ncol, nrow, sep = ","),
    format = "png32",
    interpolation = "RSP_NearestNeighbor",
    layers = paste0("show:", layer_id),
    transparent = "true",
    f = "image"
  )

  tmp <- tempfile(fileext = ".png")
  # world file: the PNG has no embedded georeferencing, and terra reads an
  # ungeoreferenced image upside down (row 1 at the bottom)
  tmp_wld <- sub("\\.png$", ".pgw", tmp)
  on.exit(unlink(c(tmp, tmp_wld)), add = TRUE)

  tryCatch({
    resp <- httr::GET(paste0(service_url, "/export"), query = query,
                       httr::write_disk(tmp, overwrite = TRUE))
    httr::stop_for_status(resp, task = "export EUNIS land cover image from the EEA service")
  }, error = function(e) {
    stop("Error when reading the EUNIS map service. Check URL and Internet connection.\n", e)
  })

  # pixel size x, rotation (2), pixel size y, center of the upper-left pixel (x, y)
  writeLines(sprintf("%.6f", c(resolution, 0, 0, -resolution,
                               bbox[["xmin"]] + resolution / 2,
                               ymax - resolution / 2)), tmp_wld)

  img <- terra::rast(tmp)
  names(img) <- c("r", "g", "b", "a")
  terra::ext(img) <- terra::ext(bbox[["xmin"]], xmax, bbox[["ymin"]], ymax)
  terra::crs(img) <- "EPSG:3035"

  # 2. DECODE the render color back into an EUNIS code via the fixed legend palette
  ref <- spacemodR::ref_eunis[spacemodR::ref_eunis$level == level, ]
  rgb_key <- ref$r * 65536L + ref$g * 256L + ref$b
  code_by_key <- tapply(ref$code, rgb_key, paste, collapse = "/")

  v <- terra::values(img)
  pixel_key <- v[, "r"] * 65536L + v[, "g"] * 256L + v[, "b"]
  code <- unname(code_by_key[as.character(pixel_key)])
  code[v[, "a"] == 0] <- NA_character_ # outside the mapped (terrestrial) extent

  code_f <- factor(code)

  out <- img[[1]]
  terra::values(out) <- as.integer(code_f)
  levels(out) <- data.frame(id = seq_along(levels(code_f)), code = levels(code_f))
  names(out) <- paste0("eunis_", level)

  out
}
