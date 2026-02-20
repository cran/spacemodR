#' Efficiently retrieve OCS GE data from a remote FlatGeobuf
#'
#' @description
#' This function retrieves OCS GE (Land Cover) data for a specific Region of Interest (ROI)
#' directly from a remote FlatGeobuf (.fgb) file hosted on a server (e.g., S3).
#'
#' It leverages GDAL's virtual file system (`/vsicurl/`) and the spatial indexing capabilities
#' of FlatGeobuf to download only the data chunks intersecting the bounding box of the ROI,
#' making it highly efficient for large datasets.
#'
#' @param roi An \code{\link[sf]{sf}} object defining the Region Of Interest.
#' It can be in any projection, but will be transformed to EPSG:2154 (Lambert-93) internally.
#' @param fgb_url Character string. The public URL to the remote `.fgb` file.
#'
#' @return An \code{\link[sf]{sf}} object containing the OCS GE polygons intersected by the ROI,
#' projected in Lambert-93 (EPSG:2154).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Transforms the \code{roi} to Lambert-93 (EPSG:2154).
#'   \item calculates the bounding box of the \code{roi}.
#'   \item Uses \code{sf::st_read} with a WKT filter to fetch only relevant features from the remote file.
#'   \item Applies a precise geometric intersection (`st_intersection`) to clip the data to the exact shape of the \code{roi}.
#' }
#'
#' @note
#' This function requires a working internet connection and GDAL support for the
#' FlatGeobuf driver and network capabilities (vsicurl).
#'
#' @examples
#' \dontrun{
#'   library(sf)
#'
#'   # 1. Define a Region of Interest (ROI)
#'   # Example: A small bounding box in France
#'   my_roi <- st_as_sf(data.frame(
#'     lon = c(2.3, 2.4, 2.4, 2.3, 2.3),
#'     lat = c(48.8, 48.8, 48.9, 48.9, 48.8)
#'   ), coords = c("lon", "lat"), crs = 4326)
#'
#'   # 2. URL to the remote FlatGeobuf file
#'   # (Replace with the actual URL of your OCS GE bucket)
#'   url_fgb <- "https://example.com/data/ocsge_grand_est.fgb"
#'
#'   # 3. Fetch data
#'   ocsge_data <- get_ocsge_data_fgb(roi = my_roi, fgb_url = url_fgb)
#'
#'   # 4. Check result
#'   print(ocsge_data)
#'   plot(st_geometry(ocsge_data))
#' }
#'
#' @importFrom sf st_transform st_bbox st_as_text st_as_sfc st_read st_intersection
#' @export
get_ocsge_data_fgb <- function(roi, fgb_url) {
  # 1. PROJECTION
  roi <- sf::st_transform(roi, 2154)
  # 2. CLEAN CLOUD READING
  # Le préfixe /vsicurl/ indique à GDAL (utilisé par sf) de lire via HTTP
  dsn <- paste0("/vsicurl/", fgb_url)
  # 3. BOUDING BOX OF ROI
  bbox <- sf::st_bbox(roi)
  # 4. FILTER READING
  tryCatch({
    res <- sf::st_read(
      dsn,
      wkt_filter = sf::st_as_text(sf::st_as_sfc(bbox)),
      quiet = TRUE
    )
    # 5. Intersection précise (optionnel mais recommandé)
    res <- sf::st_intersection(res, roi)
    return(res)
  }, error = function(e) {
    stop("Error when reading FGB on S3. Check URL and Internet connection.\n", e)
  })
}




