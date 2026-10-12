#' @noRd
.gbif_api_url <- "https://api.gbif.org/v1"

#' Retrieve species occurrences from GBIF
#'
#' @description
#' This function retrieves the occurrence records of a species from the
#' Global Biodiversity Information Facility (GBIF), with the public
#' occurrence search API. No account and no API key are needed.
#'
#' Only records with coordinates and without known geospatial issue are
#' returned.
#'
#' @param species Character. Scientific name of the species, for instance
#' \code{"Apodemus sylvaticus"}. Underscores are read as spaces, so the names
#' of \code{\link{ocsge_species_dict}} and \code{\link{eunis_species_dict}}
#' can be used directly.
#' @param roi An \code{\link[sf]{sf}} (or \code{sfc}) object defining the
#' Region Of Interest, in any projection. The search is made on its bounding
#' box, then only the records inside \code{roi} are kept. With \code{NULL}
#' (default), the search is made on the whole world.
#' @param limit Integer. Maximum number of records to download. Default is
#' 1000. The API does not return more than 100,000 records for one search
#' (see Details).
#' @param ... Other filters of the GBIF occurrence search API, as named
#' arguments. For instance \code{year = "2000,2024"},
#' \code{coordinateUncertaintyInMeters = "0,100"},
#' \code{basisOfRecord = "HUMAN_OBSERVATION"} or \code{country = "FR"}.
#' A range is written \code{"min,max"}.
#' @param api_url Character. Base URL of the GBIF API. Exposed mainly for
#' testing against a different endpoint.
#'
#' @return An \code{\link[sf]{sf}} object of points in EPSG:4326, one row for
#' each record, with the columns:
#' \describe{
#'   \item{key}{GBIF identifier of the record
#'   (\code{https://www.gbif.org/occurrence/<key>}).}
#'   \item{species}{Scientific name of the species.}
#'   \item{coordinateUncertaintyInMeters}{Uncertainty of the position, in
#'   meters. Often \code{NA}.}
#'   \item{eventDate, year}{Date and year of the observation.}
#'   \item{basisOfRecord}{Type of record, for instance
#'   \code{"HUMAN_OBSERVATION"} or \code{"PRESERVED_SPECIMEN"}.}
#'   \item{datasetKey}{GBIF identifier of the dataset
#'   (\code{https://www.gbif.org/dataset/<datasetKey>}).}
#'   \item{license}{License of the record.}
#' }
#' The total number of records matching the search in GBIF is stored in the
#' attribute \code{"gbif_count"}.
#'
#' @details
#' The name is first matched against the GBIF taxonomy
#' (\code{species/match}), then the records are downloaded by pages of 300
#' (\code{occurrence/search}).
#'
#' \strong{Limitations:}
#' \itemize{
#'   \item The search API stops at 100,000 records. For a larger request,
#'   split the region of interest, or the years with \code{year}.
#'   \item The search API gives no DOI. For a publication, GBIF asks to cite
#'   a download: see \url{https://www.gbif.org/citation-guidelines} and the
#'   function \code{occ_download()} of the 'rgbif' package.
#'   \item GBIF records are opportunistic observations. They show where the
#'   species was seen, not where it is absent, and the position can be
#'   uncertain by several kilometers.
#' }
#'
#' @note This function requires a working internet connection.
#'
#' @examples
#' \dontrun{
#'   data("roi_metaleurop")
#'   occ <- get_gbif_data("Apodemus sylvaticus", roi = roi_metaleurop)
#'   plot(sf::st_geometry(occ))
#'
#'   # recent records with a position known within 100 m
#'   occ <- get_gbif_data("Apodemus sylvaticus", roi = roi_metaleurop,
#'                        year = "2000,2024",
#'                        coordinateUncertaintyInMeters = "0,100")
#' }
#'
#' @seealso \code{\link{get_eunis_data}} and \code{\link{get_ocsge}} for the
#' land cover of the same region of interest.
#'
#' @export
get_gbif_data <- function(species, roi = NULL, limit = 1000, ...,
                          api_url = NULL) {

  if (is.null(api_url)) api_url <- .gbif_api_url
  if (!is.character(species) || length(species) != 1) {
    stop("`species` must be a single scientific name.")
  }
  if (!is.null(roi) && !inherits(roi, "sf") && !inherits(roi, "sfc")) {
    stop("`roi` must be an 'sf' or 'sfc' spatial object.")
  }

  # 1. TAXON - match the name against the GBIF taxonomy
  taxon <- .gbif_get(paste0(api_url, "/species/match"),
                     list(name = gsub("_", " ", species)))
  if (is.null(taxon$usageKey)) {
    stop("Species not found in the GBIF taxonomy: ", species)
  }

  # 2. FILTERS - the bounding box of the ROI, in longitude and latitude
  query <- list(taxonKey = taxon$usageKey, hasCoordinate = "true",
                hasGeospatialIssue = "false", ...)
  if (!is.null(roi)) {
    roi <- sf::st_transform(roi, 4326)
    bbox <- sf::st_bbox(roi)
    query$decimalLongitude <- paste(bbox[["xmin"]], bbox[["xmax"]], sep = ",")
    query$decimalLatitude <- paste(bbox[["ymin"]], bbox[["ymax"]], sep = ",")
  }

  # 3. DOWNLOAD - pages of 300 records, the maximum of the API
  limit <- min(limit, 100000)
  pages <- list()
  n <- 0
  repeat {
    page <- .gbif_get(paste0(api_url, "/occurrence/search"),
                      c(query, list(offset = n, limit = min(300, limit - n))))
    pages[[length(pages) + 1]] <- .gbif_table(page$results)
    n <- n + length(page$results)
    if (isTRUE(page$endOfRecords) || length(page$results) == 0 || n >= limit) break
  }
  out <- sf::st_as_sf(do.call(rbind, pages), coords = c("lon", "lat"), crs = 4326)

  # 5. ROI - the search was made on the bounding box
  if (!is.null(roi)) {
    out <- out[lengths(sf::st_intersects(out, roi)) > 0, ]
  }
  attr(out, "gbif_count") <- page$count
  out
}

# GET a page of the GBIF API and parse its JSON
#' @noRd
.gbif_get <- function(url, query) {
  resp <- httr::RETRY("GET", url, query = query, httr::timeout(60), times = 3,
                      quiet = TRUE)
  if (httr::http_error(resp)) {
    stop(sprintf("GBIF request failed (HTTP %s). Check the filters and the Internet connection.",
                 httr::status_code(resp)), call. = FALSE)
  }
  httr::content(resp, as = "parsed", type = "application/json", encoding = "UTF-8")
}

# Table of the records of a page; records do not all have the same fields
#' @noRd
.gbif_table <- function(records) {
  field <- function(name, missing) {
    vapply(records, function(r) if (is.null(r[[name]])) missing else r[[name]],
           missing)
  }
  data.frame(
    key = as.character(field("key", NA_real_)),
    species = field("species", NA_character_),
    coordinateUncertaintyInMeters = field("coordinateUncertaintyInMeters", NA_real_),
    eventDate = field("eventDate", NA_character_),
    year = as.integer(field("year", NA_real_)),
    basisOfRecord = field("basisOfRecord", NA_character_),
    datasetKey = field("datasetKey", NA_character_),
    license = field("license", NA_character_),
    lon = field("decimalLongitude", NA_real_),
    lat = field("decimalLatitude", NA_real_)
  )
}
