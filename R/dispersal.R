#' Disperse a species or variable over the landscape
#'
#' @description
#' Applies a dispersal mechanism to a specific layer of the `spacemodel` object.
#' This function acts as a wrapper around \code{\link{compute_dispersal}} to handle
#' the `spacemodel` class structure.
#'
#' @param spacemodel A \code{spacemodel} object.
#' @param layer Character or Integer. The name or index of the layer to disperse
#' (e.g., "Fox", 1).
#' @param method Character. The dispersal method to use. Options are:
#' \itemize{
#'   \item \code{"convolution"} (default): Uses a moving window (kernel).
#'   \item \code{"omniscape"}: Uses Circuit Theory (via Julia and Omniscape.jl).
#' }
#' @param method_option A list of parameters specific to the chosen method:
#' \itemize{
#'   \item For \code{"convolution"}: must contain \code{kernel} (a matrix).
#'   \item For \code{"omniscape"}: must contain \code{resistance} (SpatRaster) and \code{radius} (numeric).
#' }
#'
#' @return The \code{spacemodel} object with the specified layer updated with dispersed values.
#'
#' @examples
#' \dontrun{
#' # 1. Convolution example
#' my_kernel <- matrix(1, nrow=3, ncol=3)
#' sm_updated <- dispersal(sm, layer = "Predator", method = "convolution",
#'                         method_option = list(kernel = my_kernel))
#'
#' # 2. Omniscape example (requires Julia)
#' sm_updated <- dispersal(sm, layer = "Predator", method = "omniscape",
#'                         method_option = list(resistance = res_map, radius = 10))
#' }
#'
#' @seealso \code{\link{compute_dispersal}}
#' @export
dispersal <- function(spacemodel,
                      layer = 1,
                      method = "convolution",
                      method_option = list()) {

  # Secure raster for computing
  input_rast <- spacemodel[[layer]]
  # Compute dispersal using generic engine
  out_rast <- compute_dispersal(
    x = input_rast,
    method = method,
    options = method_option,
    mask = input_rast # Apply mask to keep original NAs
  )
  # Update the spacemodel layer
  spacemodel[[layer]] <- out_rast
  return(spacemodel)
}
# ===============================
# Dispersal Method
# ===============================

#' Compute dispersal or spread map (Generic Engine)
#'
#' @description
#' Low-level function to apply spatial processing (Convolution or External algorithms)
#' to a raster. It handles NA masking and method dispatch to Julia if necessary.
#'
#' @param x SpatRaster. Source layer to disperse.
#' @param method Character. "convolution" or "omniscape" (placeholder).
#' @param options List. Parameters (e.g., `kernel` for convolution).
#' @param mask SpatRaster (optional). A mask to apply after dispersion (e.g., maintain original NA structure).
#'
#' @return A \code{\link[terra]{SpatRaster}} object containing the dispersed values.
#'
#' @export
compute_dispersal <- function(x, method = "convolution", options = list(), mask = NULL) {

  # 1. Dispatch Method
  if (method == "convolution") {
    # --- CONVOLUTION (Terra) ---
    if (is.null(options$kernel) || !is.matrix(options$kernel)) {
      stop("Method 'convolution' requires a 'kernel' matrix in 'method_option'.")
    }

    out <- terra::focal(
      x,
      w = options$kernel,
      fun = "sum",
      na.policy = "omit",
      fillvalue = 0,
      expand = FALSE
    )

  } else if (method == "omniscape") {
    # --- OMNISCAPE (Julia) ---
    # Cette méthode nécessite Julia et le package Omniscape.jl
    out <- run_julia_omniscape(x, options)

  } else {
    stop(sprintf("Dispersal method '%s' is not recognized.", method))
  }

  # 2. Mask (Smart handling)
  if (!is.null(mask)) {
    out <- terra::mask(out, mask)
  }

  return(out)
}

#' @section Using Omniscape (Julia):
#' To use `method = "omniscape"`, you must have Julia installed on your machine
#' and the `JuliaCall` R package.
#'
#' \strong{First-time setup:}
#' \enumerate{
#'   \item Install Julia from \url{https://julialang.org/downloads/}
#'   \item In R, install JuliaCall: \code{install.packages("JuliaCall")}
#'   \item Setup Julia in R: \code{JuliaCall::julia_setup(installJulia = TRUE)}
#'   \item Install the Omniscape package in Julia (via R):
#'     \code{JuliaCall::julia_install_package("Omniscape")}
#' }
#'
#' \strong{Usage example:}
#' \preformatted{
#' # 1. Initialize Julia (Required at the start of the session)
#' library(JuliaCall)
#' julia_setup()
#'
#' # 2. Define inputs
#' my_habitat <- ... # SpatRaster
#' my_resistance <- ... # SpatRaster (1 = low resistance, 100 = high)
#'
#' # 3. Run dispersal
#' # This will compute connectivity using Circuit Theory
#' dispersed_map <- dispersal(
#'   spacemodel = my_model,
#'   layer = "Fox",
#'   method = "omniscape",
#'   method_option = list(
#'     resistance = my_resistance,
#'     radius = 15 # Moving window radius in pixels
#'   )
#' )
#' }
#'
#' @noRd
run_julia_omniscape <- function(x, options) {

  if (is.null(options$resistance) || is.null(options$radius)) {
    stop("Options 'resistance' and 'radius' are mandatory for omniscape.")
  }

  # --- A. PRÉPARATION DU DOSSIER TEMPORAIRE ---
  tmp_dir <- tempfile(pattern = "omniscape_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  src_file <- file.path(tmp_dir, "source.tif")
  res_file <- file.path(tmp_dir, "resistance.tif")
  out_dir  <- file.path(tmp_dir, "output")

  # --- B. EXPORT DES RASTERS (R -> Disque) ---
  src_rast <- x
  src_rast[is.na(src_rast)] <- 0

  terra::writeRaster(src_rast, src_file, overwrite = TRUE)
  terra::writeRaster(options$resistance, res_file, overwrite = TRUE)

  # --- C. CRÉATION DU FICHIER DE CONFIGURATION .INI ---
  ini_file <- file.path(tmp_dir, "config.ini")
  ini_content <- c(
    "[Required arguments]",
    sprintf("resistance_file = %s", res_file),
    sprintf("radius = %d", as.integer(options$radius)),
    sprintf("project_name = %s", out_dir),
    "",
    "[General options]",
    sprintf("source_file = %s", src_file),
    "block_size = 1",
    "source_from_resistance = false",
    "solver = cg+amg"
  )
  writeLines(ini_content, ini_file)

  # --- D. GÉNÉRATION DU SCRIPT JULIA ---
  jl_script <- file.path(tmp_dir, "run_omniscape.jl")

  # Julia ne fait plus que lire le fichier .ini !
  jl_code <- sprintf('
    using Pkg
    using Omniscape
    run_omniscape("%s")
  ', ini_file)

  writeLines(jl_code, jl_script)

  # --- E. EXÉCUTION ISOLÉE DE JULIA ---
  message("Running Omniscape in an isolated Julia sub-process...")
  status <- system2("julia", args = c(jl_script))

  if (status != 0) {
    stop("Omniscape Julia process failed. Check the logs above.")
  }

  # --- F. LECTURE DU RÉSULTAT (Disque -> R) ---
  out_file <- file.path(out_dir, "cum_currmap.tif")

  if (!file.exists(out_file)) {
    stop("Expected output file 'cum_currmap.tif' was not generated by Omniscape.")
  }

  out_rast_temp <- terra::rast(out_file)

  out_rast <- terra::rast(terra::ext(out_rast_temp),
                          resolution = terra::res(out_rast_temp),
                          crs = terra::crs(out_rast_temp))
  terra::values(out_rast) <- terra::values(out_rast_temp)

  out_rast[is.na(x)] <- NA

  return(out_rast)
}
