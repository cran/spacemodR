#' Transfer (food, contaminant) across trophic levels
#'
#' Computes the transfer through a trophic network from
#' lower to higher trophic levels using spatial spreading and intake functions.
#'
#' @param spacemodel A named list of spatial layers (e.g. \code{SpatRaster} objects).
#'        Must contain an attribute \code{trophic_tbl} of class \code{trophic_tbl}.
#' @param kernels A list of kernel parameters for each layer.
#' @param intakes A \code{trophic_tbl} object (or compatible table) containing
#'        normalized weights and flux functions for each trophic link.
#' @param exposure_weighting Character. Defines how the realized exposure is calculated
#' based on the predator's presence. Options are:
#' \itemize{
#'   \item \code{"local"} (Default): Weight exposure by the local habitat value (`predator_habitat`).
#'   Assumes the predator's intake is strictly proportional to the habitat quality/density
#'   of the pixel where it resides.
#'   \item \code{"diffuse"}: Weight exposure by a dispersed habitat kernel.
#'   Represents a "neighborhood" effect where the predator's presence is smoothed
#'   over its home range. Useful to avoid edge effects where a predator on a poor pixel
#'   surrounded by good habitat would otherwise have 0 exposure.
#'   \item \code{"potential"}: No weighting. Returns the pure environmental offer (potential exposure)
#'   regardless of the predator's density. Useful for identifying risk hotspots.
#' }
#' @param verbose Logical. If \code{TRUE}, prints progress information.
#'
#' @details
#' The function processes layers in ascending trophic order, as defined
#' by the \code{level} attribute of the trophic table.
#'
#' For each layer:
#' \enumerate{
#'   \item Resources (lower neighbors) are identified.
#'   \item Concentration from each resource is spatially spread using \code{spread()}.
#'   \item Intake is computed using \code{intake()}.
#'   \item Contributions from all resources are summed.
#' }
#'
#' The function assumes that intake weights are already normalized so that,
#' for each consumer, the sum of contributions from all resources equals 1.
#'
#' @return A named `spacemodel` object as a list of spatial layers representing
#'  values after transfer.
#'
#' @export
transfer <- function(
    spacemodel,
    kernels,
    intakes=NULL,
    exposure_weighting = "local",
    verbose = FALSE) {

  # Checking arguments
  stopifnot(!is.null(attr(spacemodel, "trophic_tbl")))
  trophic_tbl <- attr(spacemodel, "trophic_tbl")
  levels <- attr(trophic_tbl, "level") # Ordered list needed

  # Clone to do not change the original during computing
  uptake_stack <- as.list(spacemodel)
  names(uptake_stack) <- names(spacemodel)

  # loop over trophic levels (bottom to top)
  for (predator_name in names(levels)) {

    ressources <- lower_neighbors(trophic_tbl, predator_name)

    # if no ressources, increment in for loop
    if (length(ressources) == 0) next

    if (verbose) message("Processing Consumer: ", predator_name)

    # Init uptake_raster for the consumer (empty)
    predator_habitat <- uptake_stack[[predator_name]]
    total_out <- terra::init(predator_habitat, 0)

    # Kernel of the consumer (searching for the resource)
    pred_kernel <- kernels[[predator_name]]

    # check if kernel are available (otherwise, no consumer searching process, only habitat spreading)
    use_diffusion <- !is.null(pred_kernel) && is.matrix(pred_kernel)

    # --------------------
    # weighting_method
    # --------------------
    weight_map <- .compute_predator_weight(
      predator_habitat,
      pred_kernel,
      exposure_weighting,
      use_diffusion)

    for (prey_name in ressources) {

      if (verbose) message("  -> Input from: ", prey_name)

      out_r_map <- compute_exposure_map(
        prey_conc = uptake_stack[[prey_name]],
        predator_hab = predator_habitat,
        weight_map = weight_map,
        kernel = pred_kernel,
        use_diffusion = use_diffusion
      )

      # INTAKE / FLUX
      if(!is.null(intakes)) {
        out_r_map <- flux(out_r_map, intakes, from = prey_name, to = predator_name)
      }
      ## accumulation
      total_out <- total_out + out_r_map
    }
    # CLEANING FINAL
    uptake_stack[[predator_name]] <- terra::mask(total_out, predator_habitat)
  }

  # Reconstruction de l'objet spacemodel final
  # Note: `raster_stack` et `spacemodel` sont des constructeurs
  stack_transfer <- raster_stack(
    raster_list = uptake_stack,
    names = names(uptake_stack)
  )

  return(spacemodel(stack_transfer, trophic_tbl))
}


#' Compute the realized exposure map for a specific link
#'
#' @param prey_conc SpatRaster. Concentration of the prey/resource.
#' @param predator_hab SpatRaster. Habitat of the predator.
#' @param weight_map SpatRaster or Numeric. The spatial weighting of the predator (local or dispersed).
#' @param kernel Matrix or NULL. The dispersal kernel of the predator.
#' @param use_diffusion Logical. Whether to apply convolution.
#'
#' @return SpatRaster. The map of exposure (concentration * access * presence).
#' @noRd
#'
compute_exposure_map <- function(prey_conc, predator_hab, weight_map, kernel, use_diffusion) {

  # 1. Accessibilité : On ne mange que ce qui est dans sa zone d'habitat
  # (Zone d'exclusion physique)
  accessible_prey <- terra::mask(prey_conc, predator_hab)

  # 2. Dispersion (OFFRE) : Potentiel de proies autour du point
  if (use_diffusion) {
    exposure_potential <- compute_dispersal(
      x = accessible_prey,
      method = "convolution",
      options = list(kernel = kernel),
      mask = NULL
    )
  } else {
    exposure_potential <- accessible_prey
  }

  # 3. Pondération (DEMANDE) : Présence réelle du prédateur
  if (is.numeric(weight_map) && weight_map == 1) {
    exposure_realized <- exposure_potential
  } else {
    exposure_realized <- exposure_potential * weight_map
  }

  return(exposure_realized)
}


#' Apply trophic flux from a resource layer
#'
#' Computes the contribution of a resource raster to a consumer layer using
#' the normalized weight and a flux function stored in the trophic table.
#'
#' @param raster A \code{SpatRaster} (or similar) representing exposure from the resource.
#' @param intakes A \code{trophic_tbl} object that must contain a column
#'        \code{normalized_weight} and a column \code{flux}.
#' @param from Character string, name of the source node.
#' @param to Character string, name of the target node.
#'
#' @details
#' The function extracts the link corresponding to \code{from -> to} from the
#' trophic table and applies:
#' \enumerate{
#'   \item the associated flux function to the raster values
#'   \item the normalized weight of the link
#' }
#'
#' An error is thrown if no such link exists.
#'
#' @return A raster object with transformed values.
#'
#' @export
flux <- function(raster, intakes, from, to) {

  stopifnot(inherits(intakes, "trophic_tbl"))

  # Extract from/to vectors from the link column
  edges <- data.frame(
    from = sapply(intakes$link, `[[`, "from"),
    to   = sapply(intakes$link, `[[`, "to"),
    stringsAsFactors = FALSE
  )

  # Identify the matching link
  idx <- which(edges$from == from & edges$to == to)

  if (length(idx) == 0) {
    stop(sprintf("No link between %s and %s", from, to))
  }

  # Apply flux function and normalized weight
  flux_fun <- intakes$flux[[idx]]
  w <- intakes$normalized_weight[idx]
  if(!is.null(w)){
    raster[] <- w * flux_fun(raster[])
  } else{
    raster[] <- flux_fun(raster[])
  }
  raster
}

# -------------------
# INTERNAL FUNCTION
# -------------------

#' Petite fonction helper pour alléger le code principal
#' Calcule la carte de pondération du prédateur
#' @noRd
.compute_predator_weight <- function(habitat, kernel, method, use_diffusion) {
  if (method == "potential") return(1)

  if (method == "diffuse" && use_diffusion) {
    return(compute_dispersal(
      x = habitat,
      method = "convolution",
      options = list(kernel = kernel),
      mask = habitat
    ))
  }
  # Default "local"
  return(habitat)
}
