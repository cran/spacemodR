#' Constructor for Flux Parameters
#'
#' Initializes the trophic flux table for a spacemodel.
#'
#' @param x A `spacemodel` object or a `trophic_tbl`.
#' @param default The default function or coefficient for unspecified links (default is 1, meaning full transfer: f(x) = x).
#' @param normalize Logical. Whether to normalize diet weights (default TRUE).
#'
#' @return A `trophic_tbl` with an initialized `flux` column.
#' @export
flux <- function(x, default = 1, normalize = TRUE) {

  # 1. Extraction and Normalization
  if (inherits(x, "spacemodel") || isTRUE(attr(x, "spacemodel"))) {
    tbl <- attr(x, "trophic_tbl")
  } else if (inherits(x, "trophic_tbl") || inherits(x, "data.frame")) {
    tbl <- x
  } else {
    stop("Argument 'x' must be a spacemodel or a trophic_tbl.")
  }

  if (normalize) {
    # On suppose que la fonction normalize_weights existe dans ton package
    tbl <- normalize_weights(tbl)
  }

  # 2. Vérification de l'existence des colonnes from/to
  if (!all(c("from", "to") %in% names(tbl))) {
    if ("link" %in% names(tbl)) {
      tbl$from <- sapply(tbl$link, `[`, 1)
      tbl$to   <- sapply(tbl$link, `[`, 2)
    } else {
      stop("The input table must have 'from'/'to' columns or a 'link' column.")
    }
  }

  # 3. Initialisation avec le paramètre par défaut
  if (is.null(default)) {
    default_fun <- function(x) x
  } else if (length(default) == 1 && is.na(default)) {
    default_fun <- function(x) NA
  } else {
    default_fun <- .parse_flux_input(default)
  }

  tbl$flux <- lapply(seq_len(nrow(tbl)), function(i) default_fun)

  return(tbl)
}


#' Add a Flux Rule
#'
#' Adds a specific flux rule to a trophic link or target in a trophic table.
#'
#' @param tbl A `trophic_tbl` object with an initialized `flux` column (output of `flux()`).
#' @param from Character. The source species. If `NULL` (default), the rule applies to all links pointing to `to`.
#' @param to Character. The target species.
#' @param value Numeric, formula, or function. The flux definition.
#'
#' @return The updated `trophic_tbl`.
#' @export
add_flux <- function(tbl, from = NULL, to, value) {

  if (!"flux" %in% names(tbl)) {
    stop("The table does not have a 'flux' column. Run `flux()` first.")
  }

  val_fun <- .parse_flux_input(value)

  if (is.null(from)) {
    # Règle générale : s'applique à tous les liens pointant vers la cible 'to'
    if (!to %in% tbl$to) {
      warning(sprintf("Target species '%s' not found in the trophic web. Ignored.", to))
    } else {
      indices <- which(tbl$to == to)
      for (i in indices) tbl$flux[[i]] <- val_fun
    }
  } else {
    # Règle spécifique : 'from' -> 'to'
    idx <- which(tbl$from == from & tbl$to == to)

    if (length(idx) > 0) {
      tbl$flux[[idx]] <- val_fun
    } else {
      warning(sprintf("Link '%s -> %s' not found in the trophic web. Ignored.", from, to))
    }
  }

  return(tbl)
}

#' Internal helper to parse input into functions
#' @noRd
.parse_flux_input <- function(input) {
  if (is.function(input)) {
    return(input)
  } else if (is.numeric(input)) {
    force(input)
    return(function(x) x * input)
  } else if (inherits(input, "formula")) {
    # Convertit une formule (ex: ~ 10^x / 32) en fonction
    f_env <- environment(input)
    f_body <- input[[2]]

    wrapper <- function(x) {}
    body(wrapper) <- f_body
    environment(wrapper) <- f_env
    return(wrapper)
  } else {
    stop("Invalid input for flux. Must be a function, a number (coefficient), or a formula.")
  }
}
