#' Constructor for Intake Parameters
#'
#' Creates and configures the trophic flux table with a simplified syntax.
#'
#' @param x A `spacemodel` object or a `trophic_tbl`.
#' @param ... Flux definitions.
#'   - Key `"Target"` (e.g., `"Fox" = 0.5`): applies to all links pointing to Fox.
#'   - Key `"Source -> Target"` (e.g., `"Soil -> Worm" = 0.8`): targets a specific link.
#'   Values can be numeric (linear coefficient), formulas, or functions.
#' @param default The default function for unspecified links (default is identity).
#' @param normalize Logical. Whether to normalize diet weights (default TRUE).
#'
#' @return A `trophic_tbl` with a configured `flux` column.
#' @export
intake <- function(x, ..., default = NULL, normalize = TRUE) {

  # 1. Extraction and Normalization
  if (attr(x, "spacemodel")) {
    tbl <- attr(x, "trophic_tbl")
  } else if (inherits(x, "data.frame")) { # trophic_tbl inherits from data.frame
    tbl <- x
  } else {
    stop("Argument 'x' must be a spacemodel or a trophic_tbl.")
  }

  if (normalize) {
    # Assuming normalize_weights is available in the package
    tbl <- normalize_weights(tbl)
  }

  # 2. Handle Source/Target columns
  # Ensure 'from' and 'to' columns exist for matching
  if (!all(c("from", "to") %in% names(tbl))) {
    if ("link" %in% names(tbl)) {
      # Assuming link[[i]] = c(source, target)
      tbl$from <- sapply(tbl$link, `[`, 1)
      tbl$to   <- sapply(tbl$link, `[`, 2)
    } else {
      stop("The input table must have 'from'/'to' columns or a 'link' column.")
    }
  }

  # 3. Initialize with Default
  if (is.null(default)) {
    default_fun <- function(x) x
  } else {
    default_fun <- .parse_flux_input(default)
  }

  flux_list <- lapply(seq_len(nrow(tbl)), function(i) default_fun)

  # 4. Process arguments (...)
  args <- list(...)

  if (length(args) > 0) {
    if (is.null(names(args)) || any(names(args) == "")) {
      stop("All arguments in '...' must be named (e.g., 'Target' or 'Source -> Target').")
    }

    # Split general rules and specific rules to handle priority
    # Priority: Specific > General > Default
    keys <- names(args)
    is_link_rule <- grepl("->", keys)

    # First: Apply General Rules (by Target)
    for (key in keys[!is_link_rule]) {
      val_fun <- .parse_flux_input(args[[key]])

      # If key is a valid Target
      if (key %in% tbl$to) {
        indices <- which(tbl$to == key)
        for (i in indices) flux_list[[i]] <- val_fun
      } else {
        warning(sprintf("Target species '%s' not found in the trophic web. Ignored.", key))
      }
    }

    # Second: Overwrite with Specific Rules (Source -> Target)
    for (key in keys[is_link_rule]) {
      val_fun <- .parse_flux_input(args[[key]])

      # Parse the string "Source -> Target"
      parts <- strsplit(key, "\\s*->\\s*")[[1]]
      if (length(parts) != 2) {
        stop(sprintf("Invalid link format: '%s'. Please use 'Source -> Target'.", key))
      }

      src_name <- parts[1]
      tgt_name <- parts[2]

      # Find the specific link
      idx <- which(tbl$from == src_name & tbl$to == tgt_name)

      if (length(idx) > 0) {
        flux_list[[idx]] <- val_fun
      } else {
        warning(sprintf("Link '%s -> %s' not found in the trophic web. Ignored.", src_name, tgt_name))
      }
    }
  }

  tbl$flux <- flux_list
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
    # Convert formula -> function
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
