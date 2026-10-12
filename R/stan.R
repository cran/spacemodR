#' Calibrate direct transfer model
#'
#' @description
#' Fits a Bayesian direct transfer model using Stan. The model handles continuous
#' and categorical explanatory variables to predict a response variable.
#'
#' @param data A named list containing the data formatted for the Stan model.
#'   Required elements are: \code{N}, \code{K}, \code{M}, \code{y}, \code{x},
#'   \code{x_cat}, and \code{x_sim}.
#' @param ... Additional arguments passed to \code{\link[rstan]{sampling}}
#'   (e.g., \code{iter}, \code{chains}, \code{cores}, \code{control}).
#'
#' @return A \code{stanfit} object containing the posterior samples.
#' @export
calibrate_direct <- function(data, ...) {
  # 1. Check if data is a list
  if (!is.list(data)) {
    stop("`data` must be a named list.")
  }

  # 2. Check for required variables based on the Stan model 'data' block
  req_vars <- c("N", "K", "M", "y", "x", "x_cat", "x_sim")
  missing_vars <- setdiff(req_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("The following required variables are missing from `data`: ",
         paste(missing_vars, collapse = ", "))
  }

  # 3. Check if compiled Stan models object exists
  if (!exists("stanmodels") || is.null(stanmodels$stan_code_direct)) {
    stop("Compiled Stan model 'stan_code_direct' not found. ",
         "Ensure the package was properly compiled with rstantools.")
  }

  # 4. Run the sampler
  out <- rstan::sampling(
    object = stanmodels$stan_code_direct,
    data = data,
    ...
  )

  return(out)
}

#' Calibrate simple transfer model
#'
#' @description
#' Fits a Bayesian simple transfer model using Stan. The model handles continuous
#' and categorical explanatory variables to predict a response variable.
#'
#' @param data A named list containing the data formatted for the Stan model.
#'   Required elements are: \code{N}, \code{M}, \code{y}, \code{x},
#'   and \code{x_sim}.
#' @param ... Additional arguments passed to \code{\link[rstan]{sampling}}
#'   (e.g., \code{iter}, \code{chains}, \code{cores}, \code{control}).
#'
#' @return A \code{stanfit} object containing the posterior samples.
#' @export
calibrate_simple <- function(data, ...) {
  # 1. Check if data is a list
  if (!is.list(data)) {
    stop("`data` must be a named list.")
  }

  # 2. Check for required variables based on the Stan model 'data' block
  req_vars <- c("N", "M", "y", "x", "x_sim")
  missing_vars <- setdiff(req_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("The following required variables are missing from `data`: ",
         paste(missing_vars, collapse = ", "))
  }

  # 3. Check if compiled Stan models object exists
  if (!exists("stanmodels") || is.null(stanmodels$stan_code_simple)) {
    stop("Compiled Stan model 'stan_code_simple' not found. ",
         "Ensure the package was properly compiled with rstantools.")
  }

  # 4. Run the sampler
  out <- rstan::sampling(
    object = stanmodels$stan_code_simple,
    data = data,
    ...
  )

  return(out)
}



#' Calibrate trophic transfer model
#'
#' @description
#' Fits a Bayesian trophic transfer model using Stan. This model calibrates
#' relationships across multiple trophic compartments.
#'
#' @param data A named list containing the data formatted for the Stan model.
#'   Required elements are: \code{Np}, \code{Nw}, \code{Nh}, \code{Ni}, \code{M},
#'   \code{yp}, \code{xp}, \code{yw}, \code{xw}, \code{yh}, \code{xh}, \code{yi},
#'   \code{xi}, and \code{x_sim}.
#' @param ... Additional arguments passed to \code{\link[rstan]{sampling}}
#'   (e.g., \code{iter}, \code{chains}, \code{cores}, \code{control}).
#'
#' @return A \code{stanfit} object containing the posterior samples.
#' @export
calibrate_trophic <- function(data, ...) {
  # 1. Check if data is a list
  if (!is.list(data)) {
    stop("`data` must be a named list.")
  }

  # 2. Check for required variables based on the Stan model 'data' block
  req_vars <- c("Np", "Nw", "Nh", "Ni", "M", "yp", "xp", "yw", "xw",
                "yh", "xh", "yi", "xi", "x_sim")
  missing_vars <- setdiff(req_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("The following required variables are missing from `data`: ",
         paste(missing_vars, collapse = ", "))
  }

  # 3. Check if compiled Stan models object exists
  if (!exists("stanmodels") || is.null(stanmodels$stan_code_trophic)) {
    stop("Compiled Stan model 'stan_code_trophic' not found. ",
         "Ensure the package was properly compiled with rstantools.")
  }

  # 4. Run the sampler
  out <- rstan::sampling(
    object = stanmodels$stan_code_trophic,
    data = data,
    ...
  )

  return(out)
}
