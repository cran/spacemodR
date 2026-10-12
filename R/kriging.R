# ===============================
# Kriging of soil concentrations
# ===============================
#
# Self-contained implementation (sf + terra + stats only) of:
#   - the empirical semi-variogram,
#   - the weighted least-squares fit of a variogram model,
#   - ordinary kriging (OK) and kriging with external drift (KED, also called
#     universal kriging with covariates),
#   - k-fold cross-validation (random or spatial blocks).
#
# Conventions follow gstat (Pebesma 2004): a model is defined by a `nugget`,
# a partial sill `psill` and a `range` parameter, so that the semi-variance is
#   gamma(h) = nugget + psill * (1 - rho(h / range))   for h > 0.

# -------------------------------
# Internal helpers
# -------------------------------

#' @noRd
.vgm_models <- c("Exp", "Sph", "Gau", "Mat")

#' Correlation function rho(h) of a variogram model (without nugget)
#' @noRd
.vgm_correlation <- function(h, model, range, kappa = 0.5) {
  u <- h / range
  switch(model,
    Exp = exp(-u),
    Sph = ifelse(u < 1, 1 - 1.5 * u + 0.5 * u^3, 0),
    Gau = exp(-u^2),
    Mat = {
      out <- rep(1, length(u))
      pos <- u > 0
      up <- u[pos]
      out[pos] <- (up^kappa * besselK(up, kappa)) / (2^(kappa - 1) * gamma(kappa))
      out[!is.finite(out)] <- 0
      out
    },
    stop("Unknown variogram model: ", model,
         ". Choose one of ", paste(.vgm_models, collapse = ", "))
  )
}

#' Covariance C(h) of a fitted soil variogram (nugget only at h == 0)
#' @noRd
.vgm_covariance <- function(h, vgm) {
  cov <- vgm$psill * .vgm_correlation(h, vgm$model, vgm$range, vgm$kappa)
  cov[h == 0] <- cov[h == 0] + vgm$nugget
  cov
}

#' Semi-variance gamma(h) of a variogram model
#' @noRd
.vgm_gamma <- function(h, model, nugget, psill, range, kappa = 0.5) {
  g <- nugget + psill * (1 - .vgm_correlation(h, model, range, kappa))
  g[h == 0] <- 0
  g
}

#' Pairwise Euclidean distances between two coordinate matrices
#' @noRd
.cross_dist <- function(a, b) {
  d2 <- outer(a[, 1], b[, 1], "-")^2 + outer(a[, 2], b[, 2], "-")^2
  sqrt(d2)
}

#' Check and prepare the point data set
#'
#' Returns a list with coordinates, (transformed) values and the drift matrix.
#' Exact duplicated locations are averaged (kriging matrices would be singular).
#' @noRd
.prepare_soil_points <- function(points, value, log10 = TRUE,
                                 covariates = NULL, verbose = TRUE) {
  if (!inherits(points, "sf")) stop("`points` must be an sf object.")
  if (!all(sf::st_geometry_type(points) == "POINT")) {
    stop("`points` must only contain POINT geometries.")
  }
  if (sf::st_is_longlat(points)) {
    stop("`points` must be in a projected CRS in metres (e.g. EPSG:2154), ",
         "use sf::st_transform() first.")
  }
  if (!value %in% names(points)) stop("No column named '", value, "' in `points`.")

  z <- points[[value]]
  if (!is.numeric(z)) stop("Column '", value, "' must be numeric.")
  keep <- !is.na(z)
  if (any(!keep) && verbose) message(sum(!keep), " point(s) with NA '", value, "' removed.")
  points <- points[keep, ]
  z <- z[keep]

  if (log10) {
    if (any(z <= 0)) stop("log10 = TRUE requires strictly positive values in '", value, "'.")
    z <- log10(z)
  }

  xy <- sf::st_coordinates(points)[, 1:2, drop = FALSE]

  # Drift (trend) matrix: intercept + covariate values at the points
  drift <- matrix(1, nrow = length(z), ncol = 1, dimnames = list(NULL, "intercept"))
  if (!is.null(covariates)) {
    covariates <- .check_covariates(covariates, points)
    cov_val <- as.matrix(terra::extract(covariates, terra::vect(points), ID = FALSE))
    ok <- stats::complete.cases(cov_val)
    if (any(!ok)) {
      if (verbose) message(sum(!ok), " point(s) outside the covariate rasters removed.")
      z <- z[ok]; xy <- xy[ok, , drop = FALSE]; cov_val <- cov_val[ok, , drop = FALSE]
      drift <- drift[ok, , drop = FALSE]
    }
    drift <- cbind(drift, cov_val)
  }

  # Average exact duplicated locations
  key <- paste(xy[, 1], xy[, 2])
  if (anyDuplicated(key)) {
    n_dup <- sum(duplicated(key))
    if (verbose) message(n_dup, " duplicated location(s) averaged.")
    grp <- match(key, unique(key))
    z <- as.vector(tapply(z, grp, mean))
    xy <- xy[!duplicated(key), , drop = FALSE]
    drift_names <- colnames(drift)
    drift <- vapply(seq_len(ncol(drift)),
                    function(j) as.vector(tapply(drift[, j], grp, mean)),
                    numeric(length(z)))
    drift <- matrix(drift, ncol = length(drift_names),
                    dimnames = list(NULL, drift_names))
  }

  list(xy = xy, z = z, drift = drift, crs = sf::st_crs(points))
}

#' @noRd
.check_covariates <- function(covariates, points = NULL) {
  if (!inherits(covariates, "SpatRaster")) stop("`covariates` must be a SpatRaster.")
  if (any(names(covariates) == "intercept")) stop("A covariate cannot be named 'intercept'.")
  if (!is.null(points) && sf::st_crs(points) != sf::st_crs(terra::crs(covariates))) {
    stop("`points` and `covariates` must share the same CRS.")
  }
  covariates
}

# -------------------------------
# Empirical variogram
# -------------------------------

#' Empirical semi-variogram of soil concentrations
#'
#' @description
#' Computes the experimental (empirical) semi-variogram of a soil concentration
#' measured at sampling points: for every pair of points separated by a
#' distance falling in a given distance class (a *lag*), half of the squared
#' difference of their values is averaged.
#'
#' When `covariates` are given, the variogram is computed on the residuals of
#' the linear regression of the values on the covariates (this is the variogram
#' needed for kriging with external drift, see [krige_soil()]).
#'
#' @param points An `sf` object of POINT geometries in a projected CRS (metres).
#' @param value Character. Name of the column holding the concentration.
#' @param log10 Logical. Work on `log10(value)`? (default `TRUE`, recommended
#'   for skewed concentration data).
#' @param covariates Optional `SpatRaster` whose layers are used as linear drift
#'   (trend) covariates.
#' @param cutoff Numeric. Maximal distance (m) considered. Default: one third of
#'   the diagonal of the bounding box of the points.
#' @param width Numeric. Width (m) of the distance classes. Default `cutoff / 15`.
#' @param estimator Character. `"classical"` (Matheron) or `"robust"`
#'   (Cressie-Hawkins, less sensitive to outliers).
#'
#' @return A `data.frame` of class `soil_variogram` with columns `np` (number
#'   of pairs), `dist` (mean distance of the pairs) and `gamma` (semi-variance).
#'
#' @seealso [fit_soil_variogram()], [krige_soil()]
#'
#' @examples
#' data(soil_metaleurop)
#' v <- soil_variogram(soil_metaleurop, "cd")
#' v
#' @export
soil_variogram <- function(points, value, log10 = TRUE, covariates = NULL,
                           cutoff = NULL, width = NULL,
                           estimator = c("classical", "robust")) {
  estimator <- match.arg(estimator)
  d <- .prepare_soil_points(points, value, log10, covariates, verbose = FALSE)
  .empirical_variogram(d$xy, d$z, d$drift, cutoff, width, estimator)
}

#' @noRd
.empirical_variogram <- function(xy, z, drift, cutoff = NULL, width = NULL,
                                 estimator = "classical") {
  # Residuals of the trend (OLS); with only an intercept this is z - mean(z)
  res <- stats::lm.fit(drift, z)$residuals

  if (is.null(cutoff)) {
    rx <- diff(range(xy[, 1])); ry <- diff(range(xy[, 2]))
    cutoff <- sqrt(rx^2 + ry^2) / 3
  }
  if (is.null(width)) width <- cutoff / 15

  h <- stats::dist(xy)
  dz <- stats::dist(res)            # |r_i - r_j|
  h <- as.vector(h); dz <- as.vector(dz)
  sel <- h <= cutoff & h > 0
  h <- h[sel]; dz <- dz[sel]
  lag <- cut(h, breaks = seq(0, cutoff + width, by = width), include.lowest = TRUE)

  np <- as.vector(tapply(h, lag, length))
  dist <- as.vector(tapply(h, lag, mean))
  gamma <- if (estimator == "classical") {
    as.vector(tapply(dz^2, lag, mean)) / 2
  } else {
    m4 <- as.vector(tapply(sqrt(dz), lag, mean))^4
    m4 / (2 * (0.457 + 0.494 / np))
  }
  out <- data.frame(np = np, dist = dist, gamma = gamma)
  out <- out[!is.na(out$np) & out$np > 0, ]
  rownames(out) <- NULL
  attr(out, "cutoff") <- cutoff
  attr(out, "width") <- width
  attr(out, "drift") <- colnames(drift)
  class(out) <- c("soil_variogram", "data.frame")
  out
}

# -------------------------------
# Variogram model fitting
# -------------------------------

#' Fit a variogram model to an empirical soil variogram
#'
#' @description
#' Fits one or several theoretical variogram models to an empirical
#' semi-variogram (from [soil_variogram()]) by weighted least squares, using
#' the weights \eqn{N_j / h_j^2} (number of pairs over squared distance), which
#' give more importance to the short distances that matter most for kriging
#' (same default as `gstat::fit.variogram(fit.method = 7)`).
#'
#' Available models (the `range` parameter has the same meaning as in gstat):
#' \itemize{
#'   \item `"Exp"`: exponential, \eqn{\gamma(h) = c_0 + c (1 - e^{-h/a})};
#'   \item `"Sph"`: spherical, reaches the sill exactly at \eqn{h = a};
#'   \item `"Gau"`: Gaussian, very smooth at the origin;
#'   \item `"Mat"`: Matérn with smoothness `kappa` (`kappa = 0.5` is `"Exp"`).
#' }
#'
#' @param variogram A `soil_variogram` object.
#' @param model Character vector of model(s) to fit. When several are given,
#'   all are fitted and the one with the lowest weighted sum of squared errors
#'   is returned (the full comparison table is kept in `$comparison`).
#' @param kappa Numeric. Smoothness of the Matérn model (ignored otherwise).
#'
#' @return An object of class `soil_vgm`: a list with `model`, `nugget`,
#'   `psill`, `range`, `kappa`, `sse` (weighted sum of squared errors),
#'   `empirical` (the input variogram) and `comparison` (a table of all fitted
#'   models).
#'
#' @seealso [soil_variogram()], [krige_soil()]
#'
#' @examples
#' data(soil_metaleurop)
#' v <- soil_variogram(soil_metaleurop, "cd")
#' m <- fit_soil_variogram(v, model = c("Exp", "Sph", "Gau"))
#' m
#' m$comparison
#' @export
fit_soil_variogram <- function(variogram, model = "Exp", kappa = 0.5) {
  if (!inherits(variogram, "soil_variogram")) {
    stop("`variogram` must be the output of soil_variogram().")
  }
  model <- match.arg(model, .vgm_models, several.ok = TRUE)

  fits <- lapply(model, function(m) .fit_one_vgm(variogram, m, kappa))
  comparison <- do.call(rbind, lapply(fits, function(f) {
    data.frame(model = f$model, nugget = f$nugget, psill = f$psill,
               range = f$range, sse = f$sse)
  }))
  best <- fits[[which.min(comparison$sse)]]
  best$empirical <- variogram
  best$comparison <- comparison[order(comparison$sse), ]
  rownames(best$comparison) <- NULL
  class(best) <- "soil_vgm"
  best
}

#' @noRd
.fit_one_vgm <- function(v, model, kappa = 0.5) {
  h <- v$dist; g <- v$gamma
  w <- v$np / h^2
  w <- w / sum(w)
  cutoff <- attr(v, "cutoff")
  sill0 <- max(g)

  obj <- function(p) {
    sum(w * (g - .vgm_gamma(h, model, p[1], p[2], p[3], kappa))^2)
  }
  lower <- c(0, 1e-10 * sill0, cutoff / 1000)
  upper <- c(2 * sill0, 5 * sill0, 20 * cutoff)

  # Several starting values for the range, keep the best optimum
  best <- NULL
  for (r0 in cutoff * c(0.1, 0.3, 0.6, 1)) {
    start <- c(min(g) / 2, sill0 - min(g) / 2, r0)
    start <- pmin(pmax(start, lower), upper)
    o <- try(stats::optim(start, obj, method = "L-BFGS-B",
                          lower = lower, upper = upper), silent = TRUE)
    if (!inherits(o, "try-error") && (is.null(best) || o$value < best$value)) best <- o
  }
  if (is.null(best)) stop("Variogram fit failed for model ", model)

  list(model = model, nugget = best$par[1], psill = best$par[2],
       range = best$par[3], kappa = kappa, sse = best$value)
}

#' Build a variogram model by hand
#'
#' @description
#' Creates a `soil_vgm` object from known parameters, e.g. to re-use a
#' variogram fitted elsewhere (gstat, gstools, ...).
#'
#' @param model Character. One of `"Exp"`, `"Sph"`, `"Gau"`, `"Mat"`.
#' @param psill Numeric. Partial sill.
#' @param range Numeric. Range parameter (m).
#' @param nugget Numeric. Nugget effect (default 0).
#' @param kappa Numeric. Matérn smoothness (default 0.5).
#'
#' @return A `soil_vgm` object.
#' @examples
#' soil_vgm("Exp", psill = 0.1, range = 500, nugget = 0.02)
#' @export
soil_vgm <- function(model, psill, range, nugget = 0, kappa = 0.5) {
  model <- match.arg(model, .vgm_models)
  structure(list(model = model, nugget = nugget, psill = psill, range = range,
                 kappa = kappa, sse = NA_real_, empirical = NULL, comparison = NULL),
            class = "soil_vgm")
}

#' @export
print.soil_vgm <- function(x, ...) {
  cat("Variogram model:", x$model,
      if (x$model == "Mat") paste0("(kappa = ", x$kappa, ")") else "", "\n")
  cat(sprintf("  nugget = %.4g | partial sill = %.4g | range = %.4g m\n",
              x$nugget, x$psill, x$range))
  if (!is.na(x$sse)) cat(sprintf("  weighted SSE = %.4g\n", x$sse))
  if (!is.null(x$empirical)) {
    drift <- attr(x$empirical, "drift")
    cat("  fitted on", if (length(drift) > 1) {
      paste("residuals of the drift:", paste(drift[-1], collapse = " + "))
    } else "values (constant mean)", "\n")
  }
  invisible(x)
}

#' Plot an empirical variogram and its fitted model(s)
#'
#' @param x A `soil_vgm` object (output of [fit_soil_variogram()]).
#' @param all_models Logical. Draw every model of `x$comparison`? (default `TRUE`)
#' @param ... Not used.
#' @return A `ggplot` object.
#' @export
plot.soil_vgm <- function(x, all_models = TRUE, ...) {
  if (is.null(x$empirical)) stop("No empirical variogram attached to this model.")
  emp <- x$empirical
  hmax <- max(emp$dist) * 1.05
  hs <- seq(hmax / 500, hmax, length.out = 300)
  mods <- if (all_models && !is.null(x$comparison)) x$comparison else
    data.frame(model = x$model, nugget = x$nugget, psill = x$psill, range = x$range)
  curves <- do.call(rbind, lapply(seq_len(nrow(mods)), function(i) {
    data.frame(dist = hs, model = mods$model[i],
               gamma = .vgm_gamma(hs, mods$model[i], mods$nugget[i],
                                  mods$psill[i], mods$range[i], x$kappa))
  }))
  ggplot2::ggplot() +
    ggplot2::geom_point(data = as.data.frame(unclass(emp)),
                        ggplot2::aes(x = .data$dist, y = .data$gamma, size = .data$np),
                        colour = "grey30") +
    ggplot2::geom_line(data = curves,
                       ggplot2::aes(x = .data$dist, y = .data$gamma, colour = .data$model),
                       linewidth = 0.9) +
    ggplot2::expand_limits(y = 0, x = 0) +
    ggplot2::labs(x = "Distance h (m)", y = expression("Semi-variance " * gamma(h)),
                  size = "Number of pairs", colour = "Model") +
    ggplot2::theme_minimal()
}

# -------------------------------
# Kriging engine
# -------------------------------

#' Universal kriging (with external drift) at target locations
#'
#' Ordinary kriging is the special case where `drift` is a column of ones.
#' @noRd
.krige_engine <- function(xy, z, drift, vgm, target_xy, target_drift,
                          chunk_size = 10000) {
  n <- length(z)
  C <- .vgm_covariance(.cross_dist(xy, xy), vgm)
  # Tiny jitter on the diagonal for numerical stability (no nugget case)
  diag(C) <- diag(C) + 1e-10 * (vgm$psill + vgm$nugget)
  Rc <- chol(C)
  Ci_z <- backsolve(Rc, forwardsolve(t(Rc), z))
  Ci_F <- backsolve(Rc, forwardsolve(t(Rc), drift))
  FtCiF <- crossprod(drift, Ci_F)
  FtCiF_inv <- solve(FtCiF)
  beta <- FtCiF_inv %*% crossprod(drift, Ci_z)       # GLS estimate of the trend
  resid_w <- Ci_z - Ci_F %*% beta                    # C^-1 (z - F beta)
  sill <- vgm$psill + vgm$nugget

  m <- nrow(target_xy)
  pred <- var <- numeric(m)
  starts <- seq(1, m, by = chunk_size)
  for (s in starts) {
    idx <- s:min(s + chunk_size - 1, m)
    c0 <- .vgm_covariance(.cross_dist(xy, target_xy[idx, , drop = FALSE]), vgm)  # n x k
    f0 <- t(target_drift[idx, , drop = FALSE])                                  # p x k
    pred[idx] <- as.vector(crossprod(f0, beta) + crossprod(c0, resid_w))
    Ci_c0 <- backsolve(Rc, forwardsolve(t(Rc), c0))
    R <- f0 - crossprod(drift, Ci_c0)                                           # p x k
    var[idx] <- sill - colSums(c0 * Ci_c0) + colSums(R * (FtCiF_inv %*% R))
  }
  list(pred = pred, var = pmax(var, 0), beta = setNames(as.vector(beta), colnames(drift)))
}

# -------------------------------
# Main user function
# -------------------------------

#' Krige soil concentrations into a raster
#'
#' @description
#' Interpolates a soil concentration measured at sampling points onto a regular
#' raster grid by kriging. Two flavours are available:
#' \itemize{
#'   \item **Ordinary kriging (OK)** when `covariates = NULL`: the mean is
#'     unknown but constant over the area.
#'   \item **Kriging with external drift (KED)** when `covariates` is a
#'     `SpatRaster`: the mean varies linearly with the covariate layers (e.g.
#'     the log-distance to a contamination source, see
#'     [source_covariates()]) and the kriging interpolates the residuals.
#' }
#' By default the interpolation is done on `log10(value)`, which is the natural
#' scale for metal concentrations (log-normal distribution).
#'
#' @param points An `sf` object of POINT geometries in a projected CRS (metres).
#' @param value Character. Name of the concentration column (e.g. `"cd"`).
#' @param template Optional `SpatRaster` defining the output grid. If `NULL`,
#'   a grid is built from the points with [soil_grid()] using `res` and `margin`.
#'   If `covariates` is given and `template` is `NULL`, the covariate grid is used.
#' @param covariates Optional `SpatRaster` of drift covariates (must cover the
#'   points and the output grid, same CRS).
#' @param model Character vector of candidate variogram models
#'   (see [fit_soil_variogram()]); the best fitting one is used. Ignored when
#'   `vgm` is given.
#' @param vgm Optional `soil_vgm` object (from [fit_soil_variogram()] or
#'   [soil_vgm()]) to use instead of fitting one.
#' @param log10 Logical. Krige `log10(value)` (default `TRUE`).
#' @param res,margin Resolution and margin (m) of the grid built when
#'   `template` and `covariates` are `NULL`.
#' @param mask Optional `sf` polygon or `SpatVector`; cells outside are set to `NA`.
#' @param cutoff,width Passed to [soil_variogram()].
#' @param kappa Matérn smoothness, passed to [fit_soil_variogram()].
#' @param clamp Logical. Restrict the covariate values of the grid cells to
#'   the range observed at the sampling points (default `TRUE`). This prevents
#'   the linear drift from being extrapolated outside the conditions actually
#'   sampled (e.g. right next to a source where no sample was taken).
#' @param verbose Logical. Print messages?
#'
#' @return A `SpatRaster` with three layers:
#' \describe{
#'   \item{`<value>_log10`}{the kriging prediction on the log10 scale (named
#'     `<value>` if `log10 = FALSE`);}
#'   \item{`<value>_var`}{the kriging variance (same scale as the prediction);}
#'   \item{`<value>`}{(only if `log10 = TRUE`) the back-transformed
#'     concentration `10^prediction`, i.e. the median concentration.}
#' }
#' The fitted variogram is returned as the attribute `"vgm"` when available.
#'
#' @references
#' Pebesma, E. J. (2004). Multivariable geostatistics in S: the gstat package.
#' Computers & Geosciences, 30(7), 683-691.
#'
#' Hengl, T., Heuvelink, G. B. M., & Rossiter, D. G. (2007). About
#' regression-kriging: From equations to case studies. Computers & Geosciences,
#' 33(10), 1301-1315.
#'
#' @seealso [soil_variogram()], [fit_soil_variogram()], [cv_soil()],
#'   [source_covariates()], [kernel_smooth_soil()]
#'
#' @examples
#' \donttest{
#' data(soil_metaleurop)
#' # Ordinary kriging of log10(Cd) at 100 m
#' r <- krige_soil(soil_metaleurop, "cd", res = 100, margin = 500)
#' terra::plot(r)
#' }
#' @export
krige_soil <- function(points, value, template = NULL, covariates = NULL,
                       model = "Exp", vgm = NULL, log10 = TRUE,
                       res = 25, margin = 1000, mask = NULL,
                       cutoff = NULL, width = NULL, kappa = 0.5,
                       clamp = TRUE, verbose = TRUE) {
  d <- .prepare_soil_points(points, value, log10, covariates, verbose)

  if (is.null(template)) {
    template <- if (!is.null(covariates)) covariates[[1]] else soil_grid(points, res, margin)
  }
  if (!inherits(template, "SpatRaster")) stop("`template` must be a SpatRaster.")

  if (is.null(vgm)) {
    emp <- .empirical_variogram(d$xy, d$z, d$drift, cutoff, width)
    vgm <- fit_soil_variogram(emp, model = model, kappa = kappa)
    if (verbose) print(vgm)
  }
  if (!inherits(vgm, "soil_vgm")) stop("`vgm` must be a soil_vgm object.")

  # Target cells and their drift values
  target_xy <- terra::xyFromCell(template, seq_len(terra::ncell(template)))
  target_drift <- matrix(1, nrow = nrow(target_xy), ncol = 1)
  if (!is.null(covariates)) {
    cov_grid <- terra::resample(covariates, template, method = "bilinear")
    cov_val <- as.matrix(terra::values(cov_grid))
    if (clamp) {
      for (j in seq_len(ncol(cov_val))) {
        rg <- range(d$drift[, j + 1])
        out_rg <- !is.na(cov_val[, j]) & (cov_val[, j] < rg[1] | cov_val[, j] > rg[2])
        if (any(out_rg) && verbose) {
          message(sprintf("Covariate '%s' clamped to [%.4g, %.4g] in %d cell(s).",
                          colnames(cov_val)[j], rg[1], rg[2], sum(out_rg)))
        }
        cov_val[, j] <- pmin(pmax(cov_val[, j], rg[1]), rg[2])
      }
    }
    target_drift <- cbind(target_drift, cov_val)
  }
  ok <- stats::complete.cases(target_drift)

  k <- .krige_engine(d$xy, d$z, d$drift, vgm,
                     target_xy[ok, , drop = FALSE], target_drift[ok, , drop = FALSE])

  pred <- var <- rep(NA_real_, nrow(target_xy))
  pred[ok] <- k$pred
  var[ok] <- k$var

  out <- terra::rast(template, nlyrs = 2)
  terra::values(out) <- cbind(pred, var)
  names(out) <- c(if (log10) paste0(value, "_log10") else value, paste0(value, "_var"))
  if (log10) {
    conc <- 10^out[[1]]
    names(conc) <- value
    out <- c(out, conc)
  }
  if (!is.null(mask)) out <- terra::mask(out, terra::vect(mask))
  attr(out, "vgm") <- vgm
  out
}

# -------------------------------
# Cross-validation
# -------------------------------

#' Cross-validate a soil interpolation method
#'
#' @description
#' Estimates the prediction error of an interpolation method by k-fold
#' cross-validation: the points are split in `nfold` groups; each group is in
#' turn removed, the model is re-fitted (including the variogram) on the
#' remaining points and used to predict the removed ones.
#'
#' With `block_size`, folds are made of square spatial blocks instead of random
#' points (*spatial cross-validation*). This is more demanding: it mimics the
#' prediction of areas located far from any sample, where interpolators that
#' simply copy neighbours are penalised.
#'
#' @inheritParams krige_soil
#' @param method Character. `"kriging"` ([krige_soil()]) or `"kernel"`
#'   ([kernel_smooth_soil()]).
#' @param nfold Integer. Number of folds (default 10).
#' @param block_size Numeric. Side (m) of the spatial blocks; `NULL` (default)
#'   for random folds.
#' @param radius,size_std Kernel standard deviation (m) and window half-size
#'   (in number of `radius`), for `method = "kernel"` (see
#'   [kernel_smooth_soil()]). For speed, the cross-validation evaluates the
#'   Gaussian kernel exactly at the left-out points instead of on a raster.
#' @param seed Integer. Random seed for the fold assignment.
#'
#' @return A `data.frame` of class `soil_cv` with the observed value (`obs`),
#'   the prediction (`pred`) and the `fold` of each point (on the log10 scale if
#'   `log10 = TRUE`). Use [summary()] on it to get RMSE, MAE, bias and R2.
#'
#' @seealso [krige_soil()], [kernel_smooth_soil()]
#'
#' @examples
#' \donttest{
#' data(soil_metaleurop)
#' cv <- cv_soil(soil_metaleurop, "cd", model = "Exp")
#' summary(cv)
#' }
#' @export
cv_soil <- function(points, value, method = c("kriging", "kernel"),
                    covariates = NULL, model = "Exp", log10 = TRUE,
                    nfold = 10, block_size = NULL, seed = 1,
                    radius = 200, size_std = 3, kappa = 0.5,
                    cutoff = NULL, width = NULL) {
  method <- match.arg(method)
  d <- .prepare_soil_points(points, value, log10, covariates, verbose = FALSE)
  n <- length(d$z)

  set.seed(seed)
  if (is.null(block_size)) {
    fold <- sample(rep(seq_len(nfold), length.out = n))
  } else {
    blk <- paste(floor(d$xy[, 1] / block_size), floor(d$xy[, 2] / block_size))
    ub <- unique(blk)
    fold <- sample(rep(seq_len(nfold), length.out = length(ub)))[match(blk, ub)]
  }

  pred <- rep(NA_real_, n)
  for (k in seq_len(nfold)) {
    tr <- fold != k
    te <- !tr
    if (!any(te)) next
    if (method == "kriging") {
      emp <- .empirical_variogram(d$xy[tr, , drop = FALSE], d$z[tr],
                                  d$drift[tr, , drop = FALSE], cutoff, width)
      vgm <- fit_soil_variogram(emp, model = model, kappa = kappa)
      pred[te] <- .krige_engine(d$xy[tr, , drop = FALSE], d$z[tr],
                                d$drift[tr, , drop = FALSE], vgm,
                                d$xy[te, , drop = FALSE],
                                d$drift[te, , drop = FALSE])$pred
    } else {
      pred[te] <- .kernel_smooth_points(d$xy[tr, , drop = FALSE], d$z[tr],
                                        d$xy[te, , drop = FALSE], radius, size_std)
    }
  }

  out <- data.frame(x = d$xy[, 1], y = d$xy[, 2], obs = d$z, pred = pred, fold = fold)
  attr(out, "log10") <- log10
  class(out) <- c("soil_cv", "data.frame")
  out
}

#' Summarise a cross-validation
#'
#' @param object A `soil_cv` object from [cv_soil()].
#' @param ... Not used.
#' @return A one-row `data.frame` with `RMSE` (root mean squared error), `MAE`
#'   (mean absolute error), `bias` (mean error), `R2` (proportion of variance
#'   explained, \eqn{1 - SSE/SST}) and `n` (number of predicted points).
#' @export
summary.soil_cv <- function(object, ...) {
  ok <- !is.na(object$pred)
  e <- object$pred[ok] - object$obs[ok]
  o <- object$obs[ok]
  data.frame(RMSE = sqrt(mean(e^2)), MAE = mean(abs(e)), bias = mean(e),
             R2 = 1 - sum(e^2) / sum((o - mean(o))^2), n = sum(ok))
}
