# Small synthetic data set: smooth field + noise on a 2 km square
make_points <- function(n = 80, seed = 1) {
  set.seed(seed)
  x <- runif(n, 0, 2000)
  y <- runif(n, 0, 2000)
  conc <- 10^(1 + 0.5 * sin(x / 500) + 0.3 * cos(y / 400) + rnorm(n, 0, 0.05))
  sf::st_as_sf(data.frame(x = x + 7e5, y = y + 7e6, conc = conc),
               coords = c("x", "y"), crs = 2154)
}

test_that("soil_variogram returns a sensible empirical variogram", {
  pts <- make_points()
  v <- soil_variogram(pts, "conc")
  expect_s3_class(v, "soil_variogram")
  expect_true(all(c("np", "dist", "gamma") %in% names(v)))
  expect_true(all(v$gamma >= 0))
  expect_true(all(v$dist <= attr(v, "cutoff")))
  # spatial structure: short distances are more alike than long ones
  expect_lt(v$gamma[1], max(v$gamma))
})

test_that("fit_soil_variogram fits and ranks several models", {
  v <- soil_variogram(make_points(), "conc")
  m <- fit_soil_variogram(v, model = c("Exp", "Sph", "Gau"))
  expect_s3_class(m, "soil_vgm")
  expect_equal(nrow(m$comparison), 3)
  expect_equal(m$model, m$comparison$model[1])
  expect_true(m$nugget >= 0 && m$psill > 0 && m$range > 0)
  expect_s3_class(plot(m), "ggplot")
})

test_that("Matern with kappa = 0.5 equals the exponential model", {
  h <- c(0, 10, 100, 1000)
  expect_equal(.vgm_correlation(h, "Mat", 300, kappa = 0.5),
               .vgm_correlation(h, "Exp", 300))
})

test_that("krige_soil without nugget honours the data", {
  pts <- make_points(n = 30)
  # template whose cell centres are exactly the sampling points (1 row)
  xy <- sf::st_coordinates(pts)
  vgm <- soil_vgm("Exp", psill = 0.1, range = 500, nugget = 0)
  d <- .prepare_soil_points(pts, "conc", log10 = TRUE, verbose = FALSE)
  k <- .krige_engine(d$xy, d$z, d$drift, vgm, xy, matrix(1, nrow(xy), 1))
  expect_equal(k$pred, log10(pts$conc), tolerance = 1e-6)
  expect_true(all(k$var < 1e-6))
})

test_that("krige_soil returns prediction, variance and concentration layers", {
  pts <- make_points()
  r <- krige_soil(pts, "conc", res = 100, margin = 200, verbose = FALSE)
  expect_s4_class(r, "SpatRaster")
  expect_equal(names(r), c("conc_log10", "conc_var", "conc"))
  v <- terra::values(r)
  expect_equal(v[, "conc"], 10^v[, "conc_log10"])
  expect_true(all(v[, "conc_var"] >= 0))
  r2 <- krige_soil(pts, "conc", res = 100, margin = 0, log10 = FALSE, verbose = FALSE)
  expect_equal(names(r2), c("conc", "conc_var"))
})

test_that("kriging with external drift reproduces a pure linear trend", {
  set.seed(2)
  grid <- terra::rast(terra::ext(7e5, 7e5 + 2000, 7e6, 7e6 + 2000),
                      resolution = 100, crs = "EPSG:2154")
  cov <- terra::init(grid, "x")
  names(cov) <- "east"
  cov <- (cov - 7e5) / 1000
  # points at cell centres, so that the covariate value is exact
  xy <- terra::xyFromCell(grid, sample(terra::ncell(grid), 60))
  pts <- sf::st_as_sf(data.frame(x = xy[, 1], y = xy[, 2]),
                      coords = c("x", "y"), crs = 2154)
  pts$conc <- 3 + 2 * (xy[, 1] - 7e5) / 1000
  vgm <- soil_vgm("Exp", psill = 0.01, range = 300, nugget = 0.001)
  r <- krige_soil(pts, "conc", template = grid, covariates = cov, vgm = vgm,
                  log10 = FALSE, verbose = FALSE)
  expected <- 3 + 2 * terra::values(cov)[, 1]
  expect_equal(terra::values(r[["conc"]])[, 1], expected, tolerance = 1e-6)
})

test_that("covariates are clamped to the sampled range", {
  pts <- make_points()
  grid <- soil_grid(pts, res = 100, margin = 1000)
  src <- sf::st_sfc(sf::st_point(c(7e5 + 1000, 7e6 + 1000)), crs = 2154)
  cov <- source_covariates(grid, src)
  utils::capture.output(
    expect_message(krige_soil(pts, "conc", covariates = cov, verbose = TRUE),
                   "clamped")
  )
})

test_that("duplicated locations are averaged", {
  pts <- make_points(n = 20)
  dup <- rbind(pts, pts[1, ])
  dup$conc[21] <- dup$conc[1] * 10
  d <- .prepare_soil_points(dup, "conc", verbose = FALSE)
  expect_equal(length(d$z), 20)
  expect_equal(d$z[1], mean(log10(c(pts$conc[1], pts$conc[1] * 10))))
})

test_that("input checks", {
  pts <- make_points()
  expect_error(krige_soil(as.data.frame(pts), "conc"), "sf")
  expect_error(krige_soil(pts, "nope"), "No column")
  bad <- pts
  bad$conc[1] <- 0
  expect_error(soil_variogram(bad, "conc"), "positive")
  expect_error(soil_variogram(sf::st_transform(pts, 4326), "conc"), "projected")
})

test_that("cv_soil gives predictions and metrics for kriging and kernel", {
  pts <- make_points()
  cv <- cv_soil(pts, "conc", model = "Exp", nfold = 5)
  expect_s3_class(cv, "soil_cv")
  expect_equal(nrow(cv), nrow(pts))
  s <- summary(cv)
  expect_true(all(c("RMSE", "MAE", "bias", "R2", "n") %in% names(s)))
  expect_lt(s$RMSE, sd(log10(pts$conc)))

  cvk <- cv_soil(pts, "conc", method = "kernel", radius = 200, nfold = 5)
  expect_equal(nrow(cvk), nrow(pts))
  cvb <- cv_soil(pts, "conc", block_size = 500, nfold = 4)
  expect_equal(length(unique(cvb$fold)), 4)
})

test_that("kernel_smooth_soil reuses compute_kernel and preserves constants", {
  pts <- make_points()
  pts$conc <- 5
  r <- kernel_smooth_soil(pts, "conc", radius = 150, res = 50, margin = 0)
  expect_equal(names(r), c("conc_log10", "conc"))
  v <- terra::values(r[["conc"]])[, 1]
  expect_equal(unique(round(v[!is.na(v)], 8)), 5)
  # the point version gives the same estimator
  xy <- sf::st_coordinates(pts)
  expect_equal(.kernel_smooth_points(xy, rep(1, nrow(xy)), xy[1:3, ], 150), rep(1, 3))
})

test_that("source_covariates computes distance and wind sectors", {
  grid <- terra::rast(terra::ext(0, 1000, 0, 1000), resolution = 100, crs = "EPSG:2154")
  src <- sf::st_sfc(sf::st_point(c(500, 500)), crs = 2154)
  rose <- data.frame(direction = c(0, 90, 180, 270), freq = c(1, 2, 3, 4))
  cov <- source_covariates(grid, src, wind_rose = rose)
  expect_equal(names(cov), c("log10_dist", "wind"))
  # cell centred at (950, 550): east of the source
  cell <- terra::cellFromXY(grid, cbind(950, 550))
  expect_equal(cov[["wind"]][cell][1, 1], 2)
  expect_equal(cov[["log10_dist"]][cell][1, 1], log10(sqrt(450^2 + 50^2)))
  # cell north of the source
  cell_n <- terra::cellFromXY(grid, cbind(550, 950))
  expect_equal(cov[["wind"]][cell_n][1, 1], 1)
})

test_that("soil_grid is aligned on the resolution", {
  g <- soil_grid(make_points(), res = 25, margin = 100)
  e <- unname(as.vector(terra::ext(g)))
  expect_equal(e %% 25, rep(0, 4))
  expect_equal(terra::res(g), c(25, 25))
})

test_that("kriging matches gstat", {
  skip_if_not_installed("gstat")
  pts <- make_points(n = 60)
  pts$cl <- log10(pts$conc)
  grid <- soil_grid(pts, res = 200, margin = 0)
  cells <- sf::st_as_sf(as.data.frame(terra::xyFromCell(grid, seq_len(terra::ncell(grid)))),
                        coords = c("x", "y"), crs = 2154)
  ours <- krige_soil(pts, "conc", template = grid,
                     vgm = soil_vgm("Sph", psill = 0.1, range = 800, nugget = 0.01),
                     verbose = FALSE)
  ref <- gstat::krige(cl ~ 1, pts, cells, debug.level = 0,
                      model = gstat::vgm(0.1, "Sph", 800, 0.01))
  expect_equal(terra::values(ours[["conc_log10"]])[, 1], ref$var1.pred, tolerance = 1e-6)
  expect_equal(terra::values(ours[["conc_var"]])[, 1], ref$var1.var, tolerance = 1e-6)
})
