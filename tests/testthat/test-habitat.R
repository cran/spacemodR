# tests/testthat/test-habitat.R
test_that("H::abitat object is created correctly", {
  # --- geometry de test
  geom <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1, 1)),
    crs = 4326
  )

  # --- création d'un objet Habitat
  hab <- habitat(
    geometry = geom,
    habitat = c(TRUE, FALSE),
    weight = c(0.8, 0.2)
  )

  # --- classes
  expect_s3_class(hab, "habitat")
  expect_s3_class(hab, "sf")
  expect_s3_class(hab, "data.frame")

  # --- dimensions
  expect_equal(nrow(hab), 2)
  expect_equal(ncol(hab), 5) # habitat, weight, priority, touches, geometry

  # --- types of columns
  expect_type(hab$habitat, "logical")
  expect_type(hab$weight, "double")
  expect_s3_class(sf::st_geometry(hab), "sfc")

  # --- exact values
  expect_equal(hab$habitat, c(TRUE, FALSE))
  expect_equal(hab$weight, c(0.8, 0.2))
  expect_equal(hab$priority, c(1, 1))
  expect_equal(hab$touches, c(TRUE, TRUE))
})

test_that("Habitat constructor fails with wrong input", {
  geom <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 4326
  )

  # size mismatch
  expect_error(habitat(
    geometry = geom,
    habitat = c(TRUE, FALSE),
    weight = 1
  ),"Length of habitat must match geometry")

  # bad type habitat
  expect_error(habitat(
    geometry = geom,
    habitat = 1,
    weight = 1
  ), "`habitat` must be logical")

  # bad type weight
  expect_error(habitat(
    geometry = geom,
    habitat = TRUE,
    weight = "high"
  ), "`weight` must be numeric")

  # bad priority
  expect_error(habitat(
    geometry = geom,
    priority = "top"
  ), "`priority` must be numeric")
  expect_error(habitat(
    geometry = geom,
    priority = c(1, 2)
  ), "Length of priority must match geometry")

  # bad touches
  expect_error(habitat(
    geometry = geom,
    touches = "yes"
  ), "`touches` must be logical")
  expect_error(habitat(
    geometry = geom,
    touches = c(TRUE, FALSE)
  ), "Length of touches must match geometry")

  # bad type geometry
  expect_error(habitat(
    geometry = list(c(0, 0)),
    habitat = TRUE,
    weight = 1
  ), "`geometry` must be an object of class 'sf' or 'sfc'")
})

# --- helpers for the raster tests
test_template <- function() {
  terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10,
              ymin = 0, ymax = 10, crs = "EPSG:2154")
}
test_square <- function(a, b) {
  sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(
    cbind(c(a, b, b, a, a), c(a, a, b, b, a)))), crs = 2154))
}
test_line <- function(x, y) {
  sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(cbind(x, y)), crs = 2154))
}
value_at <- function(r, x, y) terra::extract(r, cbind(x, y))[[1]]

test_that("add_habitat and add_nohabitat set the priority", {
  a <- test_square(1, 9)
  b <- test_square(4, 6)

  # default: each new layer on top of the previous ones
  hab <- habitat() |> add_habitat(a) |> add_nohabitat(b) |> add_habitat(a)
  expect_s3_class(hab, "habitat")
  expect_equal(hab$priority, c(1, 2, 3))
  expect_equal(hab$habitat, c(TRUE, FALSE, TRUE))

  # explicit priority, then default on top of the highest one
  hab <- habitat() |>
    add_habitat(a, priority = 10) |>
    add_nohabitat(b, priority = 2) |>
    add_habitat(a)
  expect_equal(hab$priority, c(10, 2, 11))

  # one layer of several features shares the same default priority
  hab <- habitat() |> add_habitat(a) |> add_habitat(rbind(a, b))
  expect_equal(hab$priority, c(1, 2, 2))

  # habitat objects built before the priority and touches columns existed
  old <- habitat() |> add_habitat(a) |> add_habitat(b)
  old$priority <- NULL
  old$touches <- NULL
  hab <- add_habitat(old, a)
  expect_equal(hab$priority, c(1, 2, 3))
  expect_equal(hab$touches, c(TRUE, TRUE, TRUE))
})

test_that("add_habitat and add_nohabitat set touches", {
  a <- test_square(1, 9)
  b <- test_square(4, 6)

  # default: every touched pixel, for habitat and non-habitat
  hab <- habitat() |> add_habitat(a) |> add_nohabitat(b)
  expect_equal(hab$touches, c(TRUE, TRUE))

  # per layer, or per feature
  hab <- habitat() |>
    add_habitat(a, touches = FALSE) |>
    add_nohabitat(b, touches = FALSE) |>
    add_habitat(rbind(a, b), touches = c(TRUE, FALSE))
  expect_equal(hab$touches, c(FALSE, FALSE, TRUE, FALSE))
})

test_that("habitat_raster rasterizes weights and masks non-habitat", {
  template <- test_template()
  hab <- habitat() |>
    add_habitat(test_square(1, 9), weight = 0.8) |>
    add_nohabitat(test_square(4, 6))

  r <- habitat_raster(template, hab)
  expect_s4_class(r, "SpatRaster")
  expect_equal(names(r), "weight")
  expect_equal(value_at(r, 2.5, 2.5), 0.8)
  expect_equal(value_at(r, 0.5, 0.5), 0)
  expect_true(is.na(value_at(r, 4.5, 4.5)))

  # empty habitat: background everywhere
  r0 <- habitat_raster(template, habitat())
  expect_true(all(terra::values(r0) == 0))

  expect_error(habitat_raster(template, test_square(1, 9)),
               "must be a 'habitat' object")
})

test_that("habitat_raster stacks features by priority", {
  template <- test_template()
  big <- test_square(1, 9)
  small <- test_square(4, 6)

  # default: last added on top
  r <- habitat_raster(template, habitat() |>
                        add_habitat(small, weight = 5) |>
                        add_habitat(big, weight = 2))
  expect_equal(value_at(r, 4.5, 4.5), 2)

  # priority overrides the order of the calls
  r <- habitat_raster(template, habitat() |>
                        add_habitat(small, weight = 5, priority = 2) |>
                        add_habitat(big, weight = 2, priority = 1))
  expect_equal(value_at(r, 4.5, 4.5), 5)
  expect_equal(value_at(r, 2.5, 2.5), 2)

  # same result whatever the order of the calls
  r2 <- habitat_raster(template, habitat() |>
                         add_habitat(big, weight = 2, priority = 1) |>
                         add_habitat(small, weight = 5, priority = 2))
  expect_equal(terra::values(r2), terra::values(r))

  # non-habitat only excludes what lies below it
  r <- habitat_raster(template, habitat() |>
                        add_nohabitat(small, priority = 1) |>
                        add_habitat(big, weight = 2, priority = 2))
  expect_false(anyNA(terra::values(r)))
  r <- habitat_raster(template, habitat() |>
                        add_nohabitat(small, priority = 3) |>
                        add_habitat(big, weight = 2, priority = 2))
  expect_true(is.na(value_at(r, 4.5, 4.5)))

  # habitat objects built before the priority column existed: feature order
  old <- habitat() |>
    add_habitat(small, weight = 5) |>
    add_habitat(big, weight = 2)
  old$priority <- NULL
  expect_equal(value_at(habitat_raster(template, old), 4.5, 4.5), 2)
})

test_that("habitat_raster keeps linear features continuous", {
  template <- test_template()
  hedge <- test_line(c(0.2, 9.9), c(0.1, 9.6))

  r <- habitat_raster(template, habitat() |> add_habitat(hedge, weight = 1))
  # every crossed pixel is filled: more than the 10 pixels of a thin diagonal
  expect_gt(sum(terra::values(r) == 1), 10)
  # and they form a single patch for 4-neighbour movements
  p <- terra::patches(terra::subst(r, 0, NA), directions = 4)
  expect_equal(nrow(terra::unique(p)), 1)

  # a line barrier cannot be crossed diagonally
  r <- habitat_raster(template, habitat() |> add_nohabitat(hedge))
  p <- terra::patches(r, directions = 8)
  expect_equal(nrow(terra::unique(p)), 2)
})

test_that("habitat_raster mixes lines and polygons", {
  template <- test_template()
  field <- test_square(0, 10)
  building <- test_square(4, 6)
  hedge <- test_line(c(0, 10), c(4.5, 4.5))

  # hedge on top: continuous across the field and the building
  hab <- habitat() |>
    add_habitat(hedge, weight = 1, priority = 10) |>
    add_habitat(field, weight = 0.2, priority = 1) |>
    add_nohabitat(building, priority = 2)
  r <- habitat_raster(template, hab)
  expect_equal(unlist(r[6, ], use.names = FALSE), rep(1, 10))
  expect_equal(value_at(r, 2.5, 8.5), 0.2)
  expect_true(is.na(value_at(r, 4.5, 5.5)))

  # building on top: the hedge is cut
  hab <- habitat() |>
    add_habitat(hedge, weight = 1, priority = 2) |>
    add_habitat(field, weight = 0.2, priority = 1) |>
    add_nohabitat(building, priority = 10)
  r <- habitat_raster(template, hab)
  expect_true(is.na(value_at(r, 4.5, 4.5)))
  expect_equal(value_at(r, 2.5, 4.5), 1)
})

test_that("touches sets the pixels filled by a polygon", {
  template <- test_template()
  # a strip thinner than a pixel, covering no pixel centre
  strip <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(
    cbind(c(0, 10, 10, 0, 0), c(4.6, 4.6, 4.9, 4.9, 4.6)))), crs = 2154))

  # touches = TRUE (default): the strip is a continuous row of pixels
  r <- habitat_raster(template, habitat() |> add_habitat(strip, weight = 3))
  expect_equal(sum(terra::values(r) == 3), 10)
  r <- habitat_raster(template, habitat() |> add_nohabitat(strip))
  expect_equal(sum(is.na(terra::values(r))), 10)

  # touches = FALSE: it vanishes between the pixel centres
  r <- habitat_raster(template, habitat() |>
                        add_habitat(strip, weight = 3, touches = FALSE))
  expect_true(all(terra::values(r) == 0))
  r <- habitat_raster(template, habitat() |>
                        add_nohabitat(strip, touches = FALSE))
  expect_false(anyNA(terra::values(r)))

  # a polygon spreads on the pixels it only partly covers with touches = TRUE
  poly <- test_square(1.6, 8.4)
  r_all <- habitat_raster(template, habitat() |> add_habitat(poly))
  r_centre <- habitat_raster(template, habitat() |>
                               add_habitat(poly, touches = FALSE))
  expect_equal(sum(terra::values(r_all) == 1), 64)
  expect_equal(sum(terra::values(r_centre) == 1), 36)
})

test_that("touches sets the pixels filled by a line", {
  template <- test_template()
  hedge <- test_line(c(0.2, 9.9), c(0.1, 9.6))

  r_all <- habitat_raster(template, habitat() |> add_habitat(hedge))
  r_thin <- habitat_raster(template, habitat() |>
                             add_habitat(hedge, touches = FALSE))
  expect_gt(sum(terra::values(r_all) == 1), sum(terra::values(r_thin) == 1))

  # the thin line is only connected by pixel corners
  n_patches <- function(r, directions) {
    nrow(terra::unique(terra::patches(terra::subst(r, 0, NA),
                                      directions = directions)))
  }
  expect_equal(n_patches(r_all, 4), 1)
  expect_gt(n_patches(r_thin, 4), 1)
  expect_equal(n_patches(r_thin, 8), 1)
})

test_that("touches is applied layer by layer", {
  template <- test_template()
  strip <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(
    cbind(c(0, 10, 10, 0, 0), c(4.6, 4.6, 4.9, 4.9, 4.6)))), crs = 2154))
  cover <- test_square(1.6, 8.4)

  # land cover by pixel centre, thin structure on top on every touched pixel
  hab <- habitat() |>
    add_habitat(cover, weight = 2, priority = 1, touches = FALSE) |>
    add_habitat(strip, weight = 9, priority = 2)
  r <- habitat_raster(template, hab)
  expect_equal(sum(terra::values(r) == 9), 10)
  expect_equal(sum(terra::values(r) == 2), 36 - 6)
})
