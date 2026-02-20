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
  expect_equal(ncol(hab), 3) # habitat, weight, geometry

  # --- types of columns
  expect_type(hab$habitat, "logical")
  expect_type(hab$weight, "double")
  expect_s3_class(sf::st_geometry(hab), "sfc")

  # --- exact values
  expect_equal(hab$habitat, c(TRUE, FALSE))
  expect_equal(hab$weight, c(0.8, 0.2))
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

  # bad type geometry
  expect_error(habitat(
    geometry = list(c(0, 0)),
    habitat = TRUE,
    weight = 1
  ), "`geometry` must be an object of class 'sf' or 'sfc'")
})
