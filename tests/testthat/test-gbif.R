test_that("test gbif collector", {

  testthat::skip_on_cran()
  testthat::skip_if_offline()

  roi_test <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = 2.9, ymin = 50.3, xmax = 3.2, ymax = 50.5), crs = 4326))

  occ <- tryCatch(
    spacemodR::get_gbif_data("Apodemus_sylvaticus", roi = roi_test, limit = 350),
    error = function(e) testthat::skip(conditionMessage(e)))

  expect_s3_class(occ, "sf")
  expect_true(nrow(occ) > 0 && nrow(occ) <= 350)
  expect_true(all(occ$species == "Apodemus sylvaticus"))
  expect_true(all(lengths(sf::st_intersects(occ, roi_test)) > 0))
  expect_true(attr(occ, "gbif_count") >= nrow(occ))
})

test_that("get_gbif_data checks its arguments", {
  expect_error(get_gbif_data(c("a", "b")), "single scientific name")
  expect_error(get_gbif_data("Apodemus sylvaticus", roi = "here"), "sf")
})
