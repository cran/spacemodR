test_that("test eunis collector", {

  testthat::skip_on_cran()
  testthat::skip_if_offline()

  roi_test <- sf::st_as_sf(data.frame(
    lon = c(2.30, 2.40, 2.40, 2.30, 2.30),
    lat = c(48.83, 48.83, 48.88, 48.88, 48.83)
  ), coords = c("lon", "lat"), crs = 4326)
  roi_test <- sf::st_sf(geometry = sf::st_cast(sf::st_combine(roi_test), "POLYGON"))

  try({
    test_data <- spacemodR::get_eunis_data(roi = roi_test, level = "L1")
    print(test_data)
    terra::plot(test_data)
  })

})

test_that("ref_ocsge_eunis is consistent with both nomenclatures", {

  xw <- spacemodR::ref_ocsge_eunis
  l2 <- spacemodR::ref_eunis$code[spacemodR::ref_eunis$level == "L2"]

  # every class of both nomenclatures is covered, with no unknown code
  expect_setequal(unique(xw$code_cs), spacemodR::ref_ocsge$code_cs)
  expect_setequal(unique(xw$code_eunis), l2)

  # one default EUNIS class per OCS-GE cover, ranks without gap
  group <- paste(xw$code_cs, xw$code_us)
  expect_true(all(tapply(xw$rank, group, function(r) identical(sort(r), seq_along(r)))))
  expect_equal(sum(is.na(xw$code_us) & xw$rank == 1), nrow(spacemodR::ref_ocsge))

  # one main OCS-GE cover per EUNIS class
  expect_setequal(xw$code_eunis[xw$main_cs], l2)
  expect_equal(sum(xw$main_cs), length(l2))
})
