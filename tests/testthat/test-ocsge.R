test_that("test ocsge collector", {

  testthat::skip_on_cran()
  testthat::skip_if_offline()

  public_url_fgb = "https://spacemod-ocsge.s3.fr-par.scw.cloud/ocsge_france.fgb"
  # small polygon to test
  roi_test <- sf::st_as_sfc("POLYGON((2.4 46.5, 2.5 46.5, 2.5 46.6, 2.4 46.6, 2.4 46.5))")
  sf::st_crs(roi_test) <- 4326 # WGS84
  try({
    test_data <- spacemodR::get_ocsge_data_fgb(roi = roi_test, fgb_url = public_url_fgb)
    print(test_data)
    plot(sf::st_geometry(test_data))
  })

})
