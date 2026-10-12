## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 3.5,
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(spacemodR)

## ----load_roi-----------------------------------------------------------------
data("roi_metaleurop")
## or read your own file
# roi_metaleurop <- sf::st_read("data/metaleurop_roi.geojson")

## ----load_departement---------------------------------------------------------
dpts <- tryCatch(
  get_departements_for_roi(roi_metaleurop),
  # do not stop the tutorial if the server cannot be reached
  error = function(e) {
    message("Could not fetch departments: ", conditionMessage(e))
    NULL
  }
)
dpts

## ----load_metaleurop_ocsge----------------------------------------------------
# sf_site <- get_ocsge(roi_metaleurop, source = "ign")
data(ocsge_metaleurop)

## ----vizualize_metaleurop_roi, fig.height=7-----------------------------------
library(ggplot2)
ggplot() +
  theme_minimal() +
  geom_sf(data = ocsge_metaleurop, aes(fill = code_cs)) +
  geom_sf(data = roi_metaleurop, fill = NA, color = "red", size = 1)

## ----set_habitat_ocsge--------------------------------------------------------
layer_CS2111 = ocsge_metaleurop[ocsge_metaleurop$code_cs == "CS2.1.1.1", ]
layer_CS221 = ocsge_metaleurop[ocsge_metaleurop$code_cs == "CS2.2.1", ]
layer_CS1121 = ocsge_metaleurop[ocsge_metaleurop$code_cs == "CS1.1.2.1", ]

## ----build_plot_habitat_10----------------------------------------------------
habitat_10 = habitat(layer_CS2111)
plot(habitat_10)

## ----build_plot_habitat_11----------------------------------------------------
habitat_11 = habitat(layer_CS2111, weight = 0.2)
head(habitat_11)

## ----build_plot_habitat_12----------------------------------------------------
habitat_12 = habitat(
  layer_CS2111,
  habitat = sample(c(TRUE, FALSE), nrow(layer_CS2111), replace = TRUE),
  weight = runif(nrow(layer_CS2111), 0, 10)
)
plot(habitat_12)

## ----build_plot_habitat_20----------------------------------------------------
habitat_20 = habitat() |>
  add_habitat(layer_CS2111)
class(habitat_20)

## ----build_plot_habitat_21----------------------------------------------------
habitat_21 = habitat() |>
  add_habitat(layer_CS2111, weight = 0.2)
head(habitat_21)

## ----build_habitat_22---------------------------------------------------------
habitat_22 = habitat() |>
  add_habitat(layer_CS2111, weight = 0.2) |>
  add_habitat(layer_CS1121, weight = 0.4) |>
  add_nohabitat(layer_CS221)

head(habitat_22)

## ----plot_habitat_22----------------------------------------------------------
plot(habitat_22)

## ----load_ground_cd-----------------------------------------------------------
ground_cd <- load_raster_extdata("ground_concentration_cd_compressed.tif")
terra::plot(ground_cd)

## ----OCSGE_codes--------------------------------------------------------------
OCSGE_codes <- list(
  forest = c("CS2.1.1.1", "CS2.1.1.2", "CS2.1.1.3", "CS2.1.3"),
  grass = "CS2.2.1",
  water = "CS1.2.2",
  soil = c("CS1.2.1","CS2.1.1.1","CS2.1.1.2","CS2.1.1.3",
           "CS2.1.2","CS2.1.3","CS2.2.1","CS2.2.3"),
  sealed = c("CS1.1.1.1", "CS1.1.1.2", "CS1.1.2.1")
)

## ----build_habitat_sol--------------------------------------------------------
layer_soil_natural = ocsge_metaleurop[ocsge_metaleurop$code_cs %in% OCSGE_codes$soil, ]
layer_soil_artificial = ocsge_metaleurop[ocsge_metaleurop$code_cs %in% OCSGE_codes$sealed, ]

habitat_sol = habitat() |>
  add_habitat(layer_soil_natural) |>
  add_nohabitat(layer_soil_artificial)
plot(habitat_sol)

## -----------------------------------------------------------------------------
rast_sol <- habitat_raster(ground_cd, habitat_sol)
terra::plot(rast_sol)

## ----build_raster_habitat-----------------------------------------------------
rast_10 <- habitat_raster(ground_cd, habitat_10)
terra::plot(rast_10)

rast_12 <- habitat_raster(ground_cd, habitat_12)
terra::plot(rast_12)

rast_22 <- habitat_raster(ground_cd, habitat_22)
terra::plot(rast_22)

## ----build_stack_raster_habitat-----------------------------------------------
stack_habitat <- raster_stack(
  raster_list = list(rast_sol, rast_10, rast_12, rast_22),
  names = c("sol", "sp10", "sp12", "sp22")
)

terra::plot(stack_habitat)

## -----------------------------------------------------------------------------
df_raw <- data.frame(
  src = c("sol", "sol", "sp10", "sp12"),
  target = c("sp10", "sp12", "sp12", "sp22"),
  w = c(2, 3, 2, 1))
trophic_from_df <- trophic(df_raw, from = "src", to = "target", weight = "w")

attr(trophic_from_df, "level")

## ----build_trophic_web_df-----------------------------------------------------
trophic_df <- trophic() |>
  add_link("sol", "sp10", 2) |>
  add_link("sol", "sp12", 3) |>
  add_link("sp10", "sp12", 2) |>
  add_link("sp12", "sp22")

attr(trophic_df, "level")

## ----plot_trophic_web_df_noshift----------------------------------------------
plot(trophic_df, shift = FALSE)

## ----plot_trophic_web_df_shift------------------------------------------------
plot(trophic_df, shift = TRUE)

## ----build_spacemodel_habitat-------------------------------------------------
spcmdl_habitat <- spacemodel(stack_habitat, trophic_df)

## -----------------------------------------------------------------------------
color_habitat <- colorRampPalette(c("white", "#169E19"))(255)
terra::plot(spcmdl_habitat, col = color_habitat)

## ----set_kernels--------------------------------------------------------------
k_sp10 <- compute_kernel(radius = 10, GSD = 25, size_std = 1.5)
k_sp12 <- compute_kernel(radius = 100, GSD = 25, size_std = 0.5)
k_sp22 <- compute_kernel(radius = 200, GSD = 25, size_std = 0.1)

## ----build_spacemodel_dispersal-----------------------------------------------
spcmdl_dispersal <- spcmdl_habitat |>
  dispersal("sp10", method = "convolution", method_option = list(kernel = k_sp10)) |>
  dispersal("sp12", method = "convolution", method_option = list(kernel = k_sp12)) |>
  dispersal("sp22", method = "convolution", method_option = list(kernel = k_sp22))

## ----plot_spacemodel_dispersal------------------------------------------------
color_dispersal <- colorRampPalette(c("white", "#166C9E"))(255)
terra::plot(spcmdl_dispersal, col = color_dispersal)

## ----exposure_set_ground_cd---------------------------------------------------
spcmdl_dispersal[["sol"]][] <- ground_cd
terra::plot(spcmdl_dispersal["sol"])

## ----kernel_exposure----------------------------------------------------------
kernels <- list(
  sol  = NA,
  sp10 = k_sp10,
  sp12 = k_sp12,
  sp22 = k_sp22
)

## -----------------------------------------------------------------------------
attr(spcmdl_dispersal, "trophic_tbl")

## ----exposure_spacemodel_build------------------------------------------------
test_fluxes <- flux(spcmdl_dispersal, default = 1) |>
  # from the soil to sp10: coefficient 0.4
  add_flux(from = "sol", to = "sp10", value = 0.4) |>
  # all links to sp12: coefficient 0.1
  add_flux(to = "sp12", value = 0.1) |>
  # except when sp12 feeds on sp10: a formula
  add_flux(from = "sp10", to = "sp12", value = ~ x^2/10) |>
  # all links to sp22: a formula
  add_flux(to = "sp22", value = ~ x^0.1)

spcmdl_transfer <- transfer(spcmdl_dispersal, kernels, test_fluxes)

## ----exposure_spacemodel_plot-------------------------------------------------
color_transfer <- colorRampPalette(c("white", "#A33D0A"))(255)
terra::plot(spcmdl_transfer, col = color_transfer)

## -----------------------------------------------------------------------------
spcmdl_10 <- 10^spcmdl_transfer
spcmdl_10 <- spacemodel(spcmdl_10, attr(spcmdl_transfer, "trophic_tbl"))

## -----------------------------------------------------------------------------
# thresholds of each layer (invented values)
thresholds <- c(sol = 1, sp10 = 1.5, sp12 = 1.5, sp22 = 1.5)
# same order as the layers of the spacemodel
ordered_thresholds <- thresholds[names(spcmdl_10)]
# risk index = concentration / threshold
spcmdl_risk <- spcmdl_10 / ordered_thresholds
spcmdl_risk <- spacemodel(spcmdl_risk, attr(spcmdl_transfer, "trophic_tbl"))

## ----risk---------------------------------------------------------------------
breaks_risk <- c(0, 0.1, 0.5, 1, 5, 10, Inf)
cols_risk <- c(
  "darkgreen",   # 0 - 0.1
  "green",       # 0.1 - 0.5
  "lightgreen",  # 0.5 - 1
  "yellow",      # 1 - 5
  "saddlebrown", # 5 - 10
  "#4A2C2A"      # > 10
)
terra::plot(spcmdl_risk,
            breaks = breaks_risk,
            col = cols_risk)

