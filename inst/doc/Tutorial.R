## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=10,
  fig.height=5
)

## ----setup--------------------------------------------------------------------
library(spacemodR)

## ----load_departement---------------------------------------------------------
data("roi_metaleurop")
## Or load a ROI via geojson
# roi_metaleurop <- sf::st_read("data/metaleurop_roi.geojson")
dpts <- get_departements_for_roi(roi_metaleurop)

## ----load_metaleurop_ocsge----------------------------------------------------
## This function is long to compute so we load the data set
# sf_site <- get_ocsge_data(roi_metaleurop)
data(ocsge_metaleurop)

## ----vizualize_metaleurop_roi, fig.height=10----------------------------------
library(ggplot2)
ggplot() +
  theme_minimal() +
  geom_sf(data=ocsge_metaleurop, aes(fill=code_cs)) +
    geom_sf(data=roi_metaleurop, fill=NA, color="red", size=1)

## ----set_habitat_ocsge--------------------------------------------------------
layer_CS2111 = ocsge_metaleurop[ocsge_metaleurop$code_cs == "CS2.1.1.1", ]
layer_CS221 = ocsge_metaleurop[ocsge_metaleurop$code_cs == "CS2.2.1", ]
layer_CS1121 = ocsge_metaleurop[ocsge_metaleurop$code_cs == "CS1.1.2.1", ]

## ----build_plot_habitat_10----------------------------------------------------
habitat_10 = habitat(layer_CS2111)
plot(habitat_10)

## ----build_plot_habitat_11----------------------------------------------------
habitat_11 = habitat(layer_CS2111, weight=0.2)
head(habitat_11)

## ----build_plot_habitat_12----------------------------------------------------
habitat_12 = habitat(
  layer_CS2111,
  habitat=sample(c(TRUE,FALSE), nrow(layer_CS2111), replace=TRUE),
  weight=runif(nrow(layer_CS2111), 0, 10)
)
plot(habitat_12)

## ----build_plot_habitat_20----------------------------------------------------
habitat_20 = habitat() |>
  add_habitat(layer_CS2111)
class(habitat_20)

## ----build_plot_habitat_21----------------------------------------------------
habitat_21 = habitat() |>
  add_habitat(layer_CS2111, weight=0.2)
head(habitat_21)

## ----build_habitat_22---------------------------------------------------------
habitat_22 = habitat() |>
  add_habitat(layer_CS2111, weight=0.2) |>
  add_habitat(layer_CS1121, weight=0.4) |>
  add_nohabitat(layer_CS221)

head(habitat_22)

## ----plot_habitat_22----------------------------------------------------------
plot(habitat_22)

## ----load_ground_cd-----------------------------------------------------------
# the raster tif file is internal to the spacemodR package
ground_cd <- load_raster_extdata("ground_concentration_cd_compressed.tif")
terra::plot(ground_cd)

## ----OCSGE_codes--------------------------------------------------------------
# prepare a list of OSGE code to make life easier
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

## ----build_raster_habitat-----------------------------------------------------
rast_sol <- habitat_raster(ground_cd, habitat_sol)
terra::plot(rast_sol)

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
plot(trophic_df, shift=FALSE)

## ----plot_trophic_web_df_shift------------------------------------------------
plot(trophic_df, shift=TRUE)

## ----build_spacemodel_habitat-------------------------------------------------
spcmdl_habitat <- spacemodel(stack_habitat, trophic_df)

## -----------------------------------------------------------------------------
color_habitat <- colorRampPalette(c("white", "#169E19"))(255)
terra::plot(spcmdl_habitat, col=color_habitat)

## ----set_kernels--------------------------------------------------------------
k_sp10 <- compute_kernel(radius=10, GSD=25, size_std=1.5)
k_sp12 <- compute_kernel(radius=100, GSD=25, size_std=1.5)
k_sp22 <- compute_kernel(radius=200, GSD=25, size_std=1.5)

## ----build_spacemodel_dispersal-----------------------------------------------
spcmdl_dispersal <- spcmdl_habitat |>
  dispersal("sp10",  method="convolution",  method_option=list(kernel=k_sp10)) |>
  dispersal("sp12",  method="convolution",  method_option=list(kernel=k_sp12)) |>
  dispersal("sp22",  method="convolution",  method_option=list(kernel=k_sp22))

## ----plot_spacemodel_dispersal------------------------------------------------
color_dispersal <- colorRampPalette(c("white", "#166C9E"))(255)
terra::plot(spcmdl_dispersal, col=color_dispersal)

## ----exposure_set_ground_cd---------------------------------------------------
spcmdl_dispersal[["sol"]][] <- ground_cd
terra::plot(spcmdl_dispersal[["sol"]])

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
test_intakes <- intake(spcmdl_dispersal,
  # General rule: sp12 uptake with factor 3
  "sp12" = 3,  
  # Exception: when sp12 feed on sp10, factor is 5
  "sp10 -> sp12" = 5,
  # General rule: sp22 has a complex function
  "sp22" = ~ x^2 + 10,
  # Exception: sol to sp10 is a coef
  "sol -> sp10" = 0.1,
  default = 1 # for all other default is 1
)
spcmdl_transfer <- transfer(spcmdl_dispersal, kernels, test_intakes)

## ----exposure_spacemodel_plot-------------------------------------------------
color_transfer <- colorRampPalette(c("white", "#A33D0A"))(255)
terra::plot(spcmdl_transfer, col=color_transfer)

## ----eco_ssl_cd_habitat-------------------------------------------------------
ground_cd <- load_raster_extdata("ground_concentration_cd_compressed.tif")
names_hab = c("soil", "plant", "invert", "mamHerb", "mamInsect", "birdInsect")
list_habitat <- lapply(names_hab, function(i) ground_cd)
stack_habitat <- raster_stack(list_habitat, names_hab)

terra::plot(stack_habitat)

## ----eco_ssl_cd_trophic-------------------------------------------------------
trophic_df <- trophic() |>
  add_link("soil", "plant") |>
  add_link("soil", "invert") |>
  add_link("soil", "mamHerb") |>
  add_link("soil", "mamInsect") |>
  add_link("soil", "birdInsect")

attr(trophic_df, "level")

## ----eco_ssl_cd_trophic_plot--------------------------------------------------
plot(trophic_df)

## ----eco_ssl_cd_spacemodel----------------------------------------------------
spcmdl_ecossl_h <- spacemodel(stack_habitat, trophic_df)

terra::plot(spcmdl_ecossl_h)

## ----eco_ssl_kernles_build----------------------------------------------------
# no dispersal for eco_ssl
ecossl_kernels <- list(
  soil  = NA, plant = NA, invert = NA,
  mamHerb = NA, mamInsect = NA, birdInsect = NA)

## ----eco_ssl_risk_build-------------------------------------------------------
ecossl_intakes <- intake(spcmdl_ecossl_h,
  "soil -> plant"       = ~ 10^x/32,  
  "soil -> invert"      = ~ 10^x/140,
  "soil -> mamHerb"     = ~ 10^x/73,
  "soil -> mamInsect"   = ~ 10^x/0.36,
  "soil -> birdInsect"  = ~ 10^x/0.77,
  default = 1, # for all other default is 1
  normalize = FALSE # TRUE would weight every link to sum at 1
)

spcmdl_ecossl_risk <- transfer(
  spcmdl_ecossl_h,
  ecossl_kernels,
  ecossl_intakes,
  exposure_weighting="potential")

## ----eco_ssl_risk_plot--------------------------------------------------------
# Risk colors
breaks_risk <- c(0, 0.1, 0.5, 1, 5, 10, Inf)
cols_risk <- c(
  "darkgreen",   # 0 - 0.1
  "green",       # 0.1 - 0.5
  "lightgreen",  # 0.5 - 1
  "yellow",      # 1 - 5
  "saddlebrown", # 5 - 10
  "#4A2C2A"      # > 10
)

names_keep <- names(spcmdl_ecossl_risk)[names(spcmdl_ecossl_risk) != "soil"]
spcmdl_ecossl_risk_sub <- spcmdl_ecossl_risk[[names_keep]]

terra::plot(spcmdl_ecossl_risk_sub,
            breaks = breaks_risk,
            col = cols_risk)

## ----eco_ssl_risk_plot_crop---------------------------------------------------
poly <- roi_metaleurop
poly_vect <- terra::project(terra::vect(poly), terra::crs(spcmdl_ecossl_risk_sub))

rast_crop <- terra::crop(spcmdl_ecossl_risk_sub, poly_vect)
rast_final <- terra::mask(rast_crop, poly_vect)
terra::plot(rast_final,
            breaks = breaks_risk,
            col = cols_risk)

## -----------------------------------------------------------------------------
check_ecossl_risk = list(
  "soil -> plant"       = 10^ground_cd/32,  
  "soil -> invert"      = 10^ground_cd/140,
  "soil -> mamHerb"     = 10^ground_cd/73,
  "soil -> mamInsect"   = 10^ground_cd/0.36,
  "soil -> birdInsect"  = 10^ground_cd/0.77
)

r_check_ecossl_risk = terra::rast(check_ecossl_risk)

terra::plot(r_check_ecossl_risk,
            breaks = breaks_risk,
            col = cols_risk)

