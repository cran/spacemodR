## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 3.5,
  out.width = "100%",
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(spacemodR)
library(ggplot2)
library(terra)

## ----load_roi-----------------------------------------------------------------
# Load a study area (e.g., the Metaleurop site) and its land cover data
data("roi_metaleurop")
data("ocsge_metaleurop")

ggplot() +
  theme_minimal() +
  geom_sf(data=ocsge_metaleurop, aes(fill=code_cs), color=NA) +
  geom_sf(data=roi_metaleurop, fill=NA, color="red", size=1) +
  theme(legend.position = "none") +
  labs(title="Land Cover in the Region of Interest (ROI)")

## ----build_habitat------------------------------------------------------------
# Example: Loading a background concentration raster for a contaminant (Cadmium)
ground_cd <- load_raster_extdata("ground_concentration_cd_compressed.tif")

# Habitat definition based on OSGE codes (which are in the `ocsge_metaleurop` table)
layer_soil_natural = ocsge_metaleurop$code_cs %in% 
  c("CS1.2.1","CS2.1.1.1","CS2.1.1.2","CS2.1.1.3", "CS2.1.2","CS2.1.3","CS2.2.1","CS2.2.3")
layer_soil_artificial = ocsge_metaleurop$code_cs %in%
  c("CS1.1.1.1", "CS1.1.1.2", "CS1.1.2.1")
layer_plant = ocsge_metaleurop$code_cs %in% 
   c("CS2.1.1.1", "CS2.1.1.2", "CS2.1.1.3", "CS2.1.3", "CS2.2.1")

## ----map_sol, fig.height=4.2--------------------------------------------------
habitat_sol = habitat() |>
  add_habitat(ocsge_metaleurop[layer_soil_natural,]) |>
  add_nohabitat(ocsge_metaleurop[layer_soil_artificial,])
plot(habitat_sol)

## -----------------------------------------------------------------------------
habitat_plante = habitat() |>
  add_habitat(ocsge_metaleurop[layer_plant,]) |>
  add_nohabitat(ocsge_metaleurop[layer_soil_artificial,])

## ----map_hab_microtus---------------------------------------------------------
microtus_hab <- join_ocsge_species(ocsge_metaleurop, "Microtus_arvalis")
plot_species_habitat(microtus_hab)

## ----map_hab_crocidura--------------------------------------------------------
crocidura_hab <- join_ocsge_species(ocsge_metaleurop, "Crocidura")
plot_species_habitat(crocidura_hab)

## ----def_hab_mammal-----------------------------------------------------------
# We use `suitability` as habitat weight, and take out the covers the species cannot cross
habitat_herbivore <- habitat(microtus_hab, habitat=TRUE, weight=microtus_hab$suitability) |>
  add_nohabitat(microtus_hab[microtus_hab$permeability==0,])
habitat_carnivore <- habitat(crocidura_hab, habitat=TRUE, weight=crocidura_hab$suitability) |>
  add_nohabitat(crocidura_hab[crocidura_hab$permeability==0,])

## -----------------------------------------------------------------------------
# We initialize habitat grids (rasters) for each level
rast_sol <- habitat_raster(ground_cd, habitat_sol)
rast_plant <- habitat_raster(ground_cd, habitat_plante)
rast_herbivor <- habitat_raster(ground_cd, habitat_herbivore)
rast_carnivor <- habitat_raster(ground_cd, habitat_carnivore)

# We create the `raster_stack` of the habitats
stack_habitat <- raster_stack(
  raster_list = list(rast_sol, rast_plant, rast_herbivor, rast_carnivor),
  names = c("soil", "plant", "herbivore", "carnivore")
)

terra::plot(stack_habitat)

## ----trophic_web--------------------------------------------------------------
# Build the trophic web
trophic_df <- trophic() |>
  add_link(from = "soil", to = "plant") |>
  add_link(from = "plant", to = "herbivore") |>
  add_link(from = "herbivore", to = "carnivore")

# Visualize the trophic links
plot(trophic_df)

## ----build_spacemodel---------------------------------------------------------
spcmdl <- spacemodel(stack_habitat, trophic_df)
print(spcmdl)

## ----dispersal----------------------------------------------------------------
# Compute dispersal kernels based on a mobility radius (e.g., in pixels)
k_herb <- compute_kernel(radius=50, GSD=25, size_std=0.5)
k_carn <- compute_kernel(radius=150, GSD=25, size_std=0.5)

# Apply dispersal to the spacemodel
spcmdl_dispersal <- spcmdl |>
  dispersal("herbivore", method="convolution", method_option=list(kernel=k_herb)) |>
  dispersal("carnivore", method="convolution", method_option=list(kernel=k_carn))

# Note: In this example, the soil and plants are static and do not disperse.

## ----transfer-----------------------------------------------------------------
# 1. Assign the actual concentration grid into the "soil" layer
spcmdl_dispersal[["soil"]][] <- ground_cd

# 2. Define the transfer equations (Bioaccumulation / Bioconcentration Factors)
fluxes <- flux(spcmdl_dispersal, default = 1) |>
  add_flux("soil", "plant",  ~ 10^x / 32) |> # Specific equation for plants
  add_flux("soil", "herbivore",  0.5) |>     # Simple transfer factor for the herbivore
  add_flux("herbivore", "carnivore",  0.8)   # Simple transfer factor for the carnivore

# 3. Gather our dispersal kernels for the exposure calculation
kernels <- list(soil = NA, plant = NA, herbivore = k_herb, carnivore = k_carn)

# 4. Compute the global transfer throughout the ecosystem
spcmdl_transfer <- transfer(spcmdl_dispersal, kernels, fluxes)

# Visualize the contamination absorbed by the carnivore
color_transfer <- colorRampPalette(c("white", "#A33D0A"))(255)
terra::plot(spcmdl_transfer, col=color_transfer)

## ----eco_ssl_risk-------------------------------------------------------------
# Define risk classes (from "Safe" to "Very High Risk")
breaks_risk <- c(-Inf, 0.1, 0.5, 1, 5, 10, Inf)
cols_risk <- c(
  "darkgreen",   # 0 - 0.1 (No Risk)
  "green",       # 0.1 - 0.5
  "lightgreen",  # 0.5 - 1 (Threshold limit)
  "yellow",      # 1 - 5 (Moderate Risk)
  "saddlebrown", # 5 - 10 (High Risk)
  "#4A2C2A"      # > 10 (Severe Risk)
)

# Project the risk onto the Region of Interest
poly_vect <- terra::project(terra::vect(roi_metaleurop), terra::crs(spcmdl_transfer))
rast_crop <- terra::crop(spcmdl_transfer[["herbivore"]], poly_vect)
rast_final <- terra::mask(rast_crop, poly_vect)

terra::plot(rast_final,
            breaks = breaks_risk,
            col = cols_risk,
            main = "Risk Index Map (Herbivore)")

