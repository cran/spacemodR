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
# Charger une zone d'étude (ex: site de Metaleurop) et les données d'occupation du sol
data("roi_metaleurop")
data("ocsge_metaleurop")

ggplot() +
  theme_minimal() +
  geom_sf(data=ocsge_metaleurop, aes(fill=code_cs), color=NA) +
  geom_sf(data=roi_metaleurop, fill=NA, color="red", size=1) +
  theme(legend.position = "none") +
  labs(title="Occupation du sol sur la zone d'étude")

## ----build_habitat------------------------------------------------------------
# Exemple: Chargement d'un raster de la concentration de fond d'un contaminant (Cadmium)
ground_cd <- load_raster_extdata("ground_concentration_cd_compressed.tif")

# Définition d'habitat à partir des code OSGE (qui sont dans le tableau `ocsge_metaleurop`)
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
# nous utilisons `suitability` comme poids d'habitat, et retirons les couvertures infranchissables pour l'espèce
habitat_herbivore <- habitat(microtus_hab, habitat=TRUE, weight=microtus_hab$suitability) |>
  add_nohabitat(microtus_hab[microtus_hab$permeability==0,])
habitat_carnivore <- habitat(crocidura_hab, habitat=TRUE, weight=crocidura_hab$suitability) |>
  add_nohabitat(crocidura_hab[crocidura_hab$permeability==0,])

## -----------------------------------------------------------------------------
# On initialise des grilles d'habitats (raster) pour chaque maillon
rast_sol <- habitat_raster(ground_cd, habitat_sol)
rast_plant <- habitat_raster(ground_cd, habitat_plante)
rast_herbivor <- habitat_raster(ground_cd, habitat_herbivore)
rast_carnivor <- habitat_raster(ground_cd, habitat_carnivore)

# On créé le `raster_stack` des habiats
stack_habitat <- raster_stack(
  raster_list = list(rast_sol, rast_plant, rast_herbivor, rast_carnivor),
  names = c("sol", "plante", "herbivore", "carnivore")
)

terra::plot(stack_habitat)

## ----trophic_web--------------------------------------------------------------
# Construction du réseau trophique
trophic_df <- trophic() |>
  add_link(from = "sol", to = "plante") |>
  add_link(from = "plante", to = "herbivore") |>
  add_link(from = "herbivore", to = "carnivore")

# Visualisation des liens trophiques
plot(trophic_df)

## ----build_spacemodel---------------------------------------------------------
spcmdl <- spacemodel(stack_habitat, trophic_df)
print(spcmdl)

## ----dispersal----------------------------------------------------------------
# Calcul des noyaux de dispersion en fonction d'un rayon de mobilité (ex: en pixels)
k_herb <- compute_kernel(radius=50, GSD=25, size_std=0.5)
k_carn <- compute_kernel(radius=150, GSD=25, size_std=0.5)

# Application de la dispersion sur le spacemodel
spcmdl_dispersal <- spcmdl |>
  dispersal("herbivore", method="convolution", method_option=list(kernel=k_herb)) |>
  dispersal("carnivore", method="convolution", method_option=list(kernel=k_carn))

# Note : le sol et les plantes sont évidemment statiques et ne dispersent pas.

## ----transfer-----------------------------------------------------------------
# 1. On assigne la grille de concentration réelle dans la couche "soil"
spcmdl_dispersal[["sol"]][] <- ground_cd

# 2. On définit les équations de transfert (Facteurs de Bioaccumulation / Bioconcentration)
fluxes <- flux(spcmdl_dispersal, default = 1) |>
  add_flux("soil", "plant",  ~ 10^x / 32) |> # Equation spécifique pour les plantes
  add_flux("soil", "herbivore",  0.5) |>     # Facteur simple pour l'herbivore
  add_flux("herbivore", "carnivore",  0.8)   # Facteur simple pour le carnivore

# 3. On rassemble nos noyaux de dispersion pour le calcul de l'exposition
kernels <- list(sol = NA, plante = NA, herbivore = k_herb, carnivore = k_carn)

# 4. Calcul du transfert global dans l'écosystème
spcmdl_transfer <- transfer(spcmdl_dispersal, kernels, fluxes)

# Visualisation de la contamination absorbée par le carnivore
color_transfer <- colorRampPalette(c("white", "#A33D0A"))(255)
terra::plot(spcmdl_transfer, col=color_transfer)

## ----eco_ssl_risk-------------------------------------------------------------
# Définition des classes de risque (de "Sûr" à "Risque Très Élevé")
breaks_risk <- c(-Inf, 0.1, 0.5, 1, 5, 10, Inf)
cols_risk <- c(
  "darkgreen",   # 0 - 0.1 (Pas de risque)
  "green",       # 0.1 - 0.5
  "lightgreen",  # 0.5 - 1 (Limite)
  "yellow",      # 1 - 5 (Risque modéré)
  "saddlebrown", # 5 - 10 (Risque fort)
  "#4A2C2A"      # > 10 (Risque très sévère)
)

# Projection du risque sur la zone d'étude
poly_vect <- terra::project(terra::vect(roi_metaleurop), terra::crs(spcmdl_transfer))
rast_crop <- terra::crop(spcmdl_transfer[["herbivore"]], poly_vect)
rast_final <- terra::mask(rast_crop, poly_vect)

terra::plot(rast_final,
            breaks = breaks_risk,
            col = cols_risk,
            main = "Cartographie de l'Indice de Risque (Herbivore)")

