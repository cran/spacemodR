# spacemodR (development version)

## New Features & Improvements

* **Species occurrences from GBIF:** new function `get_gbif_data()`, which downloads the occurrence records of a species for a region of interest with the public GBIF search API (no account needed) and returns them as `sf` points.
* New article "Species Occurrences (GBIF)", which compares the records of the wood mouse (*Apodemus sylvaticus*) with its habitat scores from EUNIS and OCS-GE.

# spacemodR 0.3.5

## New Features & Improvements

* **Stacking order of habitat layers:** `habitat()`, `add_habitat()` and `add_nohabitat()` gain a `priority` argument, stored in a new `priority` column of the `habitat` object. In `habitat_raster()`, overlapping features are stacked by priority and the pixel takes the value of the top one, so the result no longer depends on the order of the `add_*()` calls. By default each new layer goes on top of the previous ones, as before.
* **Continuity of thin structures:** `habitat()`, `add_habitat()` and `add_nohabitat()` gain a `touches` argument, stored in a new `touches` column. With `touches = TRUE`, lines and polygons fill every pixel they touch in `habitat_raster()`, which keeps hedgerows, rivers or roads continuous; with `touches = FALSE`, polygons fill the pixels whose centre they cover.
* **Mixed geometries:** `habitat_raster()` now accepts points, lines and polygons in the same `habitat` object.
* The "Habitat Layer" article explains `priority` and `touches` on a watercourse of the Metaleurop landscape.
* **OCS-GE from IGN:** new function `get_ocsge()`, a single entry point to get the OCS-GE of a region of interest, with a `source` argument. `source = "ign"` calls the new `get_ocsge_data_ign()`, which downloads the per-department GeoPackages of the IGN Géoplateforme (no API key), keeps them in a cache directory and lets you choose the vintage with `year`. `source = "fgb"` calls `get_ocsge_data_fgb()`, unchanged. Extracting the IGN archives needs the `archive` package or the `7z` command line tool.
* **OCS-GE / EUNIS crosswalk:** new dataset `ref_ocsge_eunis`, a ranked correspondence between the 14 OCS-GE cover classes (`CS`), optionally refined by land use (`US`), and the 45 EUNIS Level 2 classes, with the share of each match measured on the Metaleurop site.
* **Land use in OCS-GE habitats:** `ocsge_species_dict` now has rows for 47 (cover, use) pairs (`code_us`) in addition to the 14 covers, derived from `eunis_species_dict` through `ref_ocsge_eunis`. `join_ocsge_species()` gains a `us_col` argument: when the polygons carry the land use, they take the scores of their (cover, use) pair, and of their cover otherwise. New dataset `ocsge_us_metaleurop`, the OCS-GE of the Metaleurop site with cover and use.
* **Habitat data sources guide:** the article "Habitat Layer from EUNIS" becomes "Habitat Data Sources". It compares OCS-GE cover, OCS-GE cover and use, and EUNIS, shows how to move between them, and lists the other sources that could refine them.

## Behaviour changes

* `touches` defaults to `TRUE`, whereas `habitat_raster()` used to fill the pixels whose centre is covered by a polygon, and the pixels of a thin line. Existing rasters therefore change: features are drawn larger. Set `touches = FALSE` to get the previous result, in particular for layers of contiguous polygons such as a land cover map.
* **`ocsge_species_dict` rebuilt with the columns of `eunis_species_dict`:** `weight_global` becomes `suitability`, `weight_movement` becomes `permeability`, `weight_breeding` and the species-level columns (`sppID`, `taxon_group`, `habitat_breadth`, `specialization`, `confidence`, `in_france`) are added, and cover codes are written without space (`"CS2.2.1"`). The scores themselves changed: they follow an anchored scale, are lower on classes that are not habitat, and movement accounts for flight. `resistance` is now on a 1-100 logarithmic scale derived from `permeability` (it was a 1-10 score): code that rescaled it, or tested `resistance == 10`, must be updated (use `permeability == 0` for impassable classes).
* **`nom_espece` renamed `species_name`** in `ocsge_species_dict` and `eunis_species_dict`.
* `plot_species_habitat()` now expects `suitability` instead of `weight_global`, and draws resistance on a log scale.

## Bug fixes

* `habitat_raster()` no longer fails on an empty `habitat` object.
* `get_eunis_data()` returned the raster upside down (north and south swapped), because the image exported by the EEA service carries no georeferencing. The image is now georeferenced before being read. The Metaleurop layer shipped in `inst/extdata/eunis_metaleurop_L2.tif` was rebuilt; rasters obtained with earlier versions must be downloaded again.

## Packaging

* Preparation of the CRAN submission: shorter package title, `get_eunis_data()` now takes `service_url = NULL` by default (the EEA service, as before), the examples of `habitat_raster()` and `plot()` for habitat objects are run, and the vignette figures are lighter.
* `calibrate_simple()` checked for the wrong compiled model before sampling.
* `dispersal(method = "omniscape")` reports its progress with `message()` instead of `cat()`.

# spacemodR 0.3.0

## New Features & Improvements

* **Soil interpolation (kriging):** new functions to build soil contamination rasters from sampling points, without additional dependency: `soil_variogram()`, `fit_soil_variogram()`, `soil_vgm()`, `krige_soil()` (ordinary kriging or kriging with external drift), `cv_soil()` (random or spatial-block cross-validation), `kernel_smooth_soil()` (Gaussian smoothing reusing `compute_kernel()`), `soil_grid()` and `source_covariates()` (distance / wind covariates around a point source). New dataset `soil_metaleurop` (595 soil Cd, Pb, Zn samples) and new article "Soil contamination maps by kriging".
* **EUNIS land cover:** new function `get_eunis_data()` to download EUNIS Level 1 / Level 2 land cover rasters for a region of interest from the EEA Ecosystem Type Map v3.1 service, and new lookup dataset `ref_eunis`.
* **Trophic graph plots:** `plot()` for trophic objects gains link weights and custom colors.
* **Dispersal:** cleaned up the kernel dispersal code.

## Packaging

* Installable directly from GitHub with `remotes::install_github("Qonfluens/spacemodR")`.
* Source and binary packages are attached to each GitHub release.

# spacemodR 0.2.0

## Breaking Changes

* **Refactored trophic fluxes interface:** The `intake()` function has been completely redesigned and renamed to `flux()` to better reflect the mathematical and ecological concept of transfer rates.
* **New pipe-friendly syntax:** The definition of transfer parameters no longer uses multiple named arguments. Users must now initialize the table with `flux()` and chain specific rules using the new `add_flux()` function.
* **Namespace conflict resolution:** The internal utility function previously named `flux()` (used within the `transfer()` function) has been renamed to avoid conflicts with the new main API.

## New Features & Improvements

* **New `add_flux()` function:** Allows for adding transfer rules in a readable, sequential manner (e.g., `add_flux(from = "soil", to = "plant", value = ~ 10^x / 32)`).
* **Global target rules:** In `add_flux()`, if the source compartment (`from`) is not specified, the rule (`value`) is automatically applied to all links pointing to the target compartment (`to`).
* **Safe default values:** The `flux()` function now features an explicit `default` parameter. It is highly recommended to use `1` (full transfer) or `0` (no transfer) instead of `NA` to prevent the propagation of missing values during subsequent model calculations.

# spacemodR 0.1.3

* **Bug Fixes:**
  * Fixed an `rlang_error` (replacement has length zero) in `spacemodel` raster layer sub-assignment within the Tutorial vignette.
  * Removed non-ASCII characters (smart quotes) from the `DBFunc_BirdFuncDat` dataset to pass CRAN checks.
  * Resolved R CMD Check notes regarding `no visible binding for global variable` (`weight_global`, `weight_foraging`, `resistance`).
* **Documentation & Data:**
  * Added complete `roxygen2` documentation for internal datasets: `FmrBT`, `DBFunc_MamFuncDat`, and `DBFunc_BirdFuncDat`.
  * Translated and enhanced the `Food_Intake` vignette into professional scientific English.
  * Updated the `pkgdown` website structure to use a clean 3-tier navigation system (Quick Start, Core Guides, Example Zoo).

# spacemodR 0.1.2

* **New Features:**
  * Added the `Food_Intake` module and dataset integration (FmrBT, EltonTraits) to compute Daily Food Intake (DFI).
  * Included the Eco-SSL (Ecological Soil Screening Levels) risk index assessment example.
* **Enhancements:**
  * Improved the `Omniscape_Connectivity` tutorial with real-world spatial data for the Wood Mouse (*Apodemus sylvaticus*).

# spacemodR 0.1.0

* **Initial Release:**
  * Introduced the core `spacemodel` class for merging raster stacks and trophic networks.
  * Added `transfer` and `dispersal` modeling functions.
  * Integrated external Julia Omniscape connectivity algorithms.
