# spacemodR

[![Documentation](https://img.shields.io/badge/documentation-online-blue.svg)](https://qonfluens.github.io/spacemodR/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Spatial environmental risk assessment through food webs**

`spacemodR` is an R package for the ecological risk assessment of contaminated
sites. It starts from a map of soil contamination and follows the contaminant
through the food web, taking into account where each species lives and how it
moves. The result is a set of maps: habitat, exposure and risk for each species.

Full documentation: <https://qonfluens.github.io/spacemodR/>

## What the package does

The package covers the steps of a spatial risk assessment:

1. **Soil contamination.** Build a soil concentration raster from sampling
   points, by kriging or by kernel smoothing.
2. **Habitat.** Turn land cover maps (OCS-GE for France, EUNIS for Europe) into
   habitat rasters, with a weight for each species.
3. **Food web.** Describe who eats whom as a directed acyclic graph, with a
   weight on each link.
4. **Dispersal.** Spread each species around its habitat with a dispersal
   kernel, or with circuit theory (Omniscape, which requires Julia).
5. **Exposure and transfer.** Move the contaminant from the soil to each
   species along the links of the food web.
6. **Risk.** Compare the exposure maps with toxicity thresholds to map the
   risk.

The package also provides Bayesian models (written in Stan) to estimate
transfer parameters from field data.

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("Qonfluens/spacemodR")

# or a given release
remotes::install_github("Qonfluens/spacemodR@v0.3.0")
```

**System requirements.** `sf` and `terra` need GDAL, PROJ and GEOS. They are
already included in the CRAN binaries of these packages for Windows and macOS.
`spacemodR` contains compiled Stan models, so installing it from source needs
a C++ compiler ([Rtools](https://cran.r-project.org/bin/windows/Rtools/) on
Windows, Xcode command line tools on macOS) and takes a few minutes.

### Pre-built packages

Each [release](https://github.com/Qonfluens/spacemodR/releases) provides:

* `spacemodR_<version>.tar.gz`: source package (all platforms, needs a compiler),
* `spacemodR_<version>.zip`: Windows binary (no compiler needed),
* `spacemodR_<version>.tgz`: macOS binary (no compiler needed).

Binaries are built with the current R release. With an older R version, use
the source package. Download the file for your platform, then:

```r
# install the dependencies first (binaries do not install them)
install.packages(c("dplyr", "ggplot2", "httr", "patchwork", "Rcpp", "rlang",
                   "rstan", "rstantools", "sf", "terra"))

# source package
install.packages("path/to/spacemodR_<version>.tar.gz", repos = NULL, type = "source")
# Windows binary
install.packages("path/to/spacemodR_<version>.zip", repos = NULL, type = "win.binary")
# macOS binary
install.packages("path/to/spacemodR_<version>.tgz", repos = NULL, type = "binary")
```

### Docker

The repository contains a Dockerfile with all system dependencies, including
Julia for Omniscape.

```bash
# build the image
docker build -t spacemodr .

# run the container (RStudio Server on port 8787)
docker run --rm -ti \
  -p 8787:8787 \
  -e PASSWORD=yourpassword \
  spacemodr
```

## A short example

The example below builds two habitat layers, links them in a food web and
transfers cadmium from the soil to a consumer.

```r
library(spacemodR)

# data shipped with the package: land cover and soil cadmium (log10 scale)
data(ocsge_metaleurop)
ground_cd <- load_raster_extdata("ground_concentration_cd_compressed.tif")

# 1. habitats: all the area for the soil, herbaceous cover for the consumer
hab_soil <- habitat() |> add_habitat(ocsge_metaleurop)
hab_vole <- habitat() |>
  add_habitat(ocsge_metaleurop[ocsge_metaleurop$code_cs == "CS2.2.1", ])

stack <- raster_stack(
  list(habitat_raster(ground_cd, hab_soil), habitat_raster(ground_cd, hab_vole)),
  names = c("soil", "vole")
)

# 2. food web: the consumer takes up the contaminant from the soil
web <- trophic() |> add_link("soil", "vole")

# 3. spacemodel: the rasters and the food web together
model <- spacemodel(stack, web)
model[["soil"]][] <- ground_cd

# 4. transfer along the food web, with a Gaussian foraging kernel (sd = 200 m)
kernels <- list(soil = NA, vole = compute_kernel(radius = 200, GSD = 25))
fluxes <- flux(model) |> add_flux(from = "soil", to = "vole", value = 0.5)
exposure <- transfer(model, kernels, fluxes)

terra::plot(exposure)
```

## Documentation

* [Quick start](https://qonfluens.github.io/spacemodR/articles/Getting_Started_EN.html)
  (also [in French](https://qonfluens.github.io/spacemodR/articles/Getting_Started_FR.html))
* [Tutorial](https://qonfluens.github.io/spacemodR/articles/Tutorial.html):
  the whole workflow, step by step
* Guides: [soil maps by kriging](https://qonfluens.github.io/spacemodR/articles/core_Soil_Kriging.html),
  [habitat layers](https://qonfluens.github.io/spacemodR/articles/core_Habitat.html),
  [habitat data sources](https://qonfluens.github.io/spacemodR/articles/core_Habitat_Sources.html),
  [connectivity with Omniscape](https://qonfluens.github.io/spacemodR/articles/core_Omniscape_Connectivity.html),
  [food intake](https://qonfluens.github.io/spacemodR/articles/core_Food_Intake.html)
* Parameter estimation: [soil to target transfers](https://qonfluens.github.io/spacemodR/articles/stats_Inference_Direct.html),
  [trophic transfers](https://qonfluens.github.io/spacemodR/articles/stats_Inference_Trophic.html)
* [Function reference](https://qonfluens.github.io/spacemodR/reference/index.html)
  and [change log](https://qonfluens.github.io/spacemodR/news/index.html)

## Data included

| Dataset | Content |
|---|---|
| `soil_metaleurop` | Cd, Pb and Zn in 595 soil samples around the former Metaleurop smelter (France) |
| `roi_metaleurop`, `ocsge_metaleurop`, `ocsge_us_metaleurop` | Study area and OCS-GE land cover of the Metaleurop site |
| `ref_ocsge`, `ref_eunis`, `ref_ocsge_eunis` | OCS-GE and EUNIS nomenclatures, and the correspondence between them |
| `ocsge_species_dict`, `eunis_species_dict` | Habitat scores of species for each land cover class |
| `FmrBT` | Field metabolic rates, body mass and temperature |
| `DBFunc_BirdFuncDat`, `DBFunc_MamFuncDat` | Diet and functional traits of birds and mammals (EltonTraits 1.0) |
| `SSD_ecoSSL_all` | Eco-SSL toxicity data for several taxonomic groups |
| `bappet_cd`, `earthworm_cd`, `sf_micromammals` | Cadmium in soil, plants, earthworms and small mammals |

See the help page of each dataset for its source and its license.

## The SPACEMOD project

`spacemodR` is developed within the **SPACEMOD** project, funded by the French
agency for ecological transition (ADEME), call for projects *IMPACTS 2023*.

Older software for ecological risk assessment, such as TerraSys or BERISP, is
no longer maintained. SPACEMOD develops a new, generic and spatially explicit
tool for contaminated sites and soils, able to handle several exposure routes
and mixtures of chemicals in food webs.

Partners:

* UMR Chrono-environnement (CNRS / Université de Franche-Comté), project
  coordinator (Pr. Renaud Scheifler)
* Qonfluens SAS
* UK Centre for Ecology & Hydrology (UKCEH)

More information on the [ADEME research portal](https://recherche.ademe.fr/spacemod)
and on the [Chrono-environnement website](https://chrono-environnement.univ-fcomte.fr/project/spacemod/).

## Getting help

Please report bugs and ask questions on the
[issue tracker](https://github.com/Qonfluens/spacemodR/issues).

## For maintainers

### Release a new version

1. Update `Version:` in `DESCRIPTION` and add a section to `NEWS.md`.
2. Commit and push to `main`, then tag the commit and push the tag:
   ```bash
   git tag -a v0.3.0 -m "Release v0.3.0"
   git push origin v0.3.0
   ```
3. The workflow `.github/workflows/release.yaml` builds the source package and
   the Windows and macOS binaries, and attaches them to a new GitHub release.
   The tag must match the version in `DESCRIPTION`, otherwise the workflow
   stops.

### Large files

Large files such as fitted models are tracked with `git lfs`. Before adding a
new large file, track it:

```bash
git lfs track "raw_data/new_fit.rda"
```

## License

`spacemodR` is released under the MIT license. See the
[LICENSE](https://github.com/Qonfluens/spacemodR/blob/main/LICENSE.md) file.
