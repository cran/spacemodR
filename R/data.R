#' SF object defining ROI metaleurop
#'
#' @description Simple feature collection with 1 feature and 1 field.
#' Geodetic CRS:  WGS 84.
#'
#' @usage data(roi_metaleurop)
#'
#' @examples
#' data(roi_metaleurop)
#' @keywords datasets
"roi_metaleurop"


#' SF object defining very simplified OCS-GE soil cover metaleurop
#'
#' @description Simple feature collection with 9 features and 11 fields.
#' Projected CRS: RGF93 v1 / Lambert-93.
#'
#' @usage data(ocsge_metaleurop)
#'
#' @examples
#' data(ocsge_metaleurop)
#' @keywords datasets
"ocsge_metaleurop"

#' OCS-GE land cover and land use of the Metaleurop site
#'
#' @description
#' OCS-GE polygons of \code{\link{roi_metaleurop}} with both the cover
#' (\code{CS}) and the use (\code{US}), dissolved into one multipolygon per
#' (cover, use) pair. \code{\link{ocsge_metaleurop}} is dissolved by cover
#' only and cannot be used to study the land use.
#'
#' @format An \code{sf} object (EPSG:2154) with 68 multipolygons and 3 columns:
#' \describe{
#'   \item{code_cs}{Character. OCS-GE cover code (e.g. \code{"CS2.2.1"}).}
#'   \item{code_us}{Character. OCS-GE use code (e.g. \code{"US1.1"}).}
#'   \item{area_ha}{Numeric. Area of the (cover, use) pair on the site, in hectares.}
#' }
#'
#' @details
#' Built from the 2021 OCS-GE of the Nord and Pas-de-Calais departments, as
#' returned by \code{\link{get_ocsge_data_fgb}}. Outlines are simplified with
#' a 3 m tolerance to keep the dataset small.
#'
#' @seealso \code{\link{join_ocsge_species}}, \code{\link{ocsge_species_dict}}
#'
#' @usage data(ocsge_us_metaleurop)
#'
#' @examples
#' data(ocsge_us_metaleurop)
#' head(ocsge_us_metaleurop)
#' @keywords datasets
"ocsge_us_metaleurop"


#' Nomenclature of OCS-GE soil cover
#'
#' @usage data(ref_ocsge)
#'
#' @examples
#' data(ref_ocsge)
#' @keywords datasets
"ref_ocsge"

#' Nomenclature and render colors of EUNIS land cover classes
#'
#' @description
#' Code / label / render-color lookup table for the EEA "Ecosystem Type Map
#' v3.1, terrestrial part" ArcGIS service, at both EUNIS Level 1 (9 broad
#' classes) and Level 2 (46 detailed classes). It is used internally by
#' \code{\link{get_eunis_data}} to decode the service's exported image back
#' into EUNIS codes, since that service publishes no raw-value raster
#' endpoint.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{level}{Character. \code{"L1"} or \code{"L2"}.}
#'   \item{code}{Character. The EUNIS code (e.g. \code{"J1"} at L2, \code{"J"} at L1).}
#'   \item{nomenclature}{Character. The class label.}
#'   \item{couleur}{Character. The hex render color used by the source ArcGIS service.}
#'   \item{r, g, b}{Integer (0-255). The render color, split into channels for exact matching.}
#' }
#'
#' @details
#' At \code{level == "L2"}, two pairs of classes share an identical render
#' color and cannot be distinguished from the exported image alone:
#' \code{D5}/\code{F6} and \code{D6}/\code{F7}. \code{level == "L1"} has no
#' such collision. See \code{\link{get_eunis_data}} for how this is handled.
#'
#' @source European Environment Agency, "Ecosystem Type Map v3.1, terrestrial
#' part": \url{https://bio.discomap.eea.europa.eu/arcgis/rest/services/Ecosystem/EcosystemTypeMap_v3_1_Terrestrial/MapServer}
#'
#' @usage data(ref_eunis)
#'
#' @examples
#' data(ref_eunis)
#' @keywords datasets
"ref_eunis"

#' Crosswalk between OCS-GE (cover and use) and EUNIS Level 2
#'
#' @description
#' Correspondence table between the French OCS-GE nomenclature and the 45
#' EUNIS Level 2 classes of \code{\link{ref_eunis}}. The two nomenclatures do
#' not nest: OCS-GE cover (\code{CS}) describes what physically covers the
#' ground in 14 classes, while EUNIS describes habitats. The relation is
#' therefore many-to-many, and each OCS-GE class is given a ranked list of
#' EUNIS classes. Rows with a land use code (\code{US}) refine the match for a
#' given cover: for instance herbaceous cover (\code{CS2.2.1}) is arable land
#' (\code{I1}) under agricultural use, but gardens and parks (\code{I2}) under
#' residential use.
#'
#' @format A data frame with 152 rows and 7 columns:
#' \describe{
#'   \item{code_cs}{Character. OCS-GE cover code, as in \code{ref_ocsge$code_cs} (e.g. \code{"CS2.2.1"}).}
#'   \item{code_us}{Character. OCS-GE use code (e.g. \code{"US1.1"}), or \code{NA} for the cover-only match, to be used when the use is unknown or has no dedicated row.}
#'   \item{code_eunis}{Character. EUNIS Level 2 code, as in \code{ref_eunis$code} and \code{eunis_species_dict$code_eunis}.}
#'   \item{rank}{Integer. Order of the EUNIS classes within a (\code{code_cs}, \code{code_us}) group; \code{1} is the default match.}
#'   \item{main_cs}{Logical. \code{TRUE} on the single row giving the main OCS-GE cover of each EUNIS class (EUNIS to OCS-GE direction, 45 rows).}
#'   \item{pct_metaleurop}{Numeric. Measured share (\%) of the OCS-GE group area falling in this EUNIS class on the Metaleurop site; \code{NA} when the group covers less than 10 ha there. See Details.}
#'   \item{note}{Character. What the match stands for, and what extra data would be needed to resolve it.}
#' }
#'
#' @details
#' \code{rank} is an expert reading of the two nomenclatures (CNIG OCS-GE
#' specification and EUNIS habitat definitions). \code{pct_metaleurop} is a
#' measurement: OCS-GE polygons of \code{\link{roi_metaleurop}} were overlaid
#' on the EUNIS raster returned by \code{\link{get_eunis_data}}. The two
#' columns are not expected to agree closely, because the EUNIS raster has a
#' 100 m pixel carrying one dominant class, while OCS-GE maps objects a few
#' meters wide: a road or a garden usually falls in a pixel classified as
#' buildings (\code{J1}, \code{J2}). The measurement also only covers the
#' classes present on this site.
#'
#' Some EUNIS distinctions cannot be recovered from OCS-GE, with or without
#' the use: wetlands (\code{D}), grassland types (\code{E1} to \code{E4}),
#' standing versus running water (\code{C1}, \code{C2}), deciduous versus
#' evergreen broadleaved woodland (\code{G1}, \code{G2}), and crops versus
#' permanent grassland, which share the same cover and the same agricultural
#' use.
#'
#' The source table and the build script are
#' \code{raw_data/ocsge_eunis_crosswalk.csv} and
#' \code{raw_data/build_ref_ocsge_eunis.R} of the source repository.
#'
#' @seealso \code{\link{ref_ocsge}}, \code{\link{ref_eunis}},
#' \code{\link{eunis_species_dict}}, \code{\link{get_ocsge_data_fgb}}
#'
#' @usage data(ref_ocsge_eunis)
#'
#' @examples
#' data(ref_ocsge_eunis)
#'
#' # default EUNIS class of each OCS-GE cover
#' subset(ref_ocsge_eunis, is.na(code_us) & rank == 1)
#'
#' # main OCS-GE cover of each EUNIS class
#' subset(ref_ocsge_eunis, main_cs, c(code_eunis, code_cs))
#'
#' # EUNIS class of OCS-GE polygons from cover and use, falling back on cover alone
#' ocs <- data.frame(code_cs = c("CS2.2.1", "CS2.2.1", "CS2.1.1.1"),
#'                   code_us = c("US1.1", "US5", "US1.2"))
#' best <- subset(ref_ocsge_eunis, rank == 1)
#' with_us <- best$code_eunis[match(paste(ocs$code_cs, ocs$code_us),
#'                                  paste(best$code_cs, best$code_us))]
#' cs_only <- best$code_eunis[match(paste(ocs$code_cs, NA),
#'                                  paste(best$code_cs, best$code_us))]
#' ocs$code_eunis <- ifelse(is.na(with_us), cs_only, with_us)
#' ocs
#'
#' @keywords datasets
"ref_ocsge_eunis"

#' DataBase of collected MicroMammals species
#'
#' @usage data(sf_micromammals)
#'
#' @examples
#' data(sf_micromammals)
#' @keywords datasets
"sf_micromammals"

#' Species habitat dictionary for OCS-GE land cover and land use
#'
#' @description
#' Habitat suitability, breeding, foraging and movement scores of 935 European
#' terrestrial vertebrates (amphibians, reptiles, birds, mammals) for the
#' French OCS-GE nomenclature: the 14 cover classes (\code{CS}), and 47
#' (cover, use) pairs for which the land use (\code{US}) changes what the
#' cover stands for. It has the same columns as
#' \code{\link{eunis_species_dict}}, plus \code{code_us}.
#'
#' @format A data frame with 57,035 rows (935 species x 61 classes) and 14 columns:
#' \describe{
#'   \item{code_cs}{Character. OCS-GE cover code, as in \code{ref_ocsge$code_cs} (e.g. \code{"CS2.2.1"}).}
#'   \item{code_us}{Character. OCS-GE use code (e.g. \code{"US1.1"}), or \code{NA} on the rows giving the scores of the cover whatever its use.}
#'   \item{species_name}{Character. Scientific name, genus and species separated by \code{"_"}.}
#'   \item{sppID}{Character. Species identifier from the EEA source table; the first letter gives the group (A, R, B, M).}
#'   \item{taxon_group}{Character. \code{"amphibian"}, \code{"reptile"}, \code{"bird"} or \code{"mammal"}.}
#'   \item{suitability}{Integer (0-10). Overall habitat suitability: 0 unused or avoided, 2 marginal, 5 secondary, 8 main, 10 core habitat.}
#'   \item{weight_breeding}{Integer (0-10). Value of the class as a breeding, nesting, denning, roosting or larval site.}
#'   \item{weight_foraging}{Integer (0-10). Value of the class for feeding.}
#'   \item{permeability}{Integer (0-10). Ease of movement through the class: 10 moves freely, 7 readily, 4 reluctantly, 1 strong barrier, 0 impassable. Accounts for flight, swimming or fossorial habits.}
#'   \item{resistance}{Numeric (1-100). Derived from permeability as \code{10^(2 * (1 - permeability / 10))}: 10 gives 1, 5 gives 10, 0 gives 100.}
#'   \item{habitat_breadth}{Integer. Species-level: number of cover classes (out of 14) with \code{suitability >= 5}.}
#'   \item{specialization}{Numeric (0-1). Species-level: 1 minus the normalised Shannon evenness of the suitability profile over the 14 cover classes (0 generalist, 1 specialist).}
#'   \item{confidence}{Integer (1-3). Species-level: 1 vague or missing source description, 2 medium, 3 detailed.}
#'   \item{in_france}{Integer (0/1). Species-level: 1 if the species occurs natively and regularly in metropolitan France (expert knowledge). The dictionary covers Europe while OCS-GE covers France: filter on this column first.}
#' }
#'
#' @details
#' \strong{Cover rows} (\code{code_us = NA}, 935 x 14). Scores were assigned by
#' a large language model (Claude Opus 5.5) reading the EEA habitat and
#' avoided-habitat descriptions of each species against an ecological
#' description of each cover class, with the anchored rubric given above.
#'
#' \strong{Cover x use rows} (935 x 47). They are derived, not scored again:
#' each (cover, use) pair is linked to one or several EUNIS Level 2 classes
#' by \code{\link{ref_ocsge_eunis}}, and takes the mean of their
#' \code{\link{eunis_species_dict}} scores weighted by \code{1 / rank}. For
#' instance herbaceous cover under agricultural use (\code{CS2.2.1},
#' \code{US1.1}) is mostly arable land (\code{I1}, rank 1) but may be permanent
#' grassland (\code{E2}, rank 2). Only the pairs listed in
#' \code{ref_ocsge_eunis} have a row; for any other pair use the cover row,
#' which is what \code{\link{join_ocsge_species}} does.
#'
#' These are expert-judgement scores and have not been validated against
#' occurrence data. They were checked against the keywords of the EEA habitat,
#' avoided-habitat and threat descriptions
#' (\code{raw_data/check_ocsge_species_dict.R} of the source repository):
#' species naming a habitat score higher on the matching class than the
#' others, with a probability of 0.57 to 0.93 depending on the habitat for
#' cover rows, and 0.68 to 0.93 for cover x use rows. The use helps most for
#' gardens and parks (0.57 to 0.80), quarries (0.69 to 0.93) and arable land
#' (0.74 to 0.77).
#'
#' \strong{Change from spacemodR 0.3.0.} The dictionary used to hold scores
#' from another model for the cover classes only, in the columns
#' \code{weight_global}, \code{weight_movement}, \code{weight_foraging} and
#' \code{resistance} (1-10), with \code{nom_espece} and cover codes written
#' with a space (\code{"CS 2.2.1"}). \code{suitability} replaces
#' \code{weight_global} and \code{permeability} replaces
#' \code{weight_movement}; values differ (Spearman correlation 0.72 for
#' suitability, 0.22 for movement, mostly because flying species were scored
#' as if they walked) and are lower on classes that are not habitat.
#'
#' @references
#' The species list and the habitat descriptions come from:
#'
#' O'Connor, L. M. J., Renaud, J., Dou, Y., Karger, D. N., Maiorano, L., Verburg, P. H., &
#' Thuiller, W. (2024). Habitat Suitability of European Land Systems for Terrestrial Vertebrates.
#' \emph{Global Ecology and Biogeography}, 33(11), e13903.
#'
#' @seealso \code{\link{join_ocsge_species}}, \code{\link{ref_ocsge}},
#' \code{\link{ref_ocsge_eunis}}, \code{\link{eunis_species_dict}}
#'
#' @usage data(ocsge_species_dict)
#'
#' @examples
#' data(ocsge_species_dict)
#' apsy <- subset(ocsge_species_dict, species_name == "Apodemus_sylvaticus")
#'
#' # scores of each cover, whatever its use
#' subset(apsy, is.na(code_us), c(code_cs, suitability, permeability, resistance))
#'
#' # herbaceous cover, by use
#' subset(apsy, code_cs == "CS2.2.1", c(code_us, suitability, weight_foraging))
#'
#' @keywords datasets
"ocsge_species_dict"

#' Species habitat dictionary for EUNIS Level 2 land cover classes
#'
#' @description
#' Habitat suitability, breeding, foraging and movement scores of 935 European
#' terrestrial vertebrates (amphibians, reptiles, birds, mammals) for each of
#' the 45 EUNIS Level 2 classes of the EEA Ecosystem Type Map v3.1 (terrestrial
#' part), as returned by \code{\link{get_eunis_data}} with \code{level = "L2"}.
#' It is the EUNIS counterpart of \code{\link{ocsge_species_dict}}, with an
#' improved schema: scores follow an anchored rubric, breeding is separated
#' from foraging, and resistance is derived from permeability on the 1-100 log
#' scale expected by connectivity models such as Omniscape.
#'
#' @format A data frame with 42,075 rows (935 species x 45 EUNIS classes) and 13 columns:
#' \describe{
#'   \item{code_eunis}{Character. EUNIS Level 2 code (e.g. \code{"G1"}); matches \code{ref_eunis$code} for \code{level == "L2"}.}
#'   \item{species_name}{Character. Scientific name, genus and species separated by \code{"_"}.}
#'   \item{sppID}{Character. Species identifier from the EEA source table; the first letter gives the group (A, R, B, M).}
#'   \item{taxon_group}{Character. \code{"amphibian"}, \code{"reptile"}, \code{"bird"} or \code{"mammal"}.}
#'   \item{suitability}{Integer (0-10). Overall habitat suitability: 0 unused or avoided, 2 marginal, 5 secondary, 8 main, 10 core habitat.}
#'   \item{weight_breeding}{Integer (0-10). Value of the class as a breeding, nesting, denning, roosting or larval site.}
#'   \item{weight_foraging}{Integer (0-10). Value of the class for feeding.}
#'   \item{permeability}{Integer (0-10). Ease of movement through the class: 10 moves freely, 7 readily, 4 reluctantly, 1 strong barrier, 0 impassable. Accounts for flight, swimming or fossorial habits.}
#'   \item{resistance}{Numeric (1-100). Derived from permeability as \code{10^(2 * (1 - permeability / 10))}: 10 gives 1, 5 gives 10, 0 gives 100.}
#'   \item{habitat_breadth}{Integer. Species-level: number of EUNIS classes with \code{suitability >= 5}.}
#'   \item{specialization}{Numeric (0-1). Species-level: 1 minus the normalised Shannon evenness of the suitability profile (0 generalist, 1 specialist).}
#'   \item{confidence}{Integer (1-3). Species-level: 1 vague or missing source description, 2 medium, 3 detailed.}
#'   \item{in_france}{Integer (0/1). Species-level: 1 if the species occurs natively and regularly in metropolitan France (expert knowledge).}
#' }
#'
#' @details
#' Scores were assigned by a large language model (Claude Opus 5.5) reading the
#' EEA habitat and avoided-habitat descriptions of each species
#' (\code{raw_data/spp_EEA_habitats_cleaned.csv}) against short ecological
#' descriptions of each EUNIS class. They are expert-judgement scores and have
#' not been validated against occurrence data. Four species had an empty
#' description and were scored from general knowledge (\code{confidence = 1}).
#' The scoring files, the comparison with \code{ocsge_species_dict} and the
#' build scripts are in \code{raw_data/opus_scoring/} of the source repository.
#'
#' @seealso \code{\link{get_eunis_data}}, \code{\link{ref_eunis}}, \code{\link{ocsge_species_dict}}
#'
#' @usage data(eunis_species_dict)
#'
#' @examples
#' data(eunis_species_dict)
#' subset(eunis_species_dict, species_name == "Apodemus_sylvaticus")
#'
#' @keywords datasets
"eunis_species_dict"
#' Data concentration Cd Soil - Vegetation extracted from BAPPET
#'
#' @usage data(bappet_cd)
#'
#' @examples
#' data(bappet_cd)
#' @keywords datasets
"bappet_cd"

#' Data concentration Cd Soil - Earthworm
#'
#' @usage data(earthworm_cd)
#'
#' @examples
#' data(earthworm_cd)
#' @keywords datasets
"earthworm_cd"


#' Field Metabolic Rates Database (FmrBT)
#'
#' A comprehensive database containing Field Metabolic Rates (FMR), body mass,
#' and ambient temperature across more than 700 species.
#'
#' @format A data frame with variables detailing energetic properties and taxonomy:
#' \describe{
#'   \item{Kingdom}{Taxonomic kingdom.}
#'   \item{Phylum}{Taxonomic phylum.}
#'   \item{Class}{Taxonomic class.}
#'   \item{Order}{Taxonomic order.}
#'   \item{Family}{Taxonomic family.}
#'   \item{Genus}{Taxonomic genus.}
#'   \item{SpeciesVerbatim}{Original species name as recorded in the source.}
#'   \item{SpeciesAcceptedName}{Standardized accepted species name.}
#'   \item{FMR_kJ_d}{Field metabolic rate per individual in kJ/day.}
#'   \item{Mass_g}{Body mass of the individual in grams.}
#'   \item{Temp_C}{The ambient temperature recorded during the study in Celsius.}
#'   \item{Endotherm}{Indicator of thermal status (1/TRUE for endotherm, 0/FALSE for ectotherm).}
#'   \item{Reference}{Source literature reference.}
#'   \item{Comment}{Additional notes from the dataset creators.}
#'   \item{Outlier}{Flag indicating if the record is considered a statistical outlier.}
#' }
#' @source De Castro et al. (2025) FmrBT database.
"FmrBT"

#' Mammal Functional Traits Dataset (EltonTraits 1.0)
#'
#' Dietary category percentages, foraging strategies, and functional traits for mammals.
#'
#' @format A data frame containing species dietary breakdowns (summing to 100%) and trait data:
#' \describe{
#'   \item{MSW3_ID}{Mammal Species of the World (3rd ed.) identifier.}
#'   \item{Scientific}{Scientific name of the mammal species.}
#'   \item{MSWFamilyLatin}{Mammal Species of the World (3rd ed.) family name.}
#'   \item{Diet.Inv}{Percentage of invertebrates in the diet.}
#'   \item{Diet.Vend}{Percentage of vertebrate endotherms consumed (birds and mammals).}
#'   \item{Diet.Vect}{Percentage of vertebrate ectotherms consumed (reptiles and amphibians).}
#'   \item{Diet.Vfish}{Percentage of fish consumed.}
#'   \item{Diet.Vunk}{Percentage of unknown/unclassified vertebrates in the diet.}
#'   \item{Diet.Scav}{Percentage of scavenging activity.}
#'   \item{Diet.Fruit}{Percentage of fruits consumed.}
#'   \item{Diet.Nect}{Percentage of nectar/pollen consumed.}
#'   \item{Diet.Seed}{Percentage of seeds/nuts consumed.}
#'   \item{Diet.PlantO}{Percentage of other plant tissues consumed (leaves, stems, roots).}
#'   \item{Diet.Source}{Source of the dietary information.}
#'   \item{Diet.Certainty}{Certainty score for the diet data.}
#'   \item{ForStrat.Value}{Primary foraging stratum/habitat.}
#'   \item{ForStrat.Certainty}{Certainty score for the foraging stratum.}
#'   \item{ForStrat.Comment}{Notes on foraging strategy.}
#'   \item{Activity.Nocturnal}{Binary flag for nocturnal activity.}
#'   \item{Activity.Crepuscular}{Binary flag for crepuscular activity.}
#'   \item{Activity.Diurnal}{Binary flag for diurnal activity.}
#'   \item{Activity.Source}{Source of the activity information.}
#'   \item{Activity.Certainty}{Certainty score for the activity pattern.}
#'   \item{BodyMass.Value}{Body mass value in grams.}
#'   \item{BodyMass.Source}{Source of the body mass information.}
#'   \item{BodyMass.SpecLevel}{Specificity level of the body mass record.}
#' }
#' @source EltonTraits 1.0 database.
"DBFunc_MamFuncDat"

#' Bird Functional Traits Dataset (EltonTraits 1.0)
#'
#' Dietary category percentages, foraging strategies, and functional traits for birds.
#'
#' @format A data frame containing species dietary breakdowns (summing to 100%) and trait data:
#' \describe{
#'   \item{SpecID}{Unique species identifier.}
#'   \item{PassNonPass}{Indicates whether the bird is a Passerine or Non-Passerine.}
#'   \item{IOCOrder}{IOC taxonomic order.}
#'   \item{BLFamilyLatin}{BirdLife International family Latin name.}
#'   \item{BLFamilyEnglish}{BirdLife International family English name.}
#'   \item{BLFamSequID}{BirdLife International family sequence ID.}
#'   \item{Taxo}{Taxonomic grouping code.}
#'   \item{Scientific}{Scientific name of the bird species.}
#'   \item{English}{Common English name of the bird species.}
#'   \item{Diet.Inv}{Percentage of invertebrates in the diet.}
#'   \item{Diet.Vend}{Percentage of vertebrate endotherms consumed.}
#'   \item{Diet.Vect}{Percentage of vertebrate ectotherms consumed.}
#'   \item{Diet.Vfish}{Percentage of fish consumed.}
#'   \item{Diet.Vunk}{Percentage of unknown/unclassified vertebrates in the diet.}
#'   \item{Diet.Scav}{Percentage of scavenging activity.}
#'   \item{Diet.Fruit}{Percentage of fruits consumed.}
#'   \item{Diet.Nect}{Percentage of nectar/pollen consumed.}
#'   \item{Diet.Seed}{Percentage of seeds/nuts consumed.}
#'   \item{Diet.PlantO}{Percentage of other plant tissues consumed.}
#'   \item{Diet.5Cat}{Simplified 5-category diet classification.}
#'   \item{Diet.Source}{Source of the dietary information.}
#'   \item{Diet.Certainty}{Certainty score for the diet data.}
#'   \item{Diet.EnteredBy}{Identifier of the person who entered the diet data.}
#'   \item{ForStrat.watbelowsurf}{Percentage of foraging time spent underwater.}
#'   \item{ForStrat.wataroundsurf}{Percentage of foraging time spent at the water surface.}
#'   \item{ForStrat.ground}{Percentage of foraging time spent on the ground.}
#'   \item{ForStrat.understory}{Percentage of foraging time spent in the understory.}
#'   \item{ForStrat.midhigh}{Percentage of foraging time spent in the mid-high canopy.}
#'   \item{ForStrat.canopy}{Percentage of foraging time spent in the upper canopy.}
#'   \item{ForStrat.aerial}{Percentage of foraging time spent in aerial foraging.}
#'   \item{PelagicSpecialist}{Indicator of pelagic specialization.}
#'   \item{ForStrat.Source}{Source of the foraging strategy data.}
#'   \item{ForStrat.SpecLevel}{Specificity level of the foraging strategy record.}
#'   \item{ForStrat.EnteredBy}{Identifier of the person who entered the foraging data.}
#'   \item{Nocturnal}{Indicator of nocturnal activity.}
#'   \item{BodyMass.Value}{Body mass value in grams.}
#'   \item{BodyMass.Source}{Source of the body mass information.}
#'   \item{BodyMass.SpecLevel}{Specificity level of the body mass record.}
#'   \item{BodyMass.Comment}{Notes on body mass data.}
#'   \item{Record.Comment}{General comments about the record.}
#' }
#' @source EltonTraits 1.0 database.
"DBFunc_BirdFuncDat"


#' Eco-SSL Toxicity Data for Multiple Taxonomic Groups
#'
#' A comprehensive dataset combining toxicity values used for the derivation of
#' Ecological Soil Screening Levels (Eco-SSL). It includes ecotoxicological data
#' (NOAEL and LOAEL) for mammals, birds, invertebrates, and plants exposed to
#' various chemical compounds.
#'
#' @format A tibble (data frame) with 9 variables:
#' \describe{
#'   \item{ERE}{Character. Abbreviation for the Ecological Receptor Endpoint or effect category (e.g., "BIO" for Biochemical, "BEH" for Behavior).}
#'   \item{order_tox_value}{Integer. The sequential order or index of the toxicity value within its specific group.}
#'   \item{tox_value_NOAEL}{Numeric. The No Observed Adverse Effect Level (NOAEL), typically expressed in mg/kg/day or mg/kg soil.}
#'   \item{tox_value_LOAEL}{Numeric. The Lowest Observed Adverse Effect Level (LOAEL), typically expressed in mg/kg/day or mg/kg soil.}
#'   \item{compound}{Character. The chemical compound or trace element evaluated (e.g., "Antimony", "Cadmium").}
#'   \item{species_group}{Character. The broad taxonomic or functional group of the tested species (e.g., "Mammalian Wildlife").}
#'   \item{test_organism}{Character. The common and/or scientific name of the specific organism tested (e.g., "Rat (Rattus norvegicus)").}
#'   \item{tox_value}{Numeric. The primary toxicity value retained for the assessment (usually corresponds to the NOAEL or a derived threshold).}
#'   \item{ERE_full}{Character. The full, unabbreviated name of the endpoint or effect category (e.g., "Biochemical", "Behavior").}
#' }
#'
#' @source Derived from the United States Environmental Protection Agency
#' (US EPA) Ecological Soil Screening Levels (Eco-SSL) database and documentation.
#'
#' @examples
#' data(SSD_ecoSSL_all)
#' head(SSD_ecoSSL_all)
#' table(SSD_ecoSSL_all$species_group)
"SSD_ecoSSL_all"
