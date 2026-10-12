# Hack to get rid of spurious notes in package check, caused by uses
# of dplyr::{rename, filter}.
if (getRversion() >= "4.0.0")  utils::globalVariables(c(
 "trophic_tbl", "x_from", "y_from", "x_to", "y_to", "y",
  "node", "spcmdl_habitat", "plot_weight"))

#' The 'spacemodR' package.
#'
#' @description
#' Workflow for spatially explicit environmental risk assessment: habitat,
#' food web, dispersal, exposure and risk.
#'
#' @name spacemodR-package
#' @aliases spacemodR
#' @useDynLib spacemodR, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom rstantools rstan_config
#' @importFrom rlang .data
#'
#' @references
#' Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.21.2. https://mc-stan.org
NULL

