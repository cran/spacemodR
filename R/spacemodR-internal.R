# Hack to get rid of spurious notes in package check, caused by uses
# of dplyr::{rename, filter}.
if (getRversion() >= "4.0.0")  utils::globalVariables(c(
 "trophic_tbl", "x_from", "y_from", "x_to", "y_to", "y",
  "node", "spcmdl_habitat"))
