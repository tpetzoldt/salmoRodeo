#' @title A 1-layer ecological lake model in Petersen-Gujer matrix notation
#'
#' @description The package implements a 1-layer version of the ecological lake
#' model SALMO using a tabular Petersen-Gujer matrix description. It simulates
#' main components of the pelagic foodweb in a shallow, non-stratified lake.
#'
#' @keywords internal
#'
#'
#' @details Usage of the package requires installed build tools,
#'   Gnu C/C++ and Gnu Fortran on Linux or
#'   [RTools](https://cran.r-project.org/bin/windows/Rtools/) on Windows.
#'
#' @author Justus Möller
#' @author Thomas Petzoldt  \email{thomas.petzoldt@@tu-dresden.de}
#'
#' @importFrom stats setNames
#' @importFrom utils read.table
#' @importFrom rodeo rodeo
#' @importFrom rodeo forcingFunctions
#' @importFrom stringr str_replace_all
#'
#' @export
"_PACKAGE"
