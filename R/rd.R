#' Read a Tab-Separated Value (TSV) file into a data frame.
#'
#' This is a wrapper around `read.table` specifically configured for TSV files,
#' with headers, and treating strings as characters rather than factors.
#'
#' @param f The path to the TSV file.
#' @param ... Additional arguments to pass to `read.table`.
#' @return A data frame containing the data from the TSV file.
#' @seealso [read.table()]
#'
#' @keywords internal
#'
rd <- function(f, ...) {
  read.table(file = f,
             header = TRUE, sep = "\t", stringsAsFactors = FALSE, ...)
}
