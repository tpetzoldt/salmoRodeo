#' Read replacement rules for a model based on the number of phytoplankton groups.
#'
#' This function reads a TSV file containing replacement rules for a model.
#' It handles different column names based on whether the model uses 2 or 3
#' phytoplankton groups, specifically by renaming columns for the 3-group case.
#'
#' @param file The path to the TSV file containing replacement rules.
#' @param phyto_groups An integer, either 2 or 3, specifying the number of
#'   phytoplankton groups in the model version.
#' @return A data frame with replacement rules, with column names adjusted
#'   according to `phyto_groups`.
#' @seealso [rd()]
#'
#'
#' @keywords internal
#'
read_replacements <- function(file, phyto_groups = 3) {
  if (!phyto_groups %in% 2:3) {
    stop("Invalid number of phyto groups specified. Must be 2 or 3.")
  }

  output <- rd(file)
  if (phyto_groups == 3) {
    # Similar to read_pros, this renames the 'replacement' column to
    # 'twoPhytoGroups' and 'threePhytoGroups' to 'replacement'.
    colnames(output)[colnames(output) == "replacement"] <- "twoPhytoGroups"
    colnames(output)[colnames(output) == "threePhytoGroups"] <- "replacement"
  }
  output
}
