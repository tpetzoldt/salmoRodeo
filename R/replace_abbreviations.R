#' Apply multiple string replacements to a data frame column.
#'
#' This function iteratively applies a set of pattern-replacement pairs to the
#' 'expression' column of a data frame. It performs multiple passes to ensure
#' all replacements are applied, even if they depend on previous replacements.
#' Patterns are treated as full words (or preceded by non-word characters and
#' followed by non-word characters or an opening parenthesis) to prevent
#' partial word matches.
#'
#' @param file A data frame that must contain a column named `expression`.
#' @param replacements A data frame with at least two columns: `pattern`
#'   (strings to be found) and `replacement` (strings to replace them with).
#' @param npass The number of passes to apply the replacements. Multiple
#'   passes are useful if replacements can affect subsequent patterns.
#' @return The input data frame `file` with the `expression` column modified
#'   by the replacements.
#' @importFrom stringr str_replace_all
#'
#' @export
replace_abbreviations <- function(file, replacements, npass = 10) {
  # Create named vector of patterns and replacements for str_replace_all
  # This makes the patterns look for full words or followed by an opening parenthesis
  patterns <- paste0("(?<!\\w)", replacements$pattern, "(?!\\w|\\()")
  replacement_map <- setNames(replacements$replacement, patterns)

  for (pass in 1:npass) {
    file$expression <- stringr::str_replace_all(
      string = file$expression,
      pattern = replacement_map
    )
  }
  file
}
