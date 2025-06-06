#' Replace values in a named vector while preserving order and other elements.
#'
#' This function takes a source named vector (`value`) and a named vector of
#' replacements. It replaces the values in `value` where the names
#' match those in `replacement`, while keeping all other values and their
#' original order intact.
#'
#' @param value A named atomic vector (e.g., numeric, character, logical).
#'   This is the vector whose values will be modified. Must have names.
#' @param replacement A named atomic vector of the same type as `value` (or coercible).
#'   The names in this vector specify which elements in `value` to replacement,
#'   and its values are the new values to be assigned. Must have names.
#' @return A named vector with the updated values. The order and names of
#'   elements not specified in `replacement` remain unchanged from `value`.
#' @examples
#' # Example usage:
#' value <- c(a=2, b=3, d=5, foo=66)
#' replace <- c(foo=0, b=77)
#'
#' new_value <- replace_values(value, replace)
#' print(new_value)
#' # Expected output: a=2, b=77, d=5, foo=0
#'
#' # Example with some names in 'replacement' not present in 'value'
#' value2 <- c(x=10, y=20)
#' replacement2 <- c(y=50, z=100) # 'z' is not in src2
#' new_value2 <- replace_values(value2, replacement2)
#' print(new_value2)
#' # Expected output: x=10, y=50, with a warning about 'z'
#'
#' # Example with missing names in value or replacement (will throw an error)
#' # tryCatch(replace_values(c(1,2), c(a=3)), error = function(e) message(e$message))
#' # tryCatch(replace_values(c(a=1, b=2), c(3,4)), error = function(e) message(e$message))
#' @export
replace_values <- function(value, replacement) {
  # --- Input Validation ---

  # Check if value is named
  if (is.null(names(value))) {
    stop("Argument 'value' must be a named vector.")
  }

  # Check if replacement is named
  if (is.null(names(replacement))) {
    stop("Argument 'replacement' must be a named vector.")
  }

  # Check for unmatching names in 'replacement' that are not in 'value'
  replacement_names <- names(replacement)
  value_names <- names(value)

  unmatched_names <- setdiff(replacement_names, value_names)

  if (length(unmatched_names) > 0) {
    warning(
      "The following names in 'replacement' were not found in 'value' and were ignored: ",
      paste(unmatched_names, collapse = ", "), call. = FALSE
    )
  }

  # --- Core Logic ---

  # Replace the values in value where names match those in replacement
  # R's named indexing automatically handles alignment and ignores non-matching names
  value[replacement_names] <- replacement

  return(value)
}
