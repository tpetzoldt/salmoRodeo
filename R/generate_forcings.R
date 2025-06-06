#' Generate forcing function specifications for a model.
#'
#' This function creates a data frame suitable for defining forcing functions
#' in a `rodeo` model. It specifies the names of the forcing functions,
#' the corresponding column names in the input data file, and other metadata.
#'
#' @param datafile The name of the data file that contains the forcing data.
#' @param columns A character vector specifying the names of the columns
#'   in the `datafile` that correspond to the forcing functions. These
#'   names will also be used as the function names.
#' @return A data frame with columns `name`, `column`, `file`, `mode`, and
#'   `default`, structured for `rodeo::forcingFunctions`.
#' @importFrom rodeo forcingFunctions
#' @examples
#' # Assuming `rodeo::forcingFunctions` is available and correctly setup
#' # Example: Create a dummy data file
#' cat("stagnation\tVE\n",
#'     "1\t10\n",
#'     "2\t20\n",
#'     file = "input_data.tsv")
#'
#' # Generate forcing function specifications
#' forcings_spec <- generate_forcings(
#'   datafile = "input_data.tsv",
#'   columns = c("stagnation", "VE")
#' )
#' print(forcings_spec)
#' # Note: The actual `forcingFunctions` call within the function
#' # will generate Fortran/C code based on this data frame.
#' # For this example, we just show the data frame output.
#'
#' # Clean up dummy file
#' unlink("input_data.tsv")
#' @export
generate_forcings <- function(datafile, columns) {
  dat <- data.frame(
    name = columns, # forc function names
    column = columns, # col names
    file = datafile,
    mode = -1,
    default = FALSE)

  # This line assumes rodeo::forcingFunctions is globally available or imported
  # It generates code, which is then written to a file in `model_setup`.
  rodeo::forcingFunctions(dat) # generates code
}
