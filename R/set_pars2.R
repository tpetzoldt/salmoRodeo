#' Set or change parameter values in a model object.
#'
#' This function updates the parameters of a model object. You can provide
#' new parameter values directly as named arguments via the ellipsis (`...`).
#'
#' @param model A model object that has `getPars()` and `setPars()` methods.
#' @param default_pars An optional named vector representing the default
#'   parameters. If not provided, `model$getPars()` will be used.
#' @param ... Named arguments representing the new parameter values to set.
#'   These will override existing values in `default_pars`.
#' @return The modified model object with updated parameters.
#'
#' @details This function works similar to `set_pars`, but `name = value`-pairs
#'   can be given directly, without an enclosing combine `c()`.
#'
#' @seealso [replace_values()] [set_pars()]
#'
#' @export
set_pars2 <- function(model, default_pars = model$getPars(), ...) {
  # Capture all arguments passed via ellipsis as a named list
  new_pars_list <- list(...)

  # Convert the list to a named vector if necessary
  if (length(new_pars_list) > 0) {
    # Ensure it's a vector for replace_values, handling mixed types if needed
    # For parameters, they are often numeric, but can be character/logical
    # c() will coerce to a common type if possible.
    new_pars_vec <- unlist(new_pars_list)
  } else {
    new_pars_vec <- NULL # No new parameters provided
  }


  if (!is.null(new_pars_vec) && length(new_pars_vec) > 0) {
    pars <- replace_values(default_pars, new_pars_vec)
  } else {
    pars <- default_pars
  }

  model$setPars(pars)
  model
}
