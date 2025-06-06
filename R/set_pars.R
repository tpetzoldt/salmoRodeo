#' Set or change parameter values in a model object.
#'
#' This function updates the parameters of a model object. It accepts a named
#' vector of new parameters and uses the `replace_values` helper function to
#' merge these into the existing or default parameters, preserving other values
#' and their order.
#'
#' @param model A model object that has `getPars()` and `setPars()` methods
#'   (e.g., a `rodeo` model object).
#' @param new_pars A named vector of new parameter values to set. Its names
#'   should correspond to parameters in the model. If `NULL`, no parameters
#'   are changed beyond `default_pars`.
#' @param default_pars An optional named vector representing the default
#'   parameters. If not provided, `model$getPars()` will be used as the base
#'   onto which `new_pars` are applied.
#' @return The modified model object with updated parameters.
#' @seealso [replace_values()] [set_pars2()]
#'
#' @export
set_pars <- function(model, new_pars = NULL, default_pars = model$getPars()) {
  if (!is.null(new_pars)) {
    pars <- replace_values(default_pars, new_pars)
  } else {
    pars <- default_pars # If new_pars is NULL, just use default_pars
  }
  model$setPars(pars)
  model
}

