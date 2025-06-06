#' Set or change initial values of the model states.
#'
#' This function updates the initial state variables of a `rodeo` model object.
#' It performs a check to ensure that the provided initial values vector
#' matches the defined state variables of the model in terms of names.
#'
#' @param model A `rodeo` model object.
#' @param init A named numeric vector representing the initial values for
#'   the model's state variables. The names of this vector must match the
#'   names of the model's state variables exactly.
#' @return The modified `rodeo` model object with updated initial values.
#'
#' @export
set_initval <- function(model, init) {
  nm_init <- names(init)
  nm_states <- names(model$getVars())
  if (!identical(sort(nm_init), sort(nm_states))) {
    stop("Initial values vector names do not match model state variable names.")
  }
  # Ensure the order of init matches the model's expected order
  model$setVars(init[nm_states])
  model
}
