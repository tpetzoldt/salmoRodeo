% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_initval.R
\name{set_initval}
\alias{set_initval}
\title{Set or change initial values of the model states.}
\usage{
set_initval(model, init)
}
\arguments{
\item{model}{A \code{rodeo} model object.}

\item{init}{A named numeric vector representing the initial values for
the model's state variables. The names of this vector must match the
names of the model's state variables exactly.}
}
\value{
The modified \code{rodeo} model object with updated initial values.
}
\description{
This function updates the initial state variables of a \code{rodeo} model object.
It performs a check to ensure that the provided initial values vector
matches the defined state variables of the model in terms of names.
}
