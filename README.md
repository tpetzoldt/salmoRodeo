# salmoRodeo
Re-Implementation of the Classical SALMO Model in Gujer-Petersen Matrix Notation

The current version implements a single layer, which could be useful for shallow lakes. It could also serve as a starting point for a 1D version coupled to a hydrophysical driver or for further work on the biogeochemical process descriptions.

It uses the R package [**rodeo**](https://cran.r-project.org/package=rodeo) to generate fast Fortran code, that can then be solved as initial value problem using the [**deSolve**](https://CRAN.R-project.org/package=deSolve) package in R.

Status: work in progress (alpha)
