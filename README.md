# salmoRodeo

## Re-Implementation of the Classical SALMO Model

Status: work in progress (alpha)

The current implementation employs the [Gujer-Petersen Matrix](https://en.wikipedia.org/wiki/Petersen_matrix) notation and the **R** package [**rodeo**](https://cran.r-project.org/package=rodeo) to generate fast Fortran code, that is then solved as initial value problem using the [**deSolve**](https://CRAN.R-project.org/package=deSolve) package in **R**.


The current version implements a single layer, which could be useful for shallow lakes. It could also serve as a starting point for a 1D version coupled to a hydrophysical driver or for further work on the biogeochemical process descriptions.


## Installation and Getting started

1. Install the prerequisites

Install a recent version of [**R**](https:\\cran.r-project.org), e.g. 4.4.x or newer and the development toolchain (compilers), e.g. [**RTools**](https://cran.r-project.org/bin/windows/Rtools/) on Windows or the **r-base-dev** package on Linux/Ubuntu (`sudo apt-get install r-base-dev`).

2. Install the package:

```
install.packages("remotes")
remotes::install_github("tpetzoldt/salmoRodeo")
```

3. Read the [Get started](https://tpetzoldt.github.io/salmoRodeo/articles/salmoRodeo.html)-vignette.

4. Perform some simulations.

5. Give feedback to the package authors.

