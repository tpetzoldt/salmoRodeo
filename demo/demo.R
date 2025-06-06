## tested with rodeo 0.8.6, 2024-12-04
library("salmoRodeo")
library("deSolve")

## setup and compile model
model <- model_setup() # use default input file of package in /models

## alternative: Model setup with inputs from a local directory
## assuming that it is the working directory
#model <- model_setup(datafile="demo_inputs.tsv")

## set time, parameters and initial conditions
times <- seq(1, 700, 1)

## important: xsum must be sum of x1 + x2 + x3
model <- set_initval(model, c(p=50, n=3, x1=0.1, x2=0.1, x3=0.1, z=0.1, d=1, xsum=0.3))
model <- set_pars(model)

## optional: create  a clone with altered parameter values
## note: set_pars is cumulative
model2<- model$clone()
model2 <- set_pars(model2, c(npsfmode = 0, MOMIN = 0.03, MOT=0.012))
## test
model2$getPars()[c("MOMIN", "MOT")]


## run model
out <- model$dynamics(times = times, diagnostics = TRUE, atol = 1e-8,
                      rtol = 1e-2, method = "adams", fortran = TRUE)


out2 <- model2$dynamics(times = times, diagnostics = TRUE, atol = 1e-8,
                      rtol = 1e-2, method = "adams", fortran = TRUE)


plot(out, out2, which=c("p", "n", "x1", "x2", "x3", "z", "d", "xsum", "pin"))

## diagnostics
diagnostics(out)
