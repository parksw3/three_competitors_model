library(deSolve)
source("model.R")
source("functions.R")

pars1 <- list(
    matvals = c(0.5, 1, 1.5)
)

yini1 <- list(0.3, 0.2, 0.2, 0.3)
model1 <- base.model(pars1)
tvec <- seq(0, 100, 0.1)

r <- ode(unlist(yini1), tvec, model1, pars1)