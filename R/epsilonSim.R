library(deSolve)
source("model.R")
source("parameters.R")
source("circulant.R")
source("equilibrium.R")

y <- equilibrium(0.1)
model <- base.model(pars)
epsilon <- seq(0.01, 0.09, 0.01)

fn <- "epsilon.rda"

resList <- lapply(epsilon, function(x){
    yini <- y
    yini[3:4] <- yini[3:4] + c(-1, 1) * x
    r <- as.data.frame(rk(yini, tvec, model, pars))
    r$epsilon <- x
    return(r)
})

save("resList", file = fn)