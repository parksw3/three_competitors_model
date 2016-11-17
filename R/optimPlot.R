library(deSolve)
source("model.R")
source("circulant.R")
load("../data/optimize.rda")

pars <- list(matvals = c(0.3, 1, 1.7))
model <- base.model(pars)
tvec <- seq(0, 50, 0.05)

sL <- lapply(resList, function(x){
    sumvec <- c(x$par, x$value)
    names(sumvec) <- c("N1", "N2", "N3", "N4", "period")
    return(sumvec)
})

sL <- do.call("rbind", sL)

plot(NA, xlim = c(0.2, 0.3), ylim = c(0.2, 0.3))

apply(sL, 1, function(x){
    r <- as.data.frame(rk(x[-5], tvec, model, pars))
    r2 <- r[tvec > 10,]
    lines(r2[,2], r2[,3])
})
