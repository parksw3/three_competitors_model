library(deSolve)
library(emdbook)
source("model.R")
source("parameters.R")
source("circulant.R")
source("period.R")

model <- base.model(pars)

tmpf <- function(fix.start = c(0.79, 0.25),
                 fix = c(1, 3),
                 start = c(log(0.16), log(0.64))){
    
    objfun <- function(par){
        yini <- rep(0, 4)
        yini[fix] <- fix.start
        yini[-fix] <- exp(par)
        r <- as.data.frame(rk(yini, tvec, model, pars))
        return(getPeriod(r[,2]))
    }
    
    return(optim(start, objfun))
}

profile <- function(x, y,
                    start = c(0.16, 0.64),
                    fix = c(1, 3)){
    optimres <- tmpf(fix.start = c(x,y), fix = fix, start = start)
    return(optimres$value)
}

fn <- "profile.rda"

profileres <- curve3d(profile(x, y),
    xlim = c(0.1, 0.9),
    ylim = c(0.15, 0.95),
    n = c(8,8)
)

save("profileres", file = fn)
