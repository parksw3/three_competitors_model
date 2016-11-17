library(deSolve)
source("model.R")
source("parameters.R")
source("circulant.R")
source("period.R")
source("lhs_y.R")

model <- base.model(pars)

tmpf <- function(yini){
    r <- as.data.frame(rk(yini, tvec, model, pars))
    return(getPeriod(r[,2]))
}

sims <- 20

y <- lhs_y(range = c(0.1, 1), n.state = 4, n.sim = sims, seed = 101)
resList <- vector("list", sims)

fn <- "optimize.rda"

for(i in 1:sims){
    cat(i)
    res <- optim(y[i,], tmpf)
    resList[[i]] <- res
}

save("resList", file = fn)
