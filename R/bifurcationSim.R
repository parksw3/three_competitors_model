library(deSolve)
source("model.R")
source("circulant.R")
source("lhs_y.R")

alpha <- seq(0, 2, 0.01)
beta <- 1.5
tvec <- seq(0, 200, 0.1)

y <- c(0.8, 0.2, 0.3)

fn <- "bifurcation.rda"

resList <- vector("list", length(alpha))

for(i in 1:length(alpha)){
    cat(i)
    pars <- list(
        matvals = c(beta, alpha[i])
    )
    model <- base.model(pars)
    
    r <- as.data.frame(rk(y, tvec, model, pars))
    r[,"alpha"] <- alpha[i]
    r <- setNames(r, c("time", "N1", "N2", "N3", "alpha"))
    resList[[i]] <- r
}

save("resList", file = fn)
