library(deSolve)
source("model.R")
source("functions.R")

alpha <- seq(0, 2, 0.02)
beta <- 1.5

nsim <- 20

y <- seq(0, 2, length.out =  nsim)

tvec <- seq(0, 400, 1)

set.seed(101)
lhs_y <- replicate(3,sample(y))

fn <- "bifurcation.rda"

resList <- vector("list", length(alpha))

for(i in 1:length(alpha)){
    cat(i)
    pars <- list(
        matvals = c(beta, alpha[i])
    )
    model <- base.model(pars)
    res <- apply(lhs_y, 1, function(y){
        r <- as.data.frame(rk(y, tvec, model, pars))
        r[,"alpha"] <- alpha[i]
        r <- setNames(r, c("time", "N1", "N2", "N3", "alpha"))
    })
    resList[[i]] <- do.call("rbind", res)
}

save("resList", file = fn)
