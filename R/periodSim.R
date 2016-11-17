library(plyr)
source("model.R")
source("parameters.R")
source("circulant.R")
source("lhs_y.R")

model <- base.model(pars)

y <- lhs_y(range = c(0.1, 1), n.state = 4, n.sim = 1000, seed = 101)

fn <- "period.rda"

LHSres <- apply(y, 1, function(yini){
    r <- as.data.frame(rk(yini, tvec, model, pars))
    r <- setNames(r, c("time", "N1", "N2", "N3", "N4"))
})

save("LHSres", file = fn)
