equilibrium <- function(x){
    c(x, (1-2*x)/2, x, (1-2*x)/2)
}

eq_jaco <- function(x, pars){
    y <- equilibrium(x)
    -matrix(rep(y, each = 4), 4, 4) * circulant(pars)
}