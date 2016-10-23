base.model <- function(parameters){
    
    cmat <- circulant(parameters)
    
    g <- function(t, yini, parameters){
        with(as.list(c(parameters, yini)),{
            
            N <- matrix(yini, ncol = 1)
            
            dN <- N * (1 - cmat %*% N)
            
            list(c(dN))
        })
    }
    return(g)
}