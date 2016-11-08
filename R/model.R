base.model <- function(parameters){
    
    trans.mat <- circulant(parameters)
    
    g <- function(t, yini, parameters){
        
        N <- matrix(yini, ncol = 1)
        dN <- N * (1 - trans.mat %*% N)
        
        list(c(dN))
    }
    return(g)
}