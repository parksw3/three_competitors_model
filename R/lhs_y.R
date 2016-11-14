lhs_y <- function(range = c(0.1, 2),
                  n.state = 3,
                  n.sim = 50,
                  seed = NULL){
    if(!is.null(seed)) set.seed(seed)
    
    y <- seq(range[1], range[2], length.out =  n.sim)
    lhs_y <- replicate(n.state,sample(y))
}