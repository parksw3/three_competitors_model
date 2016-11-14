## http://stackoverflow.com/questions/15795318/efficient-way-to-create-a-circulant-matrix-in-r
circulant <- function(parameters){
    with(parameters,{
        x <- c(1, matvals)
        n <- length(x)
        suppressWarnings(
            matrix(x[matrix(1:n,n+1,n+1,byrow=T)[c(1,n:2),1:n]],n,n)
        )
    })
}