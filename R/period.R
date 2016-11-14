getPeriod <- function(N,
                      cutoff = 10){
    Ncut <- N[tvec > cutoff]
    cut <- (max(Ncut) + min(Ncut))/2
    
    cv1 <- Ncut <= cut
    cv2 <- Ncut >= cut
    cv3 <- cv1[-length(Ncut)]*cv2[-1]
    if(sum(cv3) < 2){
        cv3 <- cv1[-1]*cv2[-length(Ncut)]
    }
    diff(tvec[which(cv3 == 1)[1:2]])
}