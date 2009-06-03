frontierLogLike = function(param, data) {
    #The order of the mathematical operation have a strong effect on performance
    
    e  <- data$y - data$x %*% param$beta;
    n  <- length(e)
    zd <- if (ncol(data$z)>0)  data$z %*% param$delta else rep(0,n)
    d  <- zd / sqrt(param$gamma*param$sigmaSq);
    sS <- sqrt(param$gamma*(1-param$gamma)*param$sigmaSq);
    dS <- (1-param$gamma)/sS*zd - param$gamma/sS*e;
    return( as.numeric( - n/2*log(pi*param$sigmaSq*2) - 1/2/param$sigmaSq*sum((e+zd)^2) -
        sum(pnorm(d,log.p=TRUE))+sum(pnorm(dS,log.p=TRUE))));
        
}