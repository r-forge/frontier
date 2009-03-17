frontier.yEstimation = function(param, data) {
    
    s <- sqrt(param$gamma * param$sigmaSq);
    zd <- if (ncol(data$z)>0)  data$z %*% param$delta else rep(0,length(data$y));
    
    return( data$x %*% param$beta - zd - s * exp(dnorm(zd/s,log=TRUE) -
            pnorm(zd/s,log.p=TRUE)))
}