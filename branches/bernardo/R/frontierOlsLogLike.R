frontierOlsLogLike = function(param, data) {
    
    resid <- data$y - data$x %*% param$beta;
    sigma <- sqrt(sum(resid*resid)/(length(resid)));
    sum(dnorm(resid,sd=sigma,log=TRUE));

}