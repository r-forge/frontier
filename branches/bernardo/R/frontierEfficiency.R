frontierEfficiency = function(param, data) {
    e <- data$y - data$x %*% param$beta;
    zd <- if (ncol(data$z)>0)  data$z %*% param$delta else rep(0,length(e));
    sA <- sqrt(param$gamma*(1-param$gamma)*param$sigmaSq);
    miAsA <- ((1-param$gamma)*zd - param$gamma*e)/sA;
    return( exp((-miAsA+sA/2)*sA + pnorm(miAsA-sA,log.p=TRUE) 
                                 - pnorm(miAsA,log.p=TRUE)) );
}