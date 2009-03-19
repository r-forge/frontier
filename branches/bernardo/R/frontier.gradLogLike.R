#Calculate the value of the function and its gradients
frontier.gradLogLike = function(param, data) {
    #print(vetorPar)
    s2 <- param$sigmaSq;
    g <- param$gamma;
    b <- param$beta;
    n <- length(data$y);
    #print(list(s2,g,b))
    e  <- data$y - data$x %*% b;
    zd <- if (ncol(data$z)>0)  data$z %*% param$delta else rep(0,n);
    d  <- zd/sqrt(g*s2);
    sS <- sqrt(g*(1-g)*s2); #sigma star
    dS <- ((1-g)*zd - g*e)/sS; #d star
    pnormd <- pnorm(d ,log.p=TRUE);
    pnormdS <- pnorm(dS,log.p=TRUE);
    dpd <- exp(dnorm(d ,log=TRUE)-pnormd);
    dpS <- exp(dnorm(dS,log=TRUE)-pnormdS);
    ezd <- (e+zd);
    sumezd2 <- sum(ezd*ezd);
    ezds2 <- ezd/s2;
    sdpdd <- sum(dpd*d);
    sdpSdS <- sum(dpS*dS);
    dpSs <- dpS/sqrt(g*s2);
    dLdsigmaS2 <- -1/2/s2*(n-sdpdd+sdpSdS-sumezd2/s2);
    dLdgamma   <- sdpdd/2/g-sum(dpS*ezd)/sS-sdpSdS*s2*(1-2*g)/2/sS/sS;
    dLdbeta    <- t(ezds2+dpS*(g/sS)) %*% data$x;
    dLddelta   <- - t(ezds2+dpd/sqrt(g*s2)-dpS*((1-g)/sS)) %*% data$z;
    dif <- -c(dLdsigmaS2,dLdgamma,dLdbeta,dLddelta);
    #return( dif );
    returnObj <- -n/2*log(pi*s2*2)-1/2/s2*sumezd2 - sum(pnormd) + sum(pnormdS);
    attr(returnObj,"gradient") <- dif;
    #print(returnObj);
    return(returnObj);
}