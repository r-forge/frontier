#LogLike as calculated by frontier 4.1
frontierLogLikeF41 = function(param, data) {


    e = data$y - data$x %*% param$beta;
    n=length(e)
    zd = if (ncol(data$z)>0)  data$z %*% param$delta else rep(0,n)
    d  = zd/sqrt(param$gamma*param$sigmaSq);
    sS = sqrt(param$gamma*(1-param$gamma)*param$sigmaSq);
    dS = (1-param$gamma)/sS*zd - param$gamma/sS*e;
    bignum = 1e16;
    lbg = log(bignum);
    pnormd = pnorm(d,log.p=TRUE);
    pnormdS = pnorm(dS,log.p=TRUE);
    #The following lines implement the approximations used in Frontier4.1
    pnormd = ifelse(pnormd < -lbg,-lbg, pnormd);
    pnormd = ifelse(pnormd > 1-1/bignum, 1-1/bignum, pnormd); 
    pnomrdS = ifelse(pnormdS < -lbg,-lbg,pnormdS);
    pnormdS = ifelse(pnormdS > 1-1/bignum, 1-1/bignum, pnormdS); 
    return(- n/2*log(pi*param$sigmaSq*2) - 1/2/param$sigmaSq*sum((e+zd)^2) -
            sum(pnormd)+sum(pnormdS));
}