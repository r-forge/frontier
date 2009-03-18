frontier.frontierR = function(data, igrid2, gridno, iterlim, verbose=FALSE) {
    
    ols <- frontier.olsEstimation(data);
    if (verbose) {
        cat(paste("OLS Loglike:",frontier.olsLogLike(ols$param,data),"\n"));
        print(ols);
    }
    
    grid = frontier.gridEstimation(ols$param, data, igrid2, gridno);
    if (verbose) {
        cat(paste("\nGamma gridding   LogLike:", frontier.logLike(grid,data), 
                "yCorrelation: ",frontier.yCorrelation(grid,data),"\n"));
        print(grid);
    }
    
    mle = frontier.mle(grid, data, iterlim = iterlim);
    if (verbose) {
        cat(paste("\nGamma fitting  - LogLike:",frontier.logLike(mle$param,data), 
               "yCorrelation: ",frontier.yCorrelation(mle$param,data),"\n"));
        print(mle);
    }
    
    return( list(
      olsParam   = c(ols$param$beta,ols$param$sigmaSq),
      olsStdEr   = ols$stdEr,
      olsLogLike = frontier.olsLogLike(ols$param, data),
      gridParam   = c(grid$beta, grid$sigmaSq, grid$gamma),
      gridLogLike = frontier.logLike(grid, data),
      mleParam = c(beta=mle$param$beta, delta=mle$param$delta,
                   sigmaSq=mle$param$sigmaSq, gamma=mle$param$gamma),
      mleCov = mle$cov,
      mleLogLike = frontier.logLike(mle$param, data),
      nIter = mle$nIter,
      effic = frontier.efficiency(mle$param, data)
      ));
}