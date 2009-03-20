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
      olsParam   = list2vector(ols$param, attributes=FALSE),
      olsStdEr   = ols$stdEr,
      olsLogLike = frontier.olsLogLike(ols$param, data),
      gridParam   = list2vector(grid, attributes=FALSE),
      gridLogLike = frontier.logLike(grid, data),
      mleParam = list2vector(mle$param, attributes=FALSE),
      mleCov = mle$cov,
      mleLogLike = frontier.logLike(mle$param, data),
      nIter = mle$nIter,
      effic = frontier.efficiency(mle$param, data)
      ));
}