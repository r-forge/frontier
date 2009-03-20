frontierR = function(data, igrid2, gridno, iterlim, verbose=FALSE) {
    
    ols <- frontierOlsEstimation(data);
    if (verbose) {
        cat(paste("OLS Loglike:",frontierOlsLogLike(ols$param,data),"\n"));
        print(ols);
    }
    
    grid = frontierGridEstimation(ols$param, data, igrid2, gridno);
    if (verbose) {
        cat(paste("\nGamma gridding   LogLike:", frontierLogLike(grid,data), 
                "yCorrelation: ",frontierYCorrelation(grid,data),"\n"));
        print(grid);
    }
    
    mle = frontierMle(grid, data, iterlim = iterlim);
    if (verbose) {
        cat(paste("\nGamma fitting  - LogLike:",frontierLogLike(mle$param,data), 
               "yCorrelation: ",frontierYCorrelation(mle$param,data),"\n"));
        print(mle);
    }
    
    return( list(
      olsParam   = list2vector(ols$param, attributes=FALSE),
      olsStdEr   = ols$stdEr,
      olsLogLike = frontierOlsLogLike(ols$param, data),
      gridParam   = list2vector(grid, attributes=FALSE),
      gridLogLike = frontierLogLike(grid, data),
      mleParam = list2vector(mle$param, attributes=FALSE),
      mleCov = mle$cov,
      mleLogLike = frontierLogLike(mle$param, data),
      nIter = mle$nIter,
      effic = frontierEfficiency(mle$param, data)
      ));
}