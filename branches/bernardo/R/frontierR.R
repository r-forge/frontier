frontierR = function(data, modelType, mu, igrid2, gridno, iterlim,
                     startVal, verbose=FALSE) {
    
    nx=ncol(data$x);
    if (mu) {
        data$z <- cbind(rep(1,length(data$y)),data$z);
    }
    
    ols <- frontierOlsEstimation(data);
    if (verbose) {
        cat(paste("OLS Loglike:",frontierOlsLogLike(ols$param,data),"\n"));
        print(ols);
    }
    
    grid <- frontierGridEstimation(ols$param, data, igrid2, gridno);
    if (verbose) {
        cat(paste("\nGamma gridding   LogLike:", frontierLogLike(grid,data), 
                "yCorrelation: ",frontierYCorrelation(grid,data),"\n"));
        print(grid);
    }
      
      
    mle <- frontierMle(grid, data, iterlim = iterlim, startVal = startVal);
    if (verbose) {
        cat(paste("\nGamma fitting  - LogLike:",frontierLogLike(mle$param,data), 
               "yCorrelation: ",frontierYCorrelation(mle$param,data)));
        print(mle);
    }
    
    if (modelType==1 && mu) {
        parOrder <- c(1,3,4,2);
        grid <- grid[parOrder];
        mle$param <- mle$param[parOrder];
        vParOrder <- c(1:nx,nx+c(2,3,1));
        mle$cov <- mle$cov[vParOrder,vParOrder];
        rparOrder <- parOrder;
    }
    
    return( list(
      olsParam   = as.vector(unlist(ols$param)),
      olsStdEr   = ols$stdEr,
      olsLogl = frontierOlsLogLike(ols$param, data),
      gridParam   = as.vector(unlist(grid)),
      gridLogl = frontierLogLike(grid, data),
      mleParam = as.vector(unlist(mle$param)),
      mleCov = mle$cov,
      mleLogl = frontierLogLike(mle$param,data),
      nIter = mle$nIter,
      effic = frontierEfficiency(mle$param, data)
      ));
}