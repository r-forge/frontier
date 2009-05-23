frontierR = function(data, modelType, mu, evalLogLik=FALSE, gridDouble, gridno,
                     iterlim, startVal=NULL, code="R", verbose=FALSE) {
    
    nx=ncol(data$x);
    if (mu) {
        data$z <- cbind(rep(1,length(data$y)),data$z);
    }
    
    ols <- frontierOlsEstimation(data);
    if (verbose) {
        cat(paste("OLS Loglike:",frontierOlsLogLike(ols$param,data),"\n"));
        print(ols);
    }
    
    grid <- frontierGridEstimation(ols$param, data, gridDouble, gridno);
    if (verbose) {
        cat(paste("\nGamma gridding   LogLike:", frontierLogLike(grid,data), 
                "yCorrelation: ",frontierYCorrelation(grid,data),"\n"));
        print(grid);
    }
     
    if (!is.null(startVal) && length(startVal)>1) {
        startParam <- relist(startVal,skeleton=grid);
    } else {
        startParam <- grid;
    }
          
    if (evalLogLik) {
        logLike <- frontierLogLike(startParam, data);
    } else {
        mle <- frontierMle(startParam, data, iterlim = iterlim);
        if (verbose) {
            cat(paste("\nGamma fitting  - LogLike:",frontierLogLike(mle$param,data), 
                  "yCorrelation: ",frontierYCorrelation(mle$param,data)));
            print(mle);
        }
    }
        
    if (modelType==1 && mu) {
        parOrder <- c(1,3,4,2);
        grid <- grid[parOrder];
        if (!evalLogLik) {
            mle$param <- mle$param[parOrder];
            vParOrder <- c(1:nx,nx+c(2,3,1));
            mle$cov <- mle$cov[vParOrder,vParOrder];
        }
    }
    
    returnObj <- list(
      olsParam   = as.vector(unlist(ols$param)),
      olsStdEr   = ols$stdEr,
      olsLogl = frontierOlsLogLike(ols$param, data),
      gridParam   = as.vector(unlist(grid)),
      gridLogl = frontierLogLike(grid, data)
      )
    
    if (evalLogLik) {
        if (code=="R") {
            returnObj$logLike <- frontierLogLike(startParam, data);
        } else {
            returnObj$logLike <- frontierLogLikeF41(startParam, data);
        }
        returnObj$effic <- frontierEfficiency(startParam, data)
    } else {
        returnObj$mleParam <- as.vector(unlist(mle$param))
        returnObj$mleCov <- mle$cov
        returnObj$mleLogl <- frontierLogLike(mle$param,data)
        returnObj$nIter <- mle$nIter
        returnObj$effic <- frontierEfficiency(mle$param, data)
    }
    
    
    return( returnObj );
}