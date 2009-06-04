frontierR = function(data, modelType, mu, gridDouble, gridSize,
                     iterlim, startVal=NULL, code="R", verbose=FALSE) {
    
    nx=ncol(data$x);
    if (mu) {
        data$z <- cbind(rep(1,length(data$y)),data$z);
    }
    nz <- ncol( data$z )

    ols <- frontierOlsEstimation(data);
    if (verbose) {
        cat(paste("OLS Loglike:",frontierOlsLogLike(ols$param,data),"\n"));
        print(ols);
    }
    
    if( is.null( startVal ) || length( startVal ) <= 1 ) {
        grid <- frontierGridEstimation(ols$param, data, gridDouble, gridSize);
        startLogl <- frontierLogLike( grid, data )
        if (verbose) {
            cat(paste("\nGamma gridding   LogLike:", startLogl,
                  "yCorrelation: ",frontierYCorrelation(grid,data),"\n"));
            print(grid);
        }
        startParam <- grid;
    } else {
        grid <- NULL
        startParam <- list()
        startParam$beta <- startVal[ 1:nx ]
        if( modelType == 1 ) {
            if( mu ) {
                startParam$delta <- startVal[ nx + 3 ]
            }
            startParam$sigmaSq <- startVal[ nx + 1 ]
            startParam$gamma <- startVal[ nx + 2 ]
        } else if( modelType == 2 ) {
            if( nz > 0 ) {
                startParam$delta <- startVal[ ( nx + 1 ):( nx + nz ) ]
            }
            startParam$sigmaSq <- startVal[ length( startVal ) - 1 ]
            startParam$gamma <- startVal[ length( startVal ) ]
        } else {
            stop( "unknown modelType '", modelType, "'" )
        }
        startLogl <- frontierLogLike( startParam, data )
    }
          
    mle <- frontierMle(startParam, data, iterlim = iterlim);
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
    }
    
    returnObj <- list(
      olsParam   = as.vector(unlist(ols$param)),
      olsStdEr   = ols$stdEr,
      olsLogl = frontierOlsLogLike(ols$param, data),
      gridParam   = as.vector(unlist(grid)),
      startLogl = startLogl
      )
    
    returnObj$mleParam <- as.vector(unlist(mle$param))
    returnObj$mleCov <- mle$cov
    returnObj$mleLogl <- frontierLogLike(mle$param,data)
    returnObj$nIter <- mle$nIter
    returnObj$effic <- frontierEfficiency(mle$param, data)
    
    
    return( returnObj );
}