frontierMle = function(startParam, data, iterlim=100, startVal=startVal) {
    
    # The function frontierMinusLogLikeV will be called by the nonlinear
    #   minimization routine. 
    # The function frontierMinusLogLikeV recieves a vector with the values 
    #   of the parameters and return the minuslogLike. 
    # Beyond the parameters, several data are necessary to the evaluation
    vParam0 <- unlist(as.relistable(startParam));
    if (ncol(data$z)==0) {
        attr(vParam0,"skeleton")$delta <- NULL
    }
    if (!is.null(startVal) && length(startVal)>1) {
        vParam0[1:length(vParam0)] = startVal;
    }
    
    
                              
    # The adjustableParam is not being used, but may be usefull in the future.
    adjustableVParam <- unlist(as.relistable(frontierRParam(beta=rep(TRUE,ncol(data$x)),
            delta=rep(TRUE,ncol(data$z)), sigmaSq=TRUE, gamma=TRUE)));
    
    #The minimum and maximum values allowed for the parameters
    #The limits on the parameters is imposed through the function limParam and
    #unLimPara, which maps the the real numbers in a limited interval and back.
    minVParam <- unlist(as.relistable(frontierRParam(beta=rep(-Inf,ncol(data$x)),
            delta=rep(-Inf,ncol(data$z)), sigmaSq=0, gamma=0)));
    maxVParam <- unlist(as.relistable(frontierRParam(beta=rep(Inf,ncol(data$x)),
            delta=rep(Inf,ncol(data$z)), sigmaSq=Inf, gamma=1)));
    
    
    mle <- nlm(frontierNlmMinusGradLogLikeV,
       frontierNlmUnLimParam(vParam0[adjustableVParam],minVParam,maxVParam),
       data,vParam0,minVParam,maxVParam,adjustableVParam,
        iterlim=iterlim,hessian=TRUE,check.analyticals = TRUE);
    vParam <- vParam0;
    vParam[adjustableVParam] <- 
          frontierNlmLimParam(mle$estimate,minVParam,maxVParam)
    param <- relist(vParam)
    
    n <- length(vParam0);
    hessian <- matrix(0,n,n);
    hessian[rep(adjustableVParam,n) & rep(adjustableVParam,each=n)] <- mle$hessian;
    lap <- rep(1,n);
    lap[adjustableVParam] <-
                frontierNlmLapLimParam(mle$estimate,minVParam,maxVParam);
    hessian <- hessian / rep(lap,n) / rep(lap,each=n);
    rownames(hessian) <- colnames(hessian) <- names(vParam0);
    
    return( list(param = param,
                 cov = solve(hessian),
                 nIter = mle$iterations));
}
