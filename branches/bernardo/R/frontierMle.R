frontierMle = function(startParam, data, iterlim=100) {
    
    # The function frontierMinusLogLikeV will be called by the nonlinear
    #   minimization routine. 
    # The function frontierMinusLogLikeV recieves a vector with the values 
    #   of the parameters and return the minuslogLike. 
    # Beyond the parameters, several data are necessary to the evaluation
    param0 <- list2vector(startParam);
                              
    # The adjustableParam is not being used, but may be usefull in the future.
    adjustableParam <- c(rep(TRUE,ncol(data$x)), rep(TRUE,ncol(data$z)),
                         TRUE, TRUE);
    
    #The minimum and maximum values allowed for the parameters
    #The limits on the parameters is imposed through the function limParam and
    #unLimPara, which maps the the real numbers in a limited interval and back.
    minParam <- c(rep(-Inf,ncol(data$x)+ncol(data$z)),0,0);
    maxParam <- c(rep(Inf,ncol(data$x)+ncol(data$z)),Inf,1);
    
    mle <- nlm(frontierNlmMinusLogLikeV,
       frontierNlmUnLimParam(param0[adjustableParam],minParam,maxParam),
       data,param0,minParam,maxParam,adjustableParam,
        iterlim=iterlim,hessian=TRUE,check.analyticals = TRUE);
    paramV <- param0;
    paramV[adjustableParam] <- 
          frontierNlmLimParam(mle$estimate,minParam,maxParam)
    param <- vector2list(paramV)
    
    n <- length(param0);
    hessian <- matrix(0,n,n);
    hessian[rep(adjustableParam,n) & rep(adjustableParam,each=n)] <- mle$hessian;
    lap <- rep(1,n);
    lap[adjustableParam] <-
                frontierNlmLapLimParam(mle$estimate,minParam,maxParam);
    hessian <- hessian / rep(lap,n) / rep(lap,each=n);
    rownames(hessian) <- colnames(hessian) <- names(param0);
    
    return( list(param = param,
                 cov = solve(hessian),
                 nIter = mle$iterations));
}
