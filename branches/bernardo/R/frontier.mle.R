frontier.mle = function(startParam, data, iterlim=100) {
    
    # The function frontier.minusLogLikeV will be called by the nonlinear
    #   minimization routine. 
    # The function frontier.minusLogLikeV recieves a vector with the values 
    #   of the parameters and return the minuslogLike. 
    # Beyond the parameters, several data are necessary to the evaluation,
    #   these data will be stored as global variables frontier.nlm.
    # The functions that use these data will also start with frontier.nlm
    frontier.nlm.data <<- data;
    
    frontier.nlm.param0 <<- c(startParam$sigmaSq,startParam$gamma, 
                               startParam$beta, startParam$delta);
                              
    # The adjustableParam is not being used, but may be usefull in the future.
    frontier.nlm.adjustableParam <<- c(TRUE, TRUE, rep(TRUE,ncol(data$x)), 
                                       rep(TRUE,ncol(data$z)));
    
    #The minimum and maximum values allowed for the parameters
    #The limits on the parameters is imposed through the function limParam and
    #unLimPara, which maps the the real numbers in a limited interval and back.
    frontier.nlm.minParam <<- c(0,0,rep(-Inf,ncol(data$x)+ncol(data$z)));
    frontier.nlm.maxParam <<- c(Inf,1,rep(Inf,ncol(data$x)+ncol(data$z)));
    
    mle <- nlm(frontier.nlm.minusLogLikeV, 
       frontier.nlm.unLimParam(frontier.nlm.param0[frontier.nlm.adjustableParam]), 
        iterlim=iterlim,hessian=TRUE);
    
    paramV <- frontier.nlm.limParam(mle$estimate)
    param  <- list(sigmaSq=paramV[1],gamma=paramV[2],
                    beta=paramV[2+1:ncol(data$x)],
                    delta = if (ncol(data$z)>0) 
                                paramV[2+ncol(data$x)+1:ncol(data$z)] 
                            else 
                                numeric(0)  )
    
    n <- length(frontier.nlm.param0);
    hessian <- matrix(0,n,n);
    hessian[ rep(frontier.nlm.adjustableParam,n) 
           & rep(frontier.nlm.adjustableParam,each=n)] <- mle$hessian;
    lap <- rep(1,n);
    lap[frontier.nlm.adjustableParam] <- frontier.nlm.lapLimParam(mle$estimate);
    hessian <- hessian / rep(lap,n) / rep(lap,each=n);
    rownames(hessian) <- colnames(hessian) <- names(frontier.nlm.param0);
    
    #front.p <<- front.param0;
    #front.p[front.adjParam] <<- front.limParam(mle$estimate)
    #n=length(front.p);
    #front.hessian <<- matrix(0,n,n);
    #front.hessian[rep(front.adjParam,n) & rep(front.adjParam,each=n)] <<- mle$hessian;
    #lap=rep(1,n);
    #lap[front.adjParam]=front.lapLimParam(mle$estimate);
    #front.hessian <<- front.hessian / rep(lap,n) / rep(lap,each=n);
    #rownames(front.hessian) <<- colnames(front.hessian) <<- names(front.p);
    #front.param<<-list(sigmaS2=front.p[1],gamma=front.p[2],rho=front.p[3],
    #            beta=front.p[3+1:ncol(data$x)],
    #     delta = if (ncol(data$z)>0) front.p[3+ncol(data$x)+1:ncol(data$z)] else numeric(0));
    #front.param;
    return( list(param = param,
                 cov = solve(hessian),
                 nIter = mle$iterations));
}
