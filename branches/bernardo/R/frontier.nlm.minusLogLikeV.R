frontier.nlm.minusLogLikeV = function(vParam) {
    # Receive the parameters as a vector of unlimited values, transforms them to
    # the limited values and write it as list, so logLike can be called
    
    v <- frontier.nlm.param0;
    #print(v[frontier.nlm.adjustableParam]);
    #print(vParam);
    #print(frontier.nlm.limParam(vParam));
    v[frontier.nlm.adjustableParam] <- frontier.nlm.limParam(vParam);
    nx <- ncol(frontier.nlm.data$x);
    param <- list(sigmaSq=v[1],gamma=v[2],beta=v[2+1:nx],                              
                 delta=v[2+nx+1:ncol(frontier.nlm.data$z)])
    return( -frontier.logLike(param,frontier.nlm.data) );
}


