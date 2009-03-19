frontier.nlm.minusLogLikeV = function(vParam, data, param0, minParam, maxParam,
                                      adjustableParam) {
    # Receive the parameters as a vector of unlimited values, transforms them to
    # the limited values and write it as list, so logLike can be called
    
    v <- param0;
    #print(v[frontier.nlm.adjustableParam]);
    #print(vParam);
    #print(frontier.nlm.limParam(vParam));
    v[adjustableParam] <- frontier.nlm.limParam(vParam, minParam, maxParam);
    nx <- ncol(data$x);
    #param <- list(sigmaSq=v[1], gamma=v[2], beta=v[2+1:nx],                              
    #             delta=v[2+nx+1:ncol(data$z)])
    param <- vector2list(v);
    return( -frontier.logLike(param, data) );
}


