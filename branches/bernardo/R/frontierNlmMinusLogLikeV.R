frontierNlmMinusLogLikeV = function(vParam, data, param0, minParam, maxParam,
                                      adjustableParam) {
    # Receive the parameters as a vector of unlimited values, transforms them to
    # the limited values and write it as list, so logLike can be called
    
    v <- param0;
    v[adjustableParam] <- frontierNlmLimParam(vParam, minParam, maxParam);
    #nx <- ncol(data$x);
    param <- vector2list(v);
    return( -frontierLogLike(param, data) );
}


