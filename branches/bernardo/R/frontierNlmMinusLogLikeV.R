frontierNlmMinusLogLikeV = function(unLimVParam, data, vParam0, minVParam, maxVParam,
                                      adjustableVParam) {
    # Receive the parameters as a vector of unlimited values, transforms them to
    # the limited values and write it as list, so logLike can be called
    
    vParam <- vParam0;
    vParam[adjustableParam] <- frontierNlmLimParam(unLimVParam, minVParam, maxVParam);
    param <- vector2list(vParam);
    return( -frontierLogLike(param, data) );
}


