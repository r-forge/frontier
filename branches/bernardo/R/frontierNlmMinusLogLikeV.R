frontierNlmMinusLogLikeV = function(unLimVParam, data, vParam0, minVParam, maxVParam,
                                      adjustableVParam) {
    # Receive the parameters as a vector of unlimited values, transforms them to
    # the limited values and write it as list, so logLike can be called
    
    vParam <- vParam0;
    vParam[adjustableVParam] <- frontierNlmLimParam(unLimVParam, minVParam, maxVParam);
    param <- relist(vParam);
    return( -frontierLogLike(param, data) );
}


