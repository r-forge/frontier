frontierNlmMinusGradLogLikeV = function(unLimVParam, 
              data, vParam0, minVParam, maxVParam, adjustableVParam) {
    v  <- vParam0;
    v[adjustableVParam] <- frontierNlmLimParam(unLimVParam, minVParam, maxVParam);
    param <- relist(v)
    g = -frontierGradLogLike(param,data);
    attr(g,"gradient") = - attr(g,"gradient") *
               frontierNlmLapLimParam(unLimVParam, minVParam, maxVParam);
    return( g );
}

  