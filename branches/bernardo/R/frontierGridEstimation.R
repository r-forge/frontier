frontierGridEstimation = function(olsParam,  data, igrid2, gridno) {
    
    param <- list(beta = olsParam$beta, delta = rep(0,ncol(data$z)),
                  sigmaSq=0, gamma=0);
    names(param$delta) <- colnames(data$z)
    
    K <- length(olsParam$beta);
    T <- length(data$y);
    logProbMax <- -Inf;
    gamma <- gridno;
    maxGamma <- 1;
    while (igrid2>=0) {
      while (gamma+gridno/1000 < maxGamma) {
          param$gamma   <- gamma;
          param$sigmaSq <- olsParam$sigmaSq * pi * (T-K)/T/(pi-2*gamma);
          param$beta[1] <- olsParam$beta[1] + sqrt(2*gamma*param$sigmaSq/pi);
          lp <- frontierLogLike(param,data);
          if (!is.nan(lp) && lp>logProbMax) {
              logProbMax <- lp;
              paramMax <- param;
          }
          gamma <- gamma + gridno;
      }
      gamma <- paramMax$gamma - gridno/2;
      maxGamma <- min(paramMax$gamma + gridno/2,1);
      gridno <- gridno/10;
      igrid2 <- igrid2 - 1;
    }
    
  return(paramMax);
}