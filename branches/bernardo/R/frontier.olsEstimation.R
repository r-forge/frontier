frontier.olsEstimation = function(data) {

    xtx_1   <- solve( t(data$x) %*% data$x );
    beta    <- c(xtx_1 %*% t(data$x) %*% data$y);
    resid   <- data$y - data$x %*% beta;
    sigmaSq <- sum(resid*resid) / (nrow(data$x) - ncol(data$x));
        
    names(beta)    <- colnames(data$x);
    names(sigmaSq) <- "sigmaSq";
    
    return(list(param   = list(beta=beta,sigmaSq=sigmaSq),
                stdEr   = sqrt(sigmaSq*diag(xtx_1)) ));
}