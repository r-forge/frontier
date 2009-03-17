frontier.yCorrelation = function(param, data) {
    
    #Correlation between the y estimation and the data
    n <- length(data$y);
    if (is.null(param$rho) || param$rho==0) 
        s <- diag(1,n)
    else 
        s <- diag(1,n) - param$rho * front.w;
    y1 <- frontier.yEstimation(param,data);
    
    return(cor(s%*%data$y,y1));
}