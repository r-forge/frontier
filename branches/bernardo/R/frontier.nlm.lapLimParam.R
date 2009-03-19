#Laplacian: d limParam / d unlimParam
frontier.nlm.lapLimParam = function(x, min, max) {
    y <- ifelse(min == -Inf & max == Inf,
           1, 
           ifelse(max==Inf,
                  1/(2+x*x/2-x*sqrt(1+x*x/4)),
                  ifelse(min == -Inf,
                         -1/(2+x*x/2-x*sqrt(1+x*x/4)),
                         (1+x^2)^(-3/2)*(max-min)/2)));
     names(y) <- names(x);
     y;
}