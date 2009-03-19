#Maps the unlimited argument to limited values
frontier.nlm.limParam = function(x, min, max) {
                                
    y <- ifelse(min == -Inf & max == Inf,
            x,
            ifelse(max == Inf,
                min + 1/(sqrt(1+x*x/4)-x/2),
                ifelse(min == -Inf,
                    max - 1/(sqrt(1+x*x/4)-x/2),
                    min+(1+x/sqrt(1+x*x))*(max-min)/2)));
    names(y) <- names(x);
    return(y);
}

