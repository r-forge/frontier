#Reverts limParam
frontier.nlm.unLimParam = function(x, 
          min = frontier.nlm.minParam[frontier.nlm.adjustableParam],
          max = frontier.nlm.maxParam[frontier.nlm.adjustableParam]) {
    
    y <- ifelse(min == -Inf & max == Inf,
            x,
            ifelse(max==Inf,
                x-min-1/(x-min),
                ifelse(min == -Inf,
                    max-x-1/(max-x),
                    (2*(x-min)/(max-min)-1)/sqrt(1-(2*(x-min)/(max-min)-1)^2))));
     
     names(y) <- names(x);
     
     return(y)
}