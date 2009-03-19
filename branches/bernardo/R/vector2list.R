vector2list <- function(vec, lengths = attr(vec,"lengths"),
                             namesElem = attr(vec,"namesElem") ) {
    lst <- list();
    i0 <- c(0,cumsum(lengths));
    for(i in 1:length(lengths)) {
        if (lengths[i]>0) {
            lst[[i]] <- vec[ i0[i] + (1:lengths[i]) ] ;
            names(lst[[i]]) <- namesElem[[i]];
        } else
            lst[[i]] <- NULL;
        
    }
    names(lst) <- names(namesElem);
    return(lst);
}