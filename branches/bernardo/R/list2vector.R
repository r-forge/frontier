list2vector <- function(lst, attributes=TRUE) {
    vec <- c(lst, recursive=TRUE)
    if (attributes) {
        lengths <- c(lapply(lst,length),recursive=TRUE);
        namesElem <- lapply(lst,names);
        names(namesElem) <- names(lst);
        attr(vec,"lengths") <- lengths;
        attr(vec,"namesElem") <- namesElem;
    }
    return(vec);
}