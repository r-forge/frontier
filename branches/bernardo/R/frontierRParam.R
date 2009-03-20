#It is very important to creat the list in the correct order, since they
#are coerced to a vector (were order matters) for several operation

frontierRParam <- function(beta, delta=NULL, sigmaSq, gamma=NULL) {
    return( list(beta=beta, delta=delta, sigmaSq=sigmaSq, gamma=gamma) );
}