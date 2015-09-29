residuals.sfaFLW <- function( object, which = "final", ... ) {
  if( which == "final" ) {
    resid <- residuals( object$npreg ) - object$mu
  } else if( which == "first" ) {
    resid <- residuals( object$npreg )
  } else {
    stop( "argument 'which' must be either \"first\" or \"final\"" )
  }
  return( resid )
}
