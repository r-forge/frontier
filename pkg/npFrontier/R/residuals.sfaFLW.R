residuals.sfaFLW <- function( object, which = "final", ... ) {
  if( which == "final" ) {
    resid <- object$e - object$mu
  } else if( which == "first" ) {
    resid <- object$e
  } else {
    stop( "argument 'which' must be either \"first\" or \"final\"" )
  }
  return( resid )
}
