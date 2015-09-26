fitted.sfaFLW <- function( object, which = "frontier", ... ) {
  if( which == "frontier" ) {
    fit <- object$mhat
  } else if( which == "first" ) {
    fit <- object$mhat - object$mu
  } else {
    stop( "argument 'which' must be either \"first\" or \"frontier\"" )
  }
  return( fit )
}
