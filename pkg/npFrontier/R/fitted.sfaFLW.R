fitted.sfaFLW <- function( object, which = "frontier", ... ) {
  if( which == "frontier" ) {
    fit <- fitted( object$npreg ) + object$mu
  } else if( which == "first" ) {
    fit <- fitted( object$npreg )
  } else {
    stop( "argument 'which' must be either \"first\" or \"frontier\"" )
  }
  return( fit )
}
