logLik.frontier <- function( object, which = "mle", ... ) {

   if( tolower( which ) == "ols" ) {
      result <- object$olsLogl
   } else if( tolower( which ) == "grid" ) {
      result <- object$gridLogl
   } else if( tolower( which ) == "start" ) {
      result <- object$startLogl
   } else if( tolower( which ) == "mle" ) {
      result <- object$mleLogl
   } else {
      stop( "argument 'which' must be either 'ols', 'grid', 'start', or 'mle'" )
   }
   if( is.null( result ) ) {
      result <- NA
   }
   return( result )
}