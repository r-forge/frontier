# residuals of frontier models
residuals.frontier <- function( object, asInData = FALSE, ... ) {

   if( asInData ) {
      result <- drop( object$dataTable[ , 3 ] -
         cbind( rep( 1, nrow( object$dataTable ) ),
            object$dataTable[ , 4:( object$nb + 3 ) ] ) %*%
         coef( object )[ 1:( object$nb + 1 ) ] )
      names( result ) <- rownames( object$dataTable )
   } else {
      result <- object$resid
   }
   return( result )
}
