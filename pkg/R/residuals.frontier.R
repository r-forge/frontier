# residuals of frontier models
residuals.frontier <- function( object, asInData = FALSE, ... ) {

   resid <- drop( object$dataTable[ , 3 ] -
      cbind( rep( 1, nrow( object$dataTable ) ),
         object$dataTable[ , 4:( object$nb + 3 ) ] ) %*%
      coef( object )[ 1:( object$nb + 1 ) ] )
   if( asInData ) {
      result <- resid
      names( result ) <- rownames( object$dataTable )
   } else {
      result <- matrix( NA, nrow = object$nn, ncol = object$nt )
      if( length( resid ) != nrow( object$dataTable ) ) {
         stop( "internal error: length of residuals is not equal to",
            " the number of rows of the data table" )
      }
      for( i in 1:length( resid ) ) {
         result[ object$dataTable[ i, 1 ], object$dataTable[ i, 2 ] ] <-
            resid[ i ]
      }
      dimnames( result ) <- dimnames( object$resid )
   }
   return( result )
}
