# residuals of frontier models
residuals.frontier <- function( object, asInData = FALSE, ... ) {

   if( asInData ) {
      data <- eval( object$call$data )
      if( "plm.dim" %in% class( data ) ) {
         result <- rep( NA, nrow( data ) )
         names( result ) <- paste( data[[ 1 ]], data[[ 2 ]], sep = "_" )
         for( i in 1:nrow( data ) ) {
            result[ i ] <- object$resid[ data[[ 1 ]][ i ], data[[ 2 ]][ i ] ]
         }
      } else {
         result <- drop( object$resid )
      }
   } else {
      result <- object$resid
   }
   return( result )
}
