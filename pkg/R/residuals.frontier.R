# residuals of frontier models
residuals.frontier <- function( object, asInData = FALSE, ... ) {

   if( asInData ) {
      data <- eval( object$call$data )
      if( "plm.dim" %in% class( data ) ) {
         result <- rep( NA, nrow( data ) )
         for( i in 1:nrow( data ) ) {
            result[ i ] <- object$resid[ data[[ 1 ]][ i ], data[[ 2 ]][ i ] ]
         }
      } else {
         result <- drop( object$resid )
      }
      names( result ) <- rownames( object$dataTable )
   } else {
      result <- object$resid
   }
   return( result )
}
