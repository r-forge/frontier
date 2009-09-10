# efficiencies of frontier models
efficiencies.frontier <- function( object, asInData = FALSE, ... ) {

   if( asInData ) {
      if( object$modelType == 1 && object$ineffDecrease &&
            object$logDepVar && object$nt == 1 ) {
         resid <- residuals( object, asInData = TRUE )
         sigmaSq <- coef( object )[ "sigmaSq" ]
         gamma <- coef( object )[ "gamma" ]
         lambda <- sqrt( gamma / ( 1 - gamma ) )
         if( object$truncNorm ) {
            mu <- coef( object )[ "mu" ]
         } else {
            mu <- 0
         }
         muStar <- - gamma * resid + mu * ( 1 - gamma )
         sigmaStarSq <- sigmaSq * gamma * ( 1 - gamma )
         sigmaStar <- sqrt( sigmaStarSq )
         result <- ( ( 1 - pnorm( sigmaStar - muStar / sigmaStar ) ) /
            ( 1 - pnorm( - muStar / sigmaStar ) ) ) *
            exp( - muStar + 0.5 * sigmaStarSq )
      } else {
         data <- eval( object$call$data )
         if( "plm.dim" %in% class( data ) ) {
            result <- rep( NA, nrow( data ) )
            for( i in 1:nrow( data ) ) {
               if( ncol( object$effic ) == 1 ) {
                  result[ i ] <- object$effic[ data[[ 1 ]][ i ], 1 ]
               } else {
                  result[ i ] <- object$effic[ data[[ 1 ]][ i ], data[[ 2 ]][ i ] ]
               }
            }
         } else {
            result <- drop( object$effic )
         }
      }


      names( result ) <- rownames( object$dataTable )
   } else {
      result <- object$effic
   }
   return( result )
}
