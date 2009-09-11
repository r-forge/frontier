# efficiencies of frontier models
efficiencies.frontier <- function( object, asInData = FALSE, ... ) {

   if( asInData ) {
      if( object$modelType == 1 && object$logDepVar && !object$timeEffect ) {
         resid <- residuals( object, asInData = TRUE )
         if( object$nt == 1 ) {
            residStar <- resid
            tStar <- 1
         } else {
            residStar <- rep( NA, object$nn )
            tStar <- rep( NA, object$nn )
            for( i in 1:object$nn ) {
               residStar[ i ] <- sum( resid[ object$dataTable[ , 1 ] == i ] )
               tStar[ i ] <- sum( object$dataTable[ , 1 ] == i )
            }
         }
         sigmaSq <- coef( object )[ "sigmaSq" ]
         gamma <- coef( object )[ "gamma" ]
         lambda <- sqrt( gamma / ( 1 - gamma ) )
         if( object$truncNorm ) {
            mu <- coef( object )[ "mu" ]
         } else {
            mu <- 0
         }
         if( object$ineffDecrease ) {
            dir <- 1
         } else {
            dir <- -1
         }
         muStar <- ( - dir * gamma * residStar + mu * ( 1 - gamma ) ) /
            ( 1 + ( tStar - 1 ) * gamma )
         sigmaStarSq <- sigmaSq * gamma * ( 1 - gamma ) /
            ( 1 + ( tStar - 1 ) * gamma )
         sigmaStar <- sqrt( sigmaStarSq )
         effic <- ( pnorm( - dir * sigmaStar + muStar / sigmaStar ) /
            pnorm( muStar / sigmaStar ) ) *
            exp( - dir * muStar + 0.5 * sigmaStarSq )
         if( object$nt == 1 ) {
            result <- effic
         } else {
            result <- rep( NA, nrow( object$dataTable ) )
            for( i in 1:length( result ) ) {
               result[ i ] <- effic[ object$dataTable[ i, 1 ] ]
            }
         }
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
