# efficiencies of frontier models
efficiencies.frontier <- function( object, asInData = FALSE, ... ) {

   if( asInData ) {
      if( object$modelType == 1 ) {
         resid <- residuals( object, asInData = TRUE )
         if( object$timeEffect ) {
            eta <- coef( object )[ "time" ]
            etaStar <- exp( - eta * ( object$dataTable[ , 2 ] - object$nt ) )
         } else {
            eta <- 0
            etaStar <- rep( 1, object$nob ) 
         }
         if( object$nt == 1 ) {
            residStar <- resid
            tStar <- 1
         } else {
            residStar <- rep( NA, object$nob )
            tStar <- rep( NA, object$nob )
            for( i in 1:object$nob ) {
               residStar[ i ] <- sum( resid[ object$dataTable[ , 1 ] ==
                  object$dataTable[ i, 1 ] ] *
                  etaStar[ object$dataTable[ , 1 ] == object$dataTable[ i, 1 ] ] )
               tStar[ i ] <- sum( etaStar[ object$dataTable[ , 1 ] ==
                  object$dataTable[ i, 1 ] ]^2 )
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
         if( object$logDepVar ) {
            result <- ( pnorm( - dir * sigmaStar * etaStar + muStar / sigmaStar ) /
               pnorm( muStar / sigmaStar ) ) *
               exp( - dir * muStar * etaStar + 0.5 * sigmaStarSq * etaStar^2 )
         } else {
            fitted <- object$dataTable[ , 3 ] - resid
            fittedStar <- rep( NA, object$nob )
            tInd <- rep( NA, object$nob )
            for( i in 1:object$nob ) {
               fittedStar[ i ] <- sum( fitted[ object$dataTable[ , 1 ] ==
                  object$dataTable[ i, 1 ] ] )
               tInd[ i ] <- sum( object$dataTable[ , 1 ] ==
                  object$dataTable[ i, 1 ] )
            }
            result <- 1 - dir * etaStar * ( muStar + sigmaStar *
               exp( dnorm( muStar / sigmaStar, log = TRUE ) -
                  pnorm( muStar / sigmaStar, log = TRUE ) ) ) /
               ( fittedStar / tInd )
         }
         if( object$ineffDecrease ) {
            result[ result > 1 ] <- 1
         } else {
            result[ result < 1 ] <- 1
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
