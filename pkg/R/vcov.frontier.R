vcov.frontier <- function( object, extraPar = FALSE, ... ) {

   if( length( extraPar ) != 1 || !is.logical( extraPar[1] ) ) {
      stop( "argument 'extraPar' must be a single logical value" )
   }
   
   result <- object$mleCov
   
   if( extraPar ) {
      gamma <- coef( object )[ "gamma" ]
      sigmaSq <- coef( object )[ "sigmaSq" ]
      jacobian <- diag( nrow( result ) )
      jacobian <- rbind( jacobian, matrix( 0, nrow = 5, ncol = ncol( result ) ) )
      rownames( jacobian ) <- c( rownames( result ), "sigmaSqU", "sigmaSqV",
         "sigma", "sigmaU", "sigmaV" )
      colnames( jacobian ) <- colnames( result )
      jacobian[ "sigmaSqU", "sigmaSq" ] <- gamma
      jacobian[ "sigmaSqU", "gamma" ] <- sigmaSq
      jacobian[ "sigmaSqV", "sigmaSq" ] <- 1 - gamma
      jacobian[ "sigmaSqV", "gamma" ] <- - sigmaSq
      jacobian[ "sigma", "sigmaSq" ] <- 0.5 / sqrt( sigmaSq )
      jacobian[ "sigmaU", "sigmaSq" ] <- 0.5 * sqrt( gamma / sigmaSq )
      jacobian[ "sigmaU", "gamma" ] <- 0.5 * sqrt( sigmaSq / gamma )
      jacobian[ "sigmaV", "sigmaSq" ] <- 0.5 * sqrt( ( 1 - gamma ) / sigmaSq )
      jacobian[ "sigmaV", "gamma" ] <- - 0.5 * sqrt( sigmaSq / ( 1 - gamma ) )
      result <- jacobian %*% result %*% t( jacobian )
   }

   return( result )
}
