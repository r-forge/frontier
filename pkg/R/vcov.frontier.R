vcov.frontier <- function( object, extraPar = FALSE, ... ) {

   if( length( extraPar ) != 1 || !is.logical( extraPar[1] ) ) {
      stop( "argument 'extraPar' must be a single logical value" )
   }
   
   result <- object$mleCov
   
   if( extraPar ) {
      jacobian <- diag( nrow( result ) )
      jacobian <- rbind( jacobian, matrix( 0, nrow = 2, ncol = ncol( result ) ) )
      rownames( jacobian ) <- c( rownames( result ), "sigmaSqU", "sigmaSqV" )
      colnames( jacobian ) <- colnames( result )
      jacobian[ "sigmaSqU", "sigmaSq" ] <- coef( object )[ "gamma" ]
      jacobian[ "sigmaSqU", "gamma" ] <- coef( object )[ "sigmaSq" ]
      jacobian[ "sigmaSqV", "sigmaSq" ] <- 1 - coef( object )[ "gamma" ]
      jacobian[ "sigmaSqV", "gamma" ] <- - coef( object )[ "sigmaSq" ]
      result <- jacobian %*% result %*% t( jacobian )
   }

   return( result )
}
