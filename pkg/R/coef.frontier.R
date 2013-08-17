coef.frontier <- function( object, which = "mle", extraPar = FALSE, ... ) {

   if( length( extraPar ) != 1 || !is.logical( extraPar[1] ) ) {
      stop( "argument 'extraPar' must be a single logical value" )
   }

   if( tolower( which ) == "start" ){
      result <- object$startVal
   } else if( tolower( which ) == "ols" ) {
      result <- object$olsParam
   } else if( tolower( which ) == "grid" ) {
      result <- object$gridParam
   } else if( tolower( which ) == "mle" ) {
      result <- object$mleParam
   } else {
      stop( "argument 'which' must be either 'start', 'ols', 'grid',",
         " or 'mle'" )
   }
   
   if( extraPar ) {
      if( tolower( which ) == "ols" ) {
         warning( "extra parameters are not available for coefficients",
            " obtained by OLS" )
      } else {
         result <- c( result, 
            sigmaSqU = unname( result[ "sigmaSq" ] * result[ "gamma"] ),
            sigmaSqV = unname( result[ "sigmaSq" ] * ( 1 - result[ "gamma"] ) ) )
      }
   }
   
   return( result )
}