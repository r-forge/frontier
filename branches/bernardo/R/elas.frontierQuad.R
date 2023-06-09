elas.frontierQuad <- function( object, data = NULL, dataLogged = TRUE,
      yObs = FALSE, ... ) {

   if( is.null( data ) ) {
      data <- eval( object$call$data )
   }

   if( !is.logical( dataLogged ) || length( dataLogged ) != 1 ) {
      stop( "argument 'dataLogged' must be a single logical value" )
   }

   if( yObs ) {
      yName <- object$yName
   } else {
      yName <- NULL
   }

   xNames <- eval( object$call$xNames )

   if( is.null( object$call$quadHalf ) ) {
      quadHalf <- TRUE
   } else {
      quadHalf <- object$call$quadHalf
   }

   nExog <- length( xNames )
   nCoef <- 1 + nExog + nExog * ( nExog + 1 ) / 2

   if( dataLogged ) {
      # translog function
      if( !object$logDepVar ) {
         stop( "a translog function must have a logged dependent variable",
            " but the model was estimated with argument 'logDepVar'",
            " equal to 'FALSE'" )
      }
      result <- translogEla( xNames = xNames, data = data,
         coef = coef( object )[ 1:nCoef ],
         coefCov = vcov( object )[ 1:nCoef, 1:nCoef ],
         quadHalf = quadHalf, dataLogged = TRUE )
   } else {
      # quadratic function
      if( object$logDepVar ) {
         stop( "a quadratic function must NOT have a logged dependent variable",
            " but the model was estimated with argument 'logDepVar'",
            " equal to 'TRUE'" )
      }
      stop( "sorry, the calculation of elasticities of a quadratic function",
         " is not implemented yet" )
   }

   return( result )
}