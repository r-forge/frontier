frontier <- function(
      yName, xNames = NULL, zNames = NULL, data,
      zIntercept = FALSE,
      ... ) {

   # check names of variables
   checkNames( c( yName, xNames ), names( data ) )
   if( !is.null( zNames ) ) {
      if( !is.na( zNames[1] ) ) {
         checkNames( c( zNames ), names( data ) )
      }
   }
   if( any( c( "id", "t" ) %in% c( yName, xNames, zNames ) ) ) {
      stop( "variables in arguments 'yName', 'xNames', and 'zNames'",
         " must not have names 'id' or 't'" )
   }

   # zIntercept (mu)
   if( !is.logical( zIntercept ) ) {
      stop( "argument 'zIntercept' must be logical" )
   }
   if( zIntercept &&  is.null( zNames ) ) {
      warning( "argument 'zIntercept' is ignored in",
         " Efficiency Components Frontiers (ECF)" )
   }

   # formula for the SFA
   sfaFormula <- as.formula( paste( yName, "~",
      paste( xNames, collapse = " + " ) ) )

   # formula for efficiency effects
   if( is.null( zNames ) ) {
      effFormula <- NULL
   } else {
      if( is.na( zNames[1] ) ) {
         if( zIntercept ) {
            effFormula <- ~ 1
         } else {
            effFormula <- ~ - 1
         }
      } else {
         if( zIntercept ) {
            effFormula <- as.formula( paste( "~",
               paste( zNames, collapse = " + " ) ) )
         } else {
            effFormula <- as.formula( paste( "~",
               paste( zNames, collapse = " + " ), "- 1" ) )
         }
      }
   }

   returnObj <- sfa(
      formula = sfaFormula,
      effFormula = effFormula,
      data = data,
      ... )

   returnObj$zIntercept <- zIntercept
   returnObj$call <- match.call()
   return( returnObj )
}
