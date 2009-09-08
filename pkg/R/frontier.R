frontier <- function(
      yName, xNames = NULL, zNames = NULL, data,
      ineffDecrease = TRUE,
      logDepVar = TRUE,
      truncNorm = FALSE,
      zIntercept = FALSE,
      timeEffect = FALSE,
      startVal = NULL,
      tol = 0.00001,
      maxit = 1000,
      muBound = 2,
      bignum = 1.0E+16,
      searchStep = 0.00001,
      searchTol = 0.001,
      searchScale = NA,
      gridSize = 0.1,
      gridDouble = TRUE,
      printIter = 0 ) {

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
      ineffDecrease = ineffDecrease,
      logDepVar = logDepVar,
      truncNorm = truncNorm,
      timeEffect = timeEffect,
      startVal = startVal,
      tol = tol,
      maxit = maxit,
      muBound = muBound,
      bignum = bignum,
      searchStep = searchStep,
      searchTol = searchTol,
      searchScale = searchScale,
      gridSize = gridSize,
      gridDouble = gridDouble,
      printIter = printIter )

   returnObj$zIntercept <- zIntercept
   returnObj$call <- match.call()
   return( returnObj )
}
