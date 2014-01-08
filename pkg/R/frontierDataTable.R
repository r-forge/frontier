frontierDataTable <- function( data, formula, effFormula, mc, mf, mfe ) {

   # preparing model matrix and model response
   mt <- attr( mf, "terms" )
   xMat <- model.matrix( mt, mf )
   xNames <- colnames( xMat )
   yVec <- model.response( mf )
   yName <- as.character( formula )[ 2 ]
   if( length( yVec ) != nrow( xMat ) ) {
      return( paste( "the number of observations of the endogenous variable (",
         length( yVec ), ") is not equal to the number of observations",
         " of the exogenous variables (", nrow( xMat ), ")", sep = "" ) )
   }

   # cross section and time period identifier
   if( "plm.dim" %in% class( data ) ) {
      dataTable <- matrix( as.integer( data[[ 1 ]] ), ncol = 1 )
      dataTable <- cbind( dataTable, as.integer( data[[ 2 ]] ) )
   } else {
      dataTable <- matrix( 1:length( yVec ), ncol = 1 )
      dataTable <- cbind( dataTable, rep( 1, nrow( dataTable ) ) )
   }
   nb <- length( xNames )

   # endogenous variable
   dataTable <- cbind( dataTable, yVec )
   if( sum( !is.na( yVec ) & is.finite( yVec ) ) == 0 ) {
      return( "the dependent variable has no valid observations" )
   }

   # exogenous variables
   dataTable <- cbind( dataTable, xMat )
   paramNames <- NULL
   if( nb > 0 ) {
      for( i in 1:nb ) {
         paramNames <- c( paramNames, xNames[ i ] )
         if( sum( !is.na( xMat[ , i ] ) & is.finite( xMat[ , i ] ) ) == 0 ) {
            return( paste( "regressor '", xNames[ i ], "' has no valid observations",
               sep = "" ) )
         }
      }
   }

   # variables explaining the efficiency level
   if( is.null( effFormula  ) ) {
      zNames <- NULL
      zIntercept <- FALSE
   } else {
      mte <- attr( mfe, "terms" )
      zMat <- model.matrix( mte, mfe )
      if( ncol( zMat ) > 0 && colnames( zMat )[ 1 ] == "(Intercept)" ) {
         zIntercept <- TRUE
         zMat <- zMat[ , -1, drop = FALSE ]
      } else {
         zIntercept <- FALSE
      }
      if( nrow( zMat ) != nrow( xMat ) ) {
         return( paste( "the number of observations of the variables explaining",
            " efficiency (", nrow( zMat ), ") is not equal to the number",
            " of observations of the (regular) regressors (",
            nrow( xMat ), ")", sep = "" ) )
      }
      dataTable <- cbind( dataTable, zMat )
      zNames <- colnames( zMat )
      if( length( zNames ) > 0 ) {
         for( i in 1:length( zNames ) ) {
            if( sum( !is.na( zMat[ , i ] ) & is.finite( zMat[ , i ] ) ) == 0 ) {
               return( paste( "the regressor for the inefficiency term '", 
                  zNames[ i ], "' has no valid observations", sep = "" ) )
            }
         }
      }
   }
   nZvars <- length( zNames )

   # detect and remove observations with NAs, NaNs, and INFs
   validObs <- rowSums( is.na( dataTable ) | is.infinite( dataTable ) ) == 0
   dataTable <- dataTable[ validObs, ]
   # number of (valid) observations
   nob <- sum( validObs )
   
   # make sure that the cross-section units are numbered continously
   firmId <- sort( unique( dataTable[ , 1 ] ) )
   # number of cross-section units
   nn <- length( firmId )
   firmNo <- rep( NA, nrow( dataTable ) )
   for( i in 1:nn ) {
      firmNo[ dataTable[ , 1 ] == firmId[ i ] ] <- i
   }
   dataTable[ , 1 ] <- firmNo
   
   # check consistency of firm numbers
   if( any( is.na( dataTable[ , 1 ] ) ) ) {
      return( "internal error: at least one firm number is NA" )
   }
   if( min( dataTable[ , 1 ] ) != 1 ) {
      return( "internal error: the smallest firm number must be one" )
   }
   if( max( dataTable[ , 1 ] ) > nn ) {
      return( "internal error: a firm number is larger than the number of firms" )
   }
   
   # make sure that the time periods are numbered continously
   timeId <- sort( unique( dataTable[ , 2 ] ) )
   # number of time periods
   nt <- length( unique( dataTable[ , 2 ] ) )
   timeNo <- rep( NA, nrow( dataTable ) )
   for( i in 1:nt ) {
      timeNo[ dataTable[ , 2 ] == timeId[ i ] ] <- i
   }
   dataTable[ , 2 ] <- timeNo
   
   # check consistency of time period numbers
   if( any( is.na( dataTable[ , 2 ] ) ) ) {
      return( "internal error: at least one time period number is NA" )
   }
   if( min( dataTable[ , 2 ] ) != 1 ) {
      return( "internal error: the smallest time period number must be one" )
   }
   if( max( dataTable[ , 2 ] ) > nt ) {
      return( "internal error: a time period number is larger",
         " than the number of time periods" )
   }
   
   # check for double entries for firm/period combinations
   for( i in 1:nn ) {
      for( j in 1:nt ) {
         if( sum( dataTable[ , 1 ] == i & dataTable[ , 2 ] == j ) > 1 ){
            return( paste( "more than one observation for firm '", firmId[ i ],
               "' in period '", timeId[ j ], "'", sep = "" ) )
         }
      }
   }
   
   # adding column names to the data table
   colnames( dataTable ) <- c( "id", "t", yName, xNames, zNames )
   
   # obtaining names of the observations
   if( !is.null( rownames( data ) ) ) {
      obsNames <- rownames( data )
   } else if( !is.null( names( yVec ) ) ) {
      obsNames <- names( yVec )
   } else if( !is.null( rownames( xMat ) ) ) {
      obsNames <- rownames( xMat )
   } else if( !is.null( rownames( zMat ) ) ) {
      obsNames <- rownames( zMat )
   } else {
      obsNames <- NULL
   }
   rownames( dataTable ) <- obsNames[ validObs ]
   names( validObs ) <- obsNames
   
   returnObj <- list()
   returnObj$dataTable   <- dataTable
   returnObj$validObs    <- validObs
   returnObj$firmId      <- firmId
   returnObj$timeId      <- timeId
   returnObj$obsNames    <- obsNames
   returnObj$nb          <- nb
   returnObj$nob         <- nob
   returnObj$nn          <- nn
   returnObj$nt          <- nt
   returnObj$paramNames  <- paramNames
   returnObj$zNames      <- zNames
   returnObj$zIntercepts <- zIntercept
   returnObj$nZvars      <- nZvars
   
   return( returnObj )
}
