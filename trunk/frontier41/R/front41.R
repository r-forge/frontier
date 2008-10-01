system( "R CMD SHLIB front41.f" )
dyn.load( "./front41.so" )

frontierEst <- function( koutf,
      data, crossSectionName, timePeriodName = NULL,
      yName, xNames = NULL, zNames = NULL,
      im = ifelse( is.null( zNames ), 1, 2 ), 
      ipc = 1,
      il = TRUE,
      nmu = FALSE,
      neta = FALSE,
      iprint = 5,
      indic = 1,
      tol = 0.00001,
      tol2 = 0.001,
      bignum = 1.0E+16,
      step1 = 0.00001,
      igrid2 = 1,
      gridno = 0.1,
      maxit = 100,
      ite = 1,
      startVal = NULL ) {

   nn <- length( unique( data[[ crossSectionName ]] ) )
   nt <- ifelse( is.null( timePeriodName ), 1,
      length( unique( data[[ timePeriodName ]] ) ) )
   nob <- nrow( data )
   nb <- length( xNames )
   nZvars <- length( zNames )
   if( im == 2 ) {
      neta <- nZvars
   }

   dataTable <- matrix( data[[ crossSectionName ]], ncol = 1 )

   # time period identifier
   if( is.null( timePeriodName ) ) {
      dataTable <- cbind( dataTable, rep( 1, nrow( dataTable ) ) )
   } else {
      dataTable <- cbind( dataTable, data[[ timePeriodName ]] )
   }

   # endogenous variable
   dataTable <- cbind( dataTable, data[[ yName ]] )

   # exogenous variables
   if( nb > 0 ) {
      for( i in 1:nb ) {
         dataTable <- cbind( dataTable, data[[ xNames[ i ] ]] )
      }
   }

   # variables explaining the efficiency level
   if( nZvars > 0 ) {
      for( i in 1:nZvars ) {
         dataTable <- cbind( dataTable, data[[ zNames[ i ] ]] )
      }
   }

   if( is.null( startVal ) ) {
      startVal <- 0
   } else {
      nStartVal <- nb + 3 + nmu + neta
      if( nStartVal != length( startVal ) ) {
         stop( "wrong number of starting values (you provided ",
            length( startVal ), " starting values but the model has ",
            nStartVal, " parameters)" )
      }
   }
   returnObj <- .Fortran( "front41", 
      koutfArg = as.character( koutf ),
      imArg = as.integer( im ),
      ipcArg = as.integer( ipc ),
      ilArg = as.integer( il ),
      nnArg = as.integer( nn ),
      ntArg = as.integer( nt ),
      nobArg = as.integer( nob ),
      nbArg = as.integer( nb ),
      nmuArg = as.integer( nmu ),
      netaArg = as.integer( neta ),
      iprintArg = as.integer( iprint ),
      indicArg = as.integer( indic ),
      tolArg = as.double( tol ),
      tol2Arg = as.double( tol2 ),
      bignumArg = as.double( bignum ),
      step1Arg = as.double( step1 ),
      igrid2Arg = as.integer( igrid2 ),
      gridnoArg = as.double( gridno ),
      maxitArg = as.integer( maxit ),
      iteArg = as.integer( ite ),
      nStartVal = as.integer( length( startVal ) ),
      startVal = as.double( startVal ),
      nRowData = as.integer( nrow( dataTable ) ),
      nColData = as.integer( ncol( dataTable ) ),
      dataTable = as.double( dataTable ) )
   names( returnObj ) <- sub( "Arg$", "", names( returnObj ) )
   return( returnObj )
}

   library( micEcon )
   data( Coelli )
   Coelli$logOutput  <- log( Coelli$output )
   Coelli$logCapital <- log( Coelli$capital )
   Coelli$logLabour  <- log( Coelli$labour )

   a <- frontierEst( "eg1-out.txt", Coelli, "firm", "time", "logOutput",
      c( "logCapital", "logLabour" ) )
