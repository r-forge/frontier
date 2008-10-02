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

   nParamTotal <- nb + 3 + nmu + neta
   if( is.null( startVal ) ) {
      startVal <- 0
   } else {
      if( nParamTotal != length( startVal ) ) {
         stop( "wrong number of starting values (you provided ",
            length( startVal ), " starting values but the model has ",
            nParamTotal, " parameters)" )
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
      dataTable = matrix( as.double( dataTable ), nrow( dataTable ),
         ncol( dataTable ) ),
      nParamTotal = as.integer( nParamTotal ),
      olsParam = as.double( rep( 0, nParamTotal ) ),
      olsStdEr = as.double( rep( 0, nParamTotal ) ),
      olsLogl = as.double( 0 ),
      gridParam = as.double( rep( 0, nParamTotal ) ),
      mleParam = as.double( rep( 0, nParamTotal ) ),
      mleCov = matrix( as.double( 0 ), nParamTotal, nParamTotal ),
      mleLogl = as.double( 0 ),
      lrTestVal = as.double( 0 ),
      lrTestDf = as.integer( 0 ),
      nIter = as.integer( 0 ) )
   names( returnObj ) <- sub( "Arg$", "", names( returnObj ) )
   returnObj$nStartVal <- NULL
   returnObj$nRowData <- NULL
   returnObj$nColData <- NULL
   returnObj$nParamTotal <- NULL
   if( length( startVal ) == 1 ){
      returnObj$startVal <- NULL
   }
   returnObj$olsParam <- returnObj$olsParam[ 1:( nb + 2 ) ]
   returnObj$olsStdEr <- returnObj$olsStdEr[ 1:( nb + 1 ) ]
   if( length( startVal ) == 1 ){
      if( im == 1 ) {
         returnObj$gridParam <- returnObj$gridParam[ 1:( nb + 3 ) ]
      } else {
         returnObj$gridParam <- returnObj$gridParam[
            c( 1:( nb + 1 ), ( nParamTotal - 1 ):nParamTotal ) ]
      }
   } else {
      returnObj$gridParam <- NULL
   }
   return( returnObj )
}

library( micEcon )
data( Coelli )
Coelli$logOutput  <- log( Coelli$output )
Coelli$logCapital <- log( Coelli$capital )
Coelli$logLabour  <- log( Coelli$labour )

a1 <- frontierEst( "eg1-out.txt", Coelli, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ) )

a2 <- frontierEst( "eg1-out2.txt", Coelli, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ), nmu = TRUE )

a3 <- frontierEst( "eg1-out3.txt", Coelli, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ), neta = TRUE )

a4 <- frontierEst( "eg1-out4.txt", Coelli, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ), nmu = TRUE, neta = TRUE )


data( riceProdPhil )
riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
riceProdPhil$lNPK   <- log( riceProdPhil$NPK )

b1 <- frontierEst( "rice_1.out", riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )

b2 <- frontierEst( "rice_2.out", riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   nmu = TRUE )

b3 <- frontierEst( "rice_3.out", riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   neta = TRUE )

b4 <- frontierEst( "rice_4.out", riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   nmu = TRUE, neta = TRUE )

b5 <- frontierEst( "rice_5.out", riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ) )

b6 <- frontierEst( "rice_6.out", riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), nmu = TRUE )
