system( "R CMD SHLIB front41.f" )
dyn.load( "./front41.so" )

frontierEst <- function(
      data, crossSectionName, timePeriodName = NULL,
      yName, xNames = NULL, zNames = NULL,
      modelType = ifelse( is.null( zNames ), 1, 2 ), 
      functionType = 1,
      logDepVar = TRUE,
      mu = FALSE,
      eta = FALSE,
      iprint = 0,
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
   if( modelType == 2 ) {
      eta <- nZvars
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

   nParamTotal <- nb + 3 + mu + eta
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
      modelType = as.integer( modelType ),
      functionType = as.integer( functionType ),
      logDepVar = as.integer( logDepVar ),
      nn = as.integer( nn ),
      nt = as.integer( nt ),
      nob = as.integer( nob ),
      nb = as.integer( nb ),
      mu = as.integer( mu ),
      eta = as.integer( eta ),
      iprint = as.integer( iprint ),
      indic = as.integer( indic ),
      tol = as.double( tol ),
      tol2 = as.double( tol2 ),
      bignum = as.double( bignum ),
      step1 = as.double( step1 ),
      igrid2 = as.integer( igrid2 ),
      gridno = as.double( gridno ),
      maxit = as.integer( maxit ),
      ite = as.integer( ite ),
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
      nIter = as.integer( 0 ),
      effic = matrix( as.double( 0 ), nn, nt ) )
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
      if( modelType == 1 ) {
         returnObj$gridParam <- returnObj$gridParam[ 1:( nb + 3 ) ]
      } else {
         returnObj$gridParam <- returnObj$gridParam[
            c( 1:( nb + 1 ), ( nParamTotal - 1 ):nParamTotal ) ]
      }
   } else {
      returnObj$gridParam <- NULL
   }
   if( modelType == 1 && eta == FALSE ) {
      returnObj$effic <- returnObj$effic[ , 1, drop = FALSE ]
   }
   return( returnObj )
}

library( micEcon )
data( Coelli )
Coelli$logOutput  <- log( Coelli$output )
Coelli$logCapital <- log( Coelli$capital )
Coelli$logLabour  <- log( Coelli$labour )

a1 <- frontierEst( Coelli, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ) )

a2 <- frontierEst( Coelli, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ), mu = TRUE )

a3 <- frontierEst( Coelli, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ), eta = TRUE )

a4 <- frontierEst( Coelli, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ), mu = TRUE, eta = TRUE )


data( riceProdPhil )
riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
riceProdPhil$lNPK   <- log( riceProdPhil$NPK )

b1 <- frontierEst( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )

b2 <- frontierEst( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   mu = TRUE )

b3 <- frontierEst( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   eta = TRUE )

b4 <- frontierEst( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   mu = TRUE, eta = TRUE )

b5 <- frontierEst( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ) )

b6 <- frontierEst( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), mu = TRUE )
