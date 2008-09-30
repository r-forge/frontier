system( "R CMD SHLIB front41.f" )
dyn.load( "./front41.so" )

frontierEst <- function( kdatf, koutf,
      im = 1,
      ipc = 1,
      il = TRUE,
      nn,
      nt,
      nob,
      nb,
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
      ite = 1 ) {
   intStart <- c( 
      iprint = as.integer( iprint ),
      indic = as.integer( indic ),
      igrid2 = as.integer( igrid2 ),
      maxit = as.integer( maxit ),
      ite = as.integer( ite ),
      im = as.integer( im ),
      ipc = as.integer( ipc ),
      il = as.integer( il ),
      nn = as.integer( nn ),
      nt = as.integer( nt ),
      nob = as.integer( nob ),
      nb = as.integer( nb ),
      nmu = as.integer( nmu ),
      neta = as.integer( neta ) )
   doubleStart <- c(
      tol = as.double( tol ),
      tol2 = as.double( tol2 ),
      bignum = as.double( bignum ),
      step1 = as.double( step1 ),
      gridno = as.double( gridno ) )
   returnObj <- .Fortran( "front41", 
      kdatfArg = as.character( kdatf ),
      koutfArg = as.character( koutf ),
      intStart = intStart, doubleStart = doubleStart )
   returnObj$arg <- c(
      as.list( returnObj$intStart ),
      as.list( returnObj$doubleStart ) )
   returnObj$intStart <- NULL
   returnObj$doubleStart <- NULL
   return( returnObj )
}

a <- frontierEst( "eg1-dta.txt", "eg1-out2.txt", nn = 60, nt = 1, nob = 60, nb = 2 )

