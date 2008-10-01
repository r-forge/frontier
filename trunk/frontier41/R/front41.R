system( "R CMD SHLIB front41.f" )
dyn.load( "./front41.so" )

frontierEst <- function( insFile, 
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
   returnObj <- .Fortran( "front41", kins = as.character( insFile ),
      iprintArg = as.integer( iprint ),
      indicArg = as.integer( indic ),
      tolArg = as.double( tol ),
      tol2Arg = as.double( tol2 ),
      bignumArg = as.double( bignum ),
      step1Arg = as.double( step1 ),
      igrid2Arg = as.integer( igrid2 ),
      gridnoArg = as.double( gridno ),
      maxitArg = as.integer( maxit ),
      iteArg = as.integer( ite ) )
   names( returnObj ) <- sub( "Arg$", "", names( returnObj ) )
   return( returnObj )
}

frontierEst( "eg1-ins.txt" )
