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
   returnObj <- .Fortran( "front41", 
      kdatfArg = as.character( kdatf ),
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
      iteArg = as.integer( ite ) )
   names( returnObj ) <- sub( "Arg$", "", names( returnObj ) )
   return( returnObj )
}

a <- frontierEst( "eg1-dta.txt", "eg1-out.txt", nn = 60, nt = 1, nob = 60, nb = 2 )
