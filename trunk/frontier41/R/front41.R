system( "R CMD SHLIB front41.f" )
dyn.load( "./front41.so" )

frontierEst <- function( insFile ) {
   returnMsg <- .Fortran( "front41", 
      kins = as.character( insFile ) )
   return( returnMsg )
}

frontierEst( "eg1-ins.txt" )
