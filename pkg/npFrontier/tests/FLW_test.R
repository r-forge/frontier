## Code to test function sfaFLW()

library( "npFrontier" )

# load data
data( front41Data, package = "frontier" )

# function for printing objects returned by sfaFLW():
printFLW <- function( x ) {
  for( i in 1:length( x ) ) {
    cat( "$", names( x )[ i ], "\n", sep = "" )
    print( round( x[[ i ]], 2 ) )
    cat( "\n" )
  }
  invisible( x )
}


# semiparametric Cobb-Douglas production frontiers
# local-constant regression, ls.cv bandwidths
FLW_Result <- sfaFLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ) )

printFLW( FLW_Result )

# local-linear regression, ls.cv bandwidths
FLW_Result_ll <- sfaFLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ),
  regtype = "ll" )

printFLW( FLW_Result_ll )

# local-constant regression, rule-of-thumb bandwidths
FLW_Result_rot <- sfaFLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ),
  bw.sel = "rot"  )

printFLW( FLW_Result_rot )

# local-linear regression with rule-of-thumb bandwidths
FLW_Result_ll_rot <- sfaFLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ),
  regtype = "ll", bw.sel = "rot"  )

printFLW( FLW_Result_ll_rot )

# local-constant regression, ls.cv bandwidths
FLW_Result_aic <- sfaFLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ),
  bw.sel = "cv.aic" )

printFLW( FLW_Result_aic )

# local-linear regression, ls.cv bandwidths
FLW_Result_ll_aic <- sfaFLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ),
  regtype = "ll", bw.sel = "cv.aic" )

printFLW( FLW_Result_ll_aic )

# wrong value of argument 'bw.sel'
try( FLW_Result_ll_aic <- sfaFLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ),
  bw.sel = "wrong" ) )
