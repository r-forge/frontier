## Code to test function FLW()

library( "npFrontier" )

# load data
data( front41Data, package = "frontier" )

# semiparametric Cobb-Douglas production frontiers
# local-constant regression, ls.cv bandwidths
FLW_Result <- FLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ) )

for( i in 1:length( FLW_Result ) ) {
  cat( "$", names( FLW_Result )[ i ], "\n", sep = "" )
  print( round( FLW_Result[[ i ]], 2 ) )
  cat( "\n" )
}

# local-linear regression, ls.cv bandwidths
FLW_Result_ll <- FLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ),
  regtype = "ll" )

for( i in 1:length( FLW_Result_ll ) ) {
  cat( "$", names( FLW_Result_ll )[ i ], "\n", sep = "" )
  print( round( FLW_Result_ll[[ i ]], 2 ) )
  cat( "\n" )
}

# local-constant regression, rule-of-thumb bandwidths
FLW_Result_rot <- FLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ),
  bw.sel = "rot"  )

for( i in 1:length( FLW_Result_rot ) ) {
  cat( "$", names( FLW_Result_rot )[ i ], "\n", sep = "" )
  print( round( FLW_Result_rot[[ i ]], 2 ) )
  cat( "\n" )
}

# local-linear regression with rule-of-thumb bandwidths
FLW_Result_ll_rot <- FLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ),
  regtype = "ll", bw.sel = "rot"  )

for( i in 1:length( FLW_Result_ll_rot ) ) {
  cat( "$", names( FLW_Result_ll_rot )[ i ], "\n", sep = "" )
  print( round( FLW_Result_ll_rot[[ i ]], 2 ) )
  cat( "\n" )
}

# local-constant regression, ls.cv bandwidths
FLW_Result_aic <- FLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ),
  bw.sel = "cv.aic" )

for( i in 1:length( FLW_Result_aic ) ) {
  cat( "$", names( FLW_Result_aic )[ i ], "\n", sep = "" )
  print( round( FLW_Result_aic[[ i ]], 2 ) )
  cat( "\n" )
}

# local-linear regression, ls.cv bandwidths
FLW_Result_ll_aic <- FLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ),
  regtype = "ll", bw.sel = "cv.aic" )

for( i in 1:length( FLW_Result_ll_aic ) ) {
  cat( "$", names( FLW_Result_ll_aic )[ i ], "\n", sep = "" )
  print( round( FLW_Result_ll_aic[[ i ]], 2 ) )
  cat( "\n" )
}
