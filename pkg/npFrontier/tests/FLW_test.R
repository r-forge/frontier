## Code to test function FLW()

library( "npFrontier" )

# load data
data( front41Data, package = "frontier" )

# semiparametric local-constant Cobb-Douglas production frontier
FLW_Result <- FLW( log( front41Data$output ), 
  cbind( log( front41Data$capital ), log( front41Data$labour ) ) )

for( i in 1:length( FLW_Result ) ) {
  cat( "$", names( FLW_Result )[ i ], "\n", sep = "" )
  print( round( FLW_Result[[ i ]], 2 ) )
  cat( "\n" )
}
