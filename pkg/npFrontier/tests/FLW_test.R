## Code to test function sfaFLW()

library( "npFrontier" )

# load data
data( front41Data, package = "frontier" )

# function for printing objects returned by sfaFLW():
printFLW <- function( x ) {
  for( i in 1:length( x ) ) {
    cat( "$", names( x )[ i ], "\n", sep = "" )
    if( names( x )[ i ] == "npreg" ) {
      x[[ i ]]$bw <- round( x[[ i ]]$bw, 2 )
      print( x[[ i ]] )
    } else {
      print( round( x[[ i ]], 2 ) )
    }
    cat( "\n" )
  }

  cat( "> residuals( x, which = \"first\" )\n" )
  print( round( residuals( x, which = "first" ), 2 ) )
  cat( "\n> residuals( x )\n" )
  print( round( residuals( x ), 2 ) )
  
  cat( "\n> fitted( x, which = \"first\" )\n" )
  print( round( fitted( x, which = "first" ), 2 ) )
  cat( "\n> fitted( x )\n" )
  print( round( fitted( x ), 2 ) )
  
  cat( "\ngradients( x )\n" )
  print( round( gradients( x ), 2 ) )
  
  invisible( x )
}


# semiparametric Cobb-Douglas production frontiers
# local-constant regression, ls.cv bandwidths
FLW_Result <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data )

printFLW( FLW_Result )

all.equal( residuals( FLW_Result, which = "final" ),
  residuals( FLW_Result ) )

all.equal( fitted( FLW_Result, which = "frontier" ),
  fitted( FLW_Result ) )

# local-linear regression, ls.cv bandwidths
FLW_Result_ll <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, npArg = list( regtype = "ll" ) )

printFLW( FLW_Result_ll )

# local-constant regression, rule-of-thumb bandwidths
FLW_Result_rot <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, bwmethod = "rot"  )

printFLW( FLW_Result_rot )

# local-linear regression with rule-of-thumb bandwidths
FLW_Result_ll_rot <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, bwmethod = "rot", npArg = list( regtype = "ll" ) )

printFLW( FLW_Result_ll_rot )

# local-constant regression, ls.cv bandwidths
FLW_Result_aic <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, bwmethod = "cv.aic" )

printFLW( FLW_Result_aic )

# local-linear regression, ls.cv bandwidths
FLW_Result_ll_aic <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, bwmethod = "cv.aic", npArg = list( regtype = "ll" ) )

printFLW( FLW_Result_ll_aic )

# no intercept
FLW_Result_rot2 <- sfaFLW( log( output ) ~ log( capital ) + log( labour ) - 1,
  data = front41Data, bwmethod = "rot"  )

all.equal( FLW_Result_rot[ -1 ], FLW_Result_rot2[ -1 ] )
all.equal( residuals( FLW_Result_rot, which = "first" ),
  residuals( FLW_Result_rot2, which = "first" ) )
all.equal( residuals( FLW_Result_rot ), residuals( FLW_Result_rot2 ) )

all.equal( fitted( FLW_Result_rot, which = "first" ),
  fitted( FLW_Result_rot2, which = "first" ) )
all.equal( fitted( FLW_Result_rot ), fitted( FLW_Result_rot2 ) )

all.equal( gradients( FLW_Result_rot ), gradients( FLW_Result_rot2 ) )

# wrong value of argument 'bwmethod'
try( sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, bwmethod = "wrong" ) )

# wrong values of argument 'npArg'
try( sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, npArg = "ll" ) )
try( sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, npArg = list( bandwidth.compute = FALSE ) ) )
try( sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, npArg = list( ckertype = "something" ) ) )

# residuals: wrong value of argument 'which'
try( residuals( FLW_Result_rot, which = "none" ) )

# fitted: wrong value of argument 'which'
try( fitted( FLW_Result_rot, which = "final" ) )
