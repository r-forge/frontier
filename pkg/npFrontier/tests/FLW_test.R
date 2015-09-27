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
FLW_Result <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data )

printFLW( FLW_Result )
round( residuals( FLW_Result, which = "first" ), 2 )
round( residuals( FLW_Result ), 2 )
all.equal( residuals( FLW_Result, which = "final" ),
  residuals( FLW_Result ) )

round( fitted( FLW_Result, which = "first" ), 2 )
round( fitted( FLW_Result ), 2 )
all.equal( fitted( FLW_Result, which = "frontier" ),
  fitted( FLW_Result ) )

round( gradients( FLW_Result ), 2 )

# local-linear regression, ls.cv bandwidths
FLW_Result_ll <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, npArg = list( regtype = "ll" ) )

printFLW( FLW_Result_ll )
round( residuals( FLW_Result_ll, which = "first" ), 2 )
round( residuals( FLW_Result_ll ), 2 )

round( fitted( FLW_Result_ll, which = "first" ), 2 )
round( fitted( FLW_Result_ll ), 2 )

round( gradients( FLW_Result_ll ), 2 )

# local-constant regression, rule-of-thumb bandwidths
FLW_Result_rot <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, bw.sel = "rot"  )

printFLW( FLW_Result_rot )
round( residuals( FLW_Result_rot, which = "first" ), 2 )
round( residuals( FLW_Result_rot ), 2 )

round( fitted( FLW_Result_rot, which = "first" ), 2 )
round( fitted( FLW_Result_rot ), 2 )

round( gradients( FLW_Result_rot ), 2 )

# local-linear regression with rule-of-thumb bandwidths
FLW_Result_ll_rot <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, bw.sel = "rot", npArg = list( regtype = "ll" ) )

printFLW( FLW_Result_ll_rot )
round( residuals( FLW_Result_ll_rot, which = "first" ), 2 )
round( residuals( FLW_Result_ll_rot ), 2 )

round( fitted( FLW_Result_ll_rot, which = "first" ), 2 )
round( fitted( FLW_Result_ll_rot ), 2 )

round( gradients( FLW_Result_ll_rot ), 2 )

# local-constant regression, ls.cv bandwidths
FLW_Result_aic <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, bw.sel = "cv.aic" )

printFLW( FLW_Result_aic )
round( residuals( FLW_Result_aic, which = "first" ), 2 )
round( residuals( FLW_Result_aic ), 2 )

round( fitted( FLW_Result_aic, which = "first" ), 2 )
round( fitted( FLW_Result_aic ), 2 )

round( gradients( FLW_Result_aic ), 2 )

# local-linear regression, ls.cv bandwidths
FLW_Result_ll_aic <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, bw.sel = "cv.aic", npArg = list( regtype = "ll" ) )

printFLW( FLW_Result_ll_aic )
round( residuals( FLW_Result_ll_aic, which = "first" ), 2 )
round( residuals( FLW_Result_ll_aic ), 2 )

round( fitted( FLW_Result_ll_aic, which = "first" ), 2 )
round( fitted( FLW_Result_ll_aic ), 2 )

round( gradients( FLW_Result_ll_aic ), 2 )

# no intercept
FLW_Result_rot2 <- sfaFLW( log( output ) ~ log( capital ) + log( labour ) - 1,
  data = front41Data, bw.sel = "rot"  )

all.equal( FLW_Result_rot, FLW_Result_rot2 )
all.equal( residuals( FLW_Result_rot, which = "first" ),
  residuals( FLW_Result_rot2, which = "first" ) )
all.equal( residuals( FLW_Result_rot ), residuals( FLW_Result_rot2 ) )

all.equal( fitted( FLW_Result_rot, which = "first" ),
  fitted( FLW_Result_rot2, which = "first" ) )
all.equal( fitted( FLW_Result_rot ), fitted( FLW_Result_rot2 ) )

all.equal( gradients( FLW_Result_rot ), gradients( FLW_Result_rot2 ) )

# wrong value of argument 'bw.sel'
try( sfaFLW( log( output ) ~ log( capital ) + log( labour ),
  data = front41Data, bw.sel = "wrong" ) )

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
