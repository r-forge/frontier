## Code to estimate Fan, Li and Weersink (1996)
## Semiparametric Stochastic Frontier Model

library( "npFrontier" )

## Generate some fake data to try code out on
## Use Model 3 of FLW (1996)
lambda   <- 1.88
sigma.sq <- 1.66
mu       <- 0

sig.v <- sqrt(sigma.sq/(1+lambda^2))
sig.u <- sig.v*lambda

## Run small scall simulation to mimic FLW, Table 3.
set.seed( 123 )

n <- 200

v <- sig.v*rnorm(n)
u <- sig.u*abs(rnorm(n))
#u <- sig.u*rexp(n)

e <- v-u

x <- rchisq(n,1)

y <- 1+log(1+x)+e

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


# local-constant regression, rule-of-thumb bandwidths
sfa.flw <- sfaFLW( y ~ x, bwmethod = "rot" )  # ,dis="ntn"

printFLW( sfa.flw )

all.equal( residuals( sfa.flw, which = "final" ),
  residuals( sfa.flw ) )

all.equal( fitted( sfa.flw, which = "frontier" ), fitted( sfa.flw ) )

# local-linear regression, rule-of-thumb bandwidths
sfa.flw.ll <- sfaFLW( y ~ x, bwmethod = "rot",
  npArg = list( regtype = "ll" ) )

printFLW( sfa.flw.ll )

# local-constant regression, ls.cv bandwidths
sfa.flw.ls <- sfaFLW( y ~ x )

printFLW( sfa.flw.ls )

# local-linear regression, ls.cv bandwidths
sfa.flw.ll.ls <- sfaFLW( y ~ x, npArg = list( regtype = "ll" ) )

printFLW( sfa.flw.ll.ls )

# local-constant regression, aic bandwidths
sfa.flw.aic <- sfaFLW( y ~ x, bwmethod = "cv.aic" )

printFLW( sfa.flw.aic )

# local-linear regression, aic bandwidths
sfa.flw.ll.aic <- sfaFLW( y ~ x, bwmethod = "cv.aic",
  npArg = list( regtype = "ll" ) )

printFLW( sfa.flw.ll.aic )

# no intercept
sfa.flw.ll2 <- sfaFLW( y ~ x - 1, bwmethod = "rot",
  npArg = list( regtype = "ll" ) )

all.equal( sfa.flw.ll[ -1 ], sfa.flw.ll2[ -1 ] )
all.equal( residuals( sfa.flw.ll, which = "first" ),
  residuals( sfa.flw.ll, which = "first" ) )
all.equal( residuals( sfa.flw.ll ), residuals( sfa.flw.ll ) )

all.equal( fitted( sfa.flw.ll, which = "first" ),
  fitted( sfa.flw.ll, which = "first" ) )
all.equal( fitted( sfa.flw.ll ), fitted( sfa.flw.ll ) )

all.equal( gradients( sfa.flw.ll ), gradients( sfa.flw.ll ) )

# wrong value of argument 'bwmethod'
try( sfaFLW( y ~ x, bwmethod = "unknown" ) )

# wrong values of argument 'npArg'
try( sfaFLW( y ~ x, npArg = c( regtype = "ll" ) ) )
try( sfaFLW( y ~ x, npArg = list( bwmethod = "cv.aic." ) ) )
try( sfaFLW( y ~ x, npArg = list( regtype = "something" ) ) )

# residuals: wrong value of argument 'which'
try( residuals( sfa.flw, which = "failed" ) )

# fitted: wrong value of argument 'which'
try( fitted( sfa.flw, which = "not allowed" ) )
