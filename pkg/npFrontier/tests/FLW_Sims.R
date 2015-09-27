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
    print( round( x[[ i ]], 2 ) )
    cat( "\n" )
  }
  invisible( x )
}


# local-constant regression, rule-of-thumb bandwidths
sfa.flw <- sfaFLW( y ~ x, bw.sel = "rot" )  # ,dis="ntn"

printFLW( sfa.flw )
round( residuals( sfa.flw, which = "first" ), 2 )
round( residuals( sfa.flw ), 2 )
all.equal( residuals( sfa.flw, which = "final" ),
  residuals( sfa.flw ) )

round( fitted( sfa.flw, which = "first" ), 2 )
round( fitted( sfa.flw ), 2 )
all.equal( fitted( sfa.flw, which = "frontier" ), fitted( sfa.flw ) )

# local-linear regression, rule-of-thumb bandwidths
sfa.flw.ll <- sfaFLW( y ~ x, bw.sel = "rot",
  npArg = list( regtype = "ll" ) )

printFLW( sfa.flw.ll )
round( residuals( sfa.flw.ll, which = "first" ), 2 )
round( residuals( sfa.flw.ll ), 2 )

round( fitted( sfa.flw.ll, which = "first" ), 2 )
round( fitted( sfa.flw.ll ), 2 )

# local-constant regression, ls.cv bandwidths
sfa.flw.ls <- sfaFLW( y ~ x )

printFLW( sfa.flw.ls )
round( residuals( sfa.flw.ls, which = "first" ), 2 )
round( residuals( sfa.flw.ls ), 2 )

round( fitted( sfa.flw.ls, which = "first" ), 2 )
round( fitted( sfa.flw.ls ), 2 )

# local-linear regression, ls.cv bandwidths
sfa.flw.ll.ls <- sfaFLW( y ~ x, npArg = list( regtype = "ll" ) )

printFLW( sfa.flw.ll.ls )
round( residuals( sfa.flw.ll.ls, which = "first" ), 2 )
round( residuals( sfa.flw.ll.ls ), 2 )

round( fitted( sfa.flw.ll.ls, which = "first" ), 2 )
round( fitted( sfa.flw.ll.ls ), 2 )

# local-constant regression, aic bandwidths
sfa.flw.aic <- sfaFLW( y ~ x, bw.sel = "cv.aic" )

printFLW( sfa.flw.aic )
round( residuals( sfa.flw.aic, which = "first" ), 2 )
round( residuals( sfa.flw.aic ), 2 )

round( fitted( sfa.flw.aic, which = "first" ), 2 )
round( fitted( sfa.flw.aic ), 2 )

# local-linear regression, aic bandwidths
sfa.flw.ll.aic <- sfaFLW( y ~ x, bw.sel = "cv.aic",
  npArg = list( regtype = "ll" ) )

printFLW( sfa.flw.ll.aic )
round( residuals( sfa.flw.ll.aic, which = "first" ), 2 )
round( residuals( sfa.flw.ll.aic ), 2 )

round( fitted( sfa.flw.ll.aic, which = "first" ), 2 )
round( fitted( sfa.flw.ll.aic ), 2 )

# no intercept
sfa.flw.ll2 <- sfaFLW( y ~ x - 1, bw.sel = "rot",
  npArg = list( regtype = "ll" ) )

all.equal( sfa.flw.ll, sfa.flw.ll2 )
all.equal( residuals( sfa.flw.ll, which = "first" ),
  residuals( sfa.flw.ll, which = "first" ) )
all.equal( residuals( sfa.flw.ll ), residuals( sfa.flw.ll ) )

all.equal( fitted( sfa.flw.ll, which = "first" ),
  fitted( sfa.flw.ll, which = "first" ) )
all.equal( fitted( sfa.flw.ll ), fitted( sfa.flw.ll ) )

# wrong value of argument 'bw.sel'
try( sfaFLW( y ~ x, bw.sel = "unknown" ) )

# wrong values of argument 'npArg'
try( sfaFLW( y ~ x, npArg = c( regtype = "ll" ) ) )
try( sfaFLW( y ~ x, npArg = list( bwmethod = "cv.aic." ) ) )
try( sfaFLW( y ~ x, npArg = list( regtype = "something" ) ) )

# residuals: wrong value of argument 'which'
try( residuals( sfa.flw, which = "failed" ) )

# fitted: wrong value of argument 'which'
try( fitted( sfa.flw, which = "not allowed" ) )
