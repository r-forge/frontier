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

# function for printing objects returned by FLW():
printFLW <- function( x ) {
  for( i in 1:length( x ) ) {
    cat( "$", names( x )[ i ], "\n", sep = "" )
    print( round( x[[ i ]], 2 ) )
    cat( "\n" )
  }
  invisible( x )
}


# local-constant regression, rule-of-thumb bandwidths
sfa.flw <- FLW(y=y,x=x,bw.sel="rot")  # ,dis="ntn"

printFLW( sfa.flw )

# local-linear regression, rule-of-thumb bandwidths
sfa.flw.ll <- FLW( y = y, x = x, regtype = "ll", bw.sel = "rot" )

printFLW( sfa.flw.ll )

# local-constant regression, ls.cv bandwidths
sfa.flw.ls <- FLW( y = y, x = x )

printFLW( sfa.flw.ls )

# local-linear regression, ls.cv bandwidths
sfa.flw.ll.ls <- FLW( y = y, x = x, regtype = "ll" )

printFLW( sfa.flw.ll.ls )

# local-constant regression, aic bandwidths
sfa.flw.aic <- FLW( y = y, x = x, bw.sel = "cv.aic" )

printFLW( sfa.flw.aic )

# local-linear regression, aic bandwidths
sfa.flw.ll.aic <- FLW( y = y, x = x, regtype = "ll", bw.sel = "cv.aic" )

printFLW( sfa.flw.ll.aic )

# wrong value of argument 'bw.sel'
try( FLW( y = y, x = x, bw.sel = "unknown" ) )
