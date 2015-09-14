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

sfa.flw <- FLW(y=y,x=x,regtype="lc",bw.sel="rot")  # ,dis="ntn"

for( i in 1:length( sfa.flw ) ) {
  cat( "$", names( sfa.flw )[ i ], "\n", sep = "" )
  print( round( sfa.flw[[ i ]], 2 ) )
  cat( "\n" )
}
