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

# local-constant regression, rule-of-thumb bandwidths
sfa.flw <- FLW(y=y,x=x,bw.sel="rot")  # ,dis="ntn"

for( i in 1:length( sfa.flw ) ) {
  cat( "$", names( sfa.flw )[ i ], "\n", sep = "" )
  print( round( sfa.flw[[ i ]], 2 ) )
  cat( "\n" )
}

# local-linear regression, rule-of-thumb bandwidths
sfa.flw.ll <- FLW( y = y, x = x, regtype = "ll", bw.sel = "rot" )

for( i in 1:length( sfa.flw.ll ) ) {
  cat( "$", names( sfa.flw.ll )[ i ], "\n", sep = "" )
  print( round( sfa.flw.ll[[ i ]], 2 ) )
  cat( "\n" )
}

# local-constant regression, ls.cv bandwidths
sfa.flw.ls <- FLW( y = y, x = x )

for( i in 1:length( sfa.flw.ls ) ) {
  cat( "$", names( sfa.flw.ls )[ i ], "\n", sep = "" )
  print( round( sfa.flw.ls[[ i ]], 2 ) )
  cat( "\n" )
}

# local-linear regression, ls.cv bandwidths
sfa.flw.ll.ls <- FLW( y = y, x = x, regtype = "ll" )

for( i in 1:length( sfa.flw.ll.ls ) ) {
  cat( "$", names( sfa.flw.ll.ls )[ i ], "\n", sep = "" )
  print( round( sfa.flw.ll.ls[[ i ]], 2 ) )
  cat( "\n" )
}

# local-constant regression, aic bandwidths
sfa.flw.aic <- FLW( y = y, x = x, bw.sel = "cv.aic" )

for( i in 1:length( sfa.flw.aic ) ) {
  cat( "$", names( sfa.flw.aic )[ i ], "\n", sep = "" )
  print( round( sfa.flw.aic[[ i ]], 2 ) )
  cat( "\n" )
}

# local-linear regression, aic bandwidths
sfa.flw.ll.aic <- FLW( y = y, x = x, regtype = "ll", bw.sel = "cv.aic" )

for( i in 1:length( sfa.flw.ll.aic ) ) {
  cat( "$", names( sfa.flw.ll.aic )[ i ], "\n", sep = "" )
  print( round( sfa.flw.ll.aic[[ i ]], 2 ) )
  cat( "\n" )
}
