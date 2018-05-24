library( "maxLik" )
library( "SDraw" )

### generate data set
set.seed( 123 )
nObs <- 200
dat <- data.frame( x = rnorm( nObs ), eps = rnorm( nObs ) )
dat$yStar <- 0.5 + 3 * dat$x + dat$eps
dat$y <- as.numeric( dat$yStar > 0 )

### estimation using glm()
glmRes <- glm( y ~ x, data = dat, family = binomial( link = "probit" ) )
summary( glmRes )
logLik( glmRes )

### estimation using maxLik()
llf <- function( par ) {
  b0 <- par[1]
  b1 <- par[2]
  bx <- b0 + b1 * dat$x
  ll <- log( ifelse( dat$y == 1, pnorm( bx ), pnorm( -bx ) ) )
  attr( ll, "gradient" ) <- dnorm( bx ) / 
    pnorm( ifelse( dat$y == 1, bx, -bx ) ) *
    cbind( ifelse( dat$y == 1, 1, -1 ), ifelse( dat$y == 1, dat$x, -dat$x ) )
  return( ll )
}
mlRes <- maxLik( llf, start = c(0,0), method = "BHHH" )
summary( mlRes )

# compare glm() results with maxLik() results
all.equal( coef( glmRes ), coef( mlRes ), check.attributes = FALSE,
  tol = 1e-4 )
all.equal( c( logLik( glmRes ) ), logLik( mlRes ) )

### estimation using simulated maximum likelihood and maxLik()
nRan <- 1e3
llfSim <- function( par ) {
  b0 <- par[1]
  b1 <- par[2]
  bx <- b0 + b1 * dat$x
  z <- qnorm( halton( nRan ) )
  lhAll <- matrix( NA, nrow( dat ), nRan )
  dif <- 0.1
  lhGradAll <- array( NA, c( nrow( dat ), length( par ), nRan ) )
  for( i in 1:nRan ) {
    lhAll[ , i ] <- ifelse( dat$y == 1, z[i] <= bx, z[i] > bx )
    for( j in 1:2 ) {
      if( j == 1 ) {
        bxLow <- ( b0 - dif / 2 ) + b1 * dat$x
        bxUpp <- ( b0 + dif / 2 ) + b1 * dat$x
      } else {
        bxLow <- b0 + ( b1 - dif / 2 ) * dat$x
        bxUpp <- b0 + ( b1 + dif / 2 ) * dat$x
      }
      lhGradAll[ , j, i ] <- ifelse( dat$y == 1, 
        ( ( z[i] <= bxUpp ) - ( z[i] <= bxLow ) ) / dif, 
        ( ( z[i] > bxUpp ) - ( z[i] > bxLow ) ) / dif ) 
    }
  }
  ll <- log( rowSums( lhAll ) / nRan )
  attr( ll, "gradient" ) <- ( apply( lhGradAll, c(1,2), sum ) / nRan ) /
    cbind( rowSums( lhAll ) / nRan, rowSums( lhAll ) / nRan )
  return( ll )
}
plot( llf( c(0.5, 3) ), llfSim( c(0.5, 3) ) )
abline( 0, 1 )
plot( attr( llf( c(0.5, 3) ), "gradient" )[,1],
  attr( llfSim( c(0.5, 3) ), "gradient" )[,1] )
abline(0,1)
plot( attr( llf( c(0.5, 3) ), "gradient" )[,2],
  attr( llfSim( c(0.5, 3) ), "gradient" )[,2] )
abline(0,1)

mlSimRes <- maxLik( llfSim, start = c(0,0), method = "BHHH" )
summary( mlSimRes )

### compare all coefficient estimates and their standard errors
rbind( glm = c( coef( summary( glmRes ) )[,1:2] ), 
  ml = c( coef( summary( mlRes ) )[,1:2] ), 
  mlSim = c( coef( summary( mlSimRes ) )[,1:2] ) )

