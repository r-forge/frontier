library( "maxLik" )
library( "SDraw" )

data("Electricity1970",package = "AER")
Electricity1970

# lodlik function

LogLNE <- function(pars, x, y) {
  beta <- pars[1:5]
  theta <- exp(pars[6])
  sigma_v <- exp(pars[7])
  epsilon <- y - x%*%beta
  N <- length(y)
  
  llNE <- log(theta) + (1/2) * theta^2 * sigma_v^2 + theta*(-1)*epsilon + 
      log(pnorm(-((-1)*epsilon/sigma_v + theta*sigma_v)))
  
  return(llNE)
}

y <- log(Electricity1970$cost/Electricity1970$fuel)
x <- matrix(c(rep(1,158), log(Electricity1970$labor/Electricity1970$fuel), 
              log(Electricity1970$capital/ Electricity1970$fuel),
              log(Electricity1970$output), (log(Electricity1970$output))^2), ncol=5)

lm <- lm(y~x-1)
coefficients(lm)
sd(lm$residuals)
1/(sd(lm$residuals)*sqrt((1-2/pi)))

estNE <- maxLik(LogLNE, x=x, y=y, 
  start=c( coef(lm), log(1/(sd(lm$residuals)*sqrt((1-2/pi)))), 
    log(sd(lm$residuals))), method = "BFGS")
summary( estNE )
exp( coef( estNE )[6:7] )

LogLNG <- function(pars, x, y, nDraw = 500 ) {
  beta <- pars[1:5]
  theta <- exp(pars[6])
  sigma_v <- exp(pars[7])
  pPar <- 1 + exp(pars[8])
  epsilon <- y - x%*%beta
  N <- length(y)
  
  mu <- - (-1) * epsilon - theta * sigma_v^2
  
  hVec <- rep( 0, N )
  for( i in 1:nDraw ) {
    fVec <- halton( N, start = 1 + (i-1)*N )
    hVec <- hVec + 
      ( mu + sigma_v * 
          qnorm( fVec + (1-fVec) * pnorm( -mu / sigma_v ) ) )^(pPar - 1)
  }
  
  llNG <- log(theta) + (1/2) * sigma_v^2 * theta^2 + 
    theta * (-1) * epsilon + log( pnorm( mu / sigma_v ) ) + 
    ( pPar - 1 ) * log(theta) - log(gamma(pPar)) +
    log( (1/nDraw) * hVec )
  
  return(llNG)
}

estNG <- maxLik( LogLNG, x = x, y = y, 
  start = c( coef(lm), log(1/(sd(lm$residuals)*sqrt((1-2/pi)))), 
    log(sd(lm$residuals)), log( 1.1 - 1) ), method = "BFGS" )
summary( estNG )
exp( coef( estNG )[6:8] ) + c(0,0,1)
