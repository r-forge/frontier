## Code for conducting semiparametric stochastic frontier analysis
## 1. Fan, Li and Weersink (1996) (FLW)
flw.ll <- function(lmd=1,e){

  ## Caculate sigma(lambda) as in equation (14), page 462
  sc <- sqrt(2*lmd^2/pi/(1+lmd^2))
  si <- sqrt(mean(e^2)/(1-sc^2)) ## This is (14)
  
  ## Now calculate mean correction as in discussion 
  ## on page 462, left column prior to (10) 
  mu <- si*sc
  
  ## Now calculate bias adjusted residuals
  ep <- e-mu
  
  # log-likelihood
  flw.ll <- -length(ep)*log(si)+sum(pnorm(-ep*lmd/si,log.p=TRUE))-0.5*sum(ep^2)/si^2    
  return(-flw.ll)
}

sfaFLW <- function( formula, data = sys.frame( sys.parent() ),
  bw.sel = "cv.ls", npArg = list() ) {

  # warn if the intercept is suppressed
  if( attr( terms( formula ), "intercept" ) == 0L ) {
    warning( "the intercept cannot be suppressed",
      " in nonparametric regression" )
  }

  # check argument 'npArg'
  if( is.list( npArg ) ) {
    if( any( names( npArg ) %in%
        c( "xdat", "ydat", "bwmethod", "bandwidth.compute" ) ) ) {
      stop( "argument 'npArg' may not be used to specify arguments",
        " 'xdat', 'ydat', 'bwmethod', and 'bandwidth.compute'" )
    }
  } else {
    stop( "argument 'npArg' must be a list" )
  }
  
  # save the (matched) call  
  mc <- match.call( expand.dots = FALSE )
  
  # obtain the model matrix and the vector of responses
  m <- match( c( "formula", "data" ), names( mc ), 0L )
  mf <- mc[ c( 1L, m ) ]
  mf$drop.unused.levels <- TRUE
  mf[[ 1L ]] <- as.name( "model.frame" )
  mf <- eval( mf, parent.frame() )
  mt <- attr( mf, "terms" )
  x <- model.matrix( mt, mf )
  y <- model.response( mf, "numeric" )

  # remove intercept from model matrix
  x <- x[ , colnames( x ) != "(Intercept)", drop = FALSE ]
  
  ## Step 1: Estimate conditional mean and obtain residuals
  
  if( bw.sel %in% c( "cv.ls", "cv.aic" ) ) {
    bw <- do.call( npregbw,
      args = c( list( ydat = y, xdat = x, bwmethod = bw.sel ), npArg ) )
  } else if( bw.sel == "rot" ) {
    bw <- do.call( npregbw,
      args = c( list( ydat = y, xdat = x, bandwidth.compute = FALSE ),
        npArg ) )
    bw$bw <- 1.06*apply(as.matrix(x),2,sd)*length(y)^(-1/(4+ncol(as.matrix(x))))
  } else {
    stop( "argument 'bw.sel' must be either 'cv.ls', 'cv.aic', or 'rot'" )
  }

  ## Need to allow for users to pass own bandwidths directly
  ## so tht we can also have constrained FLW.

  model <- npreg(bws=bw,tydat=y,txdat=x,residuals=TRUE,gradients=TRUE)
  
  ## Extract residuals from model
  resid <- residuals(model)
  
  ## Step 2: Pass resid to flw.ll to obtain estimates of lambda and sigma
  ## if(skewness(resid)>0){resid<- -resid}
  
  sol <- mle2(flw.ll,start=list(lmd=1),data=list(e=resid))
  
  lmd <- as.numeric(coef(sol)[1])
  
  ## Use estimate of lambda to calculate sigma^2
  sc <- sqrt(2*lmd^2/pi/(1+lmd^2))
  sig.sq <- mean(resid^2)/(1-sc^2)
  
  ## Now calculate sigma.u and sigma.v
  sigma.v <- sqrt(sig.sq/(1+lmd^2))
  sigma.u <- sigma.v*lmd
  
  ## Caluclate Bias Correction, see beneath (15) on page 463.
  mu <- sqrt(sig.sq)*sc

  returnObj <- list()
  returnObj$mu <- mu 
  returnObj$mhat <- fitted( model ) + mu 
  returnObj$mprime <- gradients(model)
  returnObj$e <- unname( resid )
  returnObj$sigma.sq <- sig.sq
  returnObj$lambda <- lmd
  returnObj$sigma.u <- sigma.u
  returnObj$sigma.v <- sigma.v
  returnObj$bw <- bw$bw
  
  class( returnObj ) <- "sfaFLW" 
  
  return( returnObj )
}
