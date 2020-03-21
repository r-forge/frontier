cooks.distance.frontier <- function( model, progressBar = TRUE, ... ) {
  
  estCall <- model$call
  estFunc <- as.character( estCall[[ 1 ]] )
  estArg <- as.list( estCall )[ -1 ]
  
  # data set used for the estimation
  estData <- eval( estCall$data )
  fitVal <- fitted( model, asInData = TRUE )
  residVal <- residuals( model, asInData = TRUE )
  
  # make some checks
  reportMessage <- paste( 
    "Please report this problem to the maintainer of the 'frontier' package",
    "preferable wit R code and data that reproduce this error" )
  if( nrow( estData ) != length( model$validObs ) ) {
    stop( "internal error: the number of rows of 'estData' is not equal to",
      " the number of elements of 'validObs'. ",
      reportMessage )
  }
  if( length( fitVal ) != length( model$validObs ) ) {
    stop( "internal error: the number of elements of 'fitVal' is not equal to",
      " the number of elements of 'validObs'. ",
      reportMessage )
  }
  if( length( residVal ) != length( model$validObs ) ) {
    stop( "internal error: the number of elements of 'residVal' is not equal to",
      " the number of elements of 'validObs'. ",
      reportMessage )
  }
  if( any( is.na( fitVal[ model$validObs ] ) ) ) {
    stop( "internal error: there are NA values in 'fitVal[ validObs ]'. ",
      reportMessage )
  }
  if( any( is.na( residVal[ model$validObs ] ) ) ) {
    stop( "internal error: there are NA values in 'residVal[ validObs ]'. ",
      reportMessage )
  }
  
  # variance of the error term
  sigma2 <- sum( ( residVal[ model$validObs ] - 
      mean( residVal[ model$validObs ] ) )^2 ) /
    ( sum( model$validObs ) - model$nb )
  
  # do not print output at iteration when re-estimating the SFA models
  estArg$printIter <- 0
  
  # vector for Cook's distances
  cooksDist <- rep( NA, sum( model$validObs ) )
  
  # create progress bar
  if( progressBar ) {
    progBar <- txtProgressBar( min = 0, max = length( cooksDist ), style = 3 )
  }
  
  for( i in 1:length( cooksDist ) ) {
    # re-estimate the SFA model without the i-th valid observation
    estArg$data <- estData[ -which( model$validObs )[i], ]
    estNew <- suppressWarnings( do.call( estFunc, estArg ) )
  
    # obtain predicted values for all observations
    predVal <- predict( estNew, newdata = estData )
    
    # calculate Cook's distance
    cooksDist[i] <- sum( ( (fitVal - predVal )[ model$validObs ] )^2 ) /
      ( model$nb * sigma2 )
    
    # update progress bar
    if( progressBar ) {
      setTxtProgressBar( progBar, i )
    }
  }
  # close progress bar
  if( progressBar ) {
    close( progBar )
  }
  
  return( cooksDist )
}
