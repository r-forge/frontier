library( frontier )

## example data included in FRONTIER 4.1 (cross-section data)
data( front41Data )
front41Data$firmNo <- c( 1:nrow( front41Data ) )

## non-existing variable
try( sfa( log( output ) ~ log( capital7 ) + log( labour ),
   data = front41Data ) )

## nParamTotal > nObs
try( sfa( log( output ) ~ log( capital ) + log( labour ),
   data = front41Data[ 1:4, ] ) )

## nParamTotal >> nObs
try( sfa( log( output ) ~ log( capital ) + log( labour ),
   data = front41Data[ 1:2, ] ) )

## nParamTotal > number of valid observations
try( sfa( log( output ) ~ log( capital ) + log( labour ) + log( firmNo - 56 ),
   data = front41Data ) )

## the dependent variable has only infinite values
try( sfa( log( 0 * output ) ~ log( capital ) + log( labour ),
   data = front41Data ) )

## the dependent variable has only NA values
try( sfa( log( -output ) ~ log( capital ) + log( labour ),
   data = front41Data ) )

## one of the regressors has only infinite values
try( sfa( log( output ) ~ log( 0 * capital ) + log( labour ),
   data = front41Data ) )

## one of the regressors has only NA values
try( sfa( log( output ) ~ log( capital ) + log( -labour ),
   data = front41Data ) )

## one of the regressors of the inefficiency term has only infinite values
try( sfa( log( output ) ~ log( capital ) + log( labour ) | log( 0 * firmNo ),
   data = front41Data ) )

## one of the regressors of the inefficiency term has only NA values
try( sfa( log( output ) ~ log( capital ) + log( labour ) | log(-firmNo ),
   data = front41Data ) )


