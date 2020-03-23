library( "frontier" )
library( "plm" )
options( digits = 5 )

## example data included in FRONTIER 4.1 (cross-section data)
data( front41Data )
row.names( front41Data ) <- paste( "F", row.names( front41Data ), sep = "_" )
front41Data$logOutput  <- log( front41Data$output )
front41Data$logCapital <- log( front41Data$capital )
front41Data$logLabour  <- log( front41Data$labour )
front41Data$firmNo     <- c( 1:nrow( front41Data ) )
front41Data$ones       <- 1
   
## cross-section data, error components frontier
a1 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ) )
round( cooks.distance( a1, progressBar = FALSE ), 3 )

## cross-section data, error components frontier, truncNorm, starting values
a5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), truncNorm = TRUE,
   startVal = c( 0.5, 0.3, 0.5, 0.5, 0.9, -1 ) )
round( cooks.distance( a5, progressBar = FALSE ), 3 )

## cross-section data, efficiency effects frontier, zIntercept, starting values
aa5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", zIntercept = TRUE,
   startVal = c( 0.5, 0.3, 0.5, -0.4, -0.01 , 0.4, 0.9 ) )
round( cooks.distance( aa5, progressBar = FALSE ), 3 )


## cross-section data with NAs and infinit values
naData <- front41Data
naData$output[3] <- NA
naData$capital[5] <- 0
naData$labour[9] <- 0
naData$firmNo[14] <- NA

## cross-section data with NAs, error components frontier
San1 <- sfa( log( output ) ~ log( capital ) + log( labour ), data = naData )
round( cooks.distance( San1, progressBar = FALSE ), 3 )

## cross-section data with NAs, efficiency effects frontier
Saan1 <- sfa( log( output ) ~ log( capital ) + log( labour ) | firmNo - 1,
   data = naData )
round( cooks.distance( Saan1, progressBar = FALSE ), 3 )


## data set of rice producers in the Philippines
data( riceProdPhil )
riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
riceProdPhil$lNPK   <- log( riceProdPhil$NPK )
riceProdPhil$ones   <- 1

## cross-section rice data, error components frontier
bb1 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
round( cooks.distance( bb1, progressBar = FALSE ), 3 )

## cross-section rice data, error components frontier, truncNorm, starting values
bb7 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.9, -0.01 ) )
round( cooks.distance( bb7, progressBar = FALSE ), 3 )


## Cost Frontier (with land as quasi-fixed input)
riceProdPhil$cost <- riceProdPhil$LABOR * riceProdPhil$LABORP +
   riceProdPhil$NPK * riceProdPhil$NPKP
riceProdPhil$lCost   <- log( riceProdPhil$cost )
riceProdPhil$lLABORP <- log( riceProdPhil$LABORP )
riceProdPhil$lNPKP   <- log( riceProdPhil$NPKP )

## cross-section rice data, error components cost frontier
dd1 <- frontier( "lCost", xNames = c( "lPROD", "lAREA", "lLABORP", "lNPKP" ),
   data = riceProdPhil, ineffDecrease = FALSE )
round( cooks.distance( dd1, progressBar = FALSE ), 3 )


## panel data
riceProdPhil$farm <- paste( "F_", ifelse( riceProdPhil$FMERCODE > 9, "", "0" ),
   riceProdPhil$FMERCODE, sep = "" )
riceProdPhil$year <- riceProdPhil$YEARDUM + 1998
riceProdPhilPanel <- pdata.frame( riceProdPhil, c( "farm", "year" ),
   row.names = FALSE )
riceProdPhilPanel$ones <- 1

## panel data, error components frontier
b1 <- frontier( data = riceProdPhilPanel,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
round( cooks.distance( b1, progressBar = FALSE ), 3 )


## unbalanced panel data
set.seed( 321 )
riceProdPhilPanelUnb <- riceProdPhilPanel
riceProdPhilPanelUnb[ 3, c( "PROD", "lPROD" ) ] <- NA
riceProdPhilPanelUnb[ 5, c( "AREA", "lAREA" ) ] <- NA
riceProdPhilPanelUnb[ 111, c( "LABOR", "lLABOR", "LABORP", "lLABORP" ) ] <- NA
riceProdPhilPanelUnb[ 222, c( "NPK", "lNPK", "NPKP", "lNPKP" ) ] <- NA

## unbalanced panel data, error components frontier, truncNorm, timeEffect
b4u <- sfa( lPROD ~ lAREA + lLABOR + lNPK, data = riceProdPhilPanelUnb,
   truncNorm = TRUE, timeEffect = TRUE )
round( cooks.distance( b4u, progressBar = FALSE ), 3 )


## unbalanced panel data with firms that have NAs in all time periods
naPanelData <- riceProdPhilPanelUnb
naPanelData[ naPanelData$farm == "F_21", "PROD" ] <- NA
naPanelData[ naPanelData$farm == "F_23", "AREA" ] <- NA
naPanelData[ naPanelData$farm == "F_26", "LABOR" ] <- NA
naPanelData[ naPanelData$farm == "F_30", "NPK" ] <- NA
naPanelData[ naPanelData$farm == "F_35", "EDYRS" ] <- NA

## panel data with NA firms, efficiency effects frontier
b5n <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
   EDYRS + BANRAT - 1, data = naPanelData )
round( cooks.distance( b5n, progressBar = FALSE ), 3 )
