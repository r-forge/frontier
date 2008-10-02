library( frontier )
options( digits = 5 )

data( front41Data )
front41Data$logOutput  <- log( front41Data$output )
front41Data$logCapital <- log( front41Data$capital )
front41Data$logLabour  <- log( front41Data$labour )

a1 <- frontier( front41Data, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ) )
print( a1 )
print.default( a1 )

a2 <- frontier( front41Data, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ), mu = TRUE )
print( a2 )
print.default( a2 )

a3 <- frontier( front41Data, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ), eta = TRUE )
print( a3 )
print.default( a3 )

a4 <- frontier( front41Data, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ), mu = TRUE, eta = TRUE )
print( a4 )
print.default( a4 )

a5 <- frontier( front41Data, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ), mu = TRUE,
   startVal = c( 0.5, 0.3, 0.5, 0.5, 0.9, -1 ) )
print( a5 )
print.default( a5 )


data( riceProdPhil )
riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
riceProdPhil$lNPK   <- log( riceProdPhil$NPK )

b1 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
print( b1 )
print.default( b1 )

b2 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   mu = TRUE )
print( b2 )
print.default( b2 )

b3 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   eta = TRUE )
print( b3 )
print.default( b3 )

b4 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   mu = TRUE, eta = TRUE )
print( b4 )
print.default( b4 )

b5 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ) )
print( b5 )
print.default( b5 )

b6 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), mu = TRUE )
print( b6 )
print.default( b6 )

b7 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   mu = TRUE, eta = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.5, -0.3, 0.1 ) )
print( b7 )
print.default( b7 )

b8 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), mu = TRUE, 
   startVal = c( -1, 0.3, 0.3, 0.3, -3, -0.1, -4, 2, 0.8 ) )
print( b8 )
print.default( b8 )
