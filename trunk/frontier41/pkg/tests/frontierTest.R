library( frontier )

data( front41Data )
front41Data$logOutput  <- log( front41Data$output )
front41Data$logCapital <- log( front41Data$capital )
front41Data$logLabour  <- log( front41Data$labour )

a1 <- frontier( front41Data, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ) )
print.default( a1 )

a2 <- frontier( front41Data, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ), mu = TRUE )
print.default( a2 )

a3 <- frontier( front41Data, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ), eta = TRUE )
print.default( a3 )

a4 <- frontier( front41Data, "firm", "time", "logOutput",
   c( "logCapital", "logLabour" ), mu = TRUE, eta = TRUE )
print.default( a4 )


data( riceProdPhil )
riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
riceProdPhil$lNPK   <- log( riceProdPhil$NPK )

b1 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )
print.default( b1 )

b2 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   mu = TRUE )
print.default( b2 )

b3 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   eta = TRUE )
print.default( b3 )

b4 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   mu = TRUE, eta = TRUE )
print.default( b4 )

b5 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ) )
print.default( b5 )

b6 <- frontier( riceProdPhil,
   crossSectionName = "FMERCODE", timePeriodName = "YEARDUM",
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), mu = TRUE )
print.default( b6 )
