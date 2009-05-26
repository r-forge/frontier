library( frontier )
options( digits = 5 )

data( front41Data )
row.names( front41Data ) <- paste( "F", row.names( front41Data ), sep = "_" )
front41Data$logOutput  <- log( front41Data$output )
front41Data$logCapital <- log( front41Data$capital )
front41Data$logLabour  <- log( front41Data$labour )
front41Data$firmNo     <- c( 1:nrow( front41Data ) )

## cross-section data, error components frontier
a1 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), code = "R" )
print( a1 )
coef( a1, which = "start" )
coef( a1, which = "ols" )
coef( a1, which = "grid" )
coef( a1 )
coef( summary( a1 ), which = "ols" )
coef( summary( a1 ) )
vcov( a1 )
logLik( a1, which = "ols" )
logLik( a1, which = "grid" )
logLik( a1 )
print( summary( a1 ) )
efficiencies( a1 )
efficiencies( a1, asInData = TRUE )
print.default( a1 )

## cross-section data, error components frontier, truncNorm
a2 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), truncNorm = TRUE ,code="R")
print( a2 )
coef( a2, which = "start" )
coef( a2, which = "ols" )
coef( a2, which = "grid" )
coef( a2 )
coef( summary( a2 ), which = "ols" )
coef( summary( a2 ) )
vcov( a2 )
logLik( a2, which = "ols" )
logLik( a2 )
print( summary( a2 ) )
efficiencies( a2 )
efficiencies( a2, asInData = TRUE )
print.default( a2 )

## cross-section data, error components frontier, truncNorm, starting values
a5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), truncNorm = TRUE,
   startVal = c( 0.5, 0.3, 0.5, 0.5, 0.9, -1 ) , code = "R" )
print( a5 )
coef( a5, which = "start" )
coef( a5, which = "ols" )
coef( a5, which = "grid" )
coef( a5 )
coef( summary( a5 ), which = "ols" )
coef( summary( a5 ) )
vcov( a5 )
logLik( a5, which = "ols" )
logLik( a5 )
print( summary( a5 ) )
efficiencies( a5 )
efficiencies( a5, asInData = TRUE )
print.default( a5 )

## cross-section data, efficiency effects frontier
aa1 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", code = "R" )
print( aa1 )
coef( aa1, which = "start" )
coef( aa1, which = "ols" )
coef( aa1, which = "grid" )
coef( aa1 )
coef( summary( aa1 ), which = "ols" )
coef( summary( aa1 ) )
vcov( aa1 )
print( summary( aa1 ) )
efficiencies( aa1 )
efficiencies( aa1, asInData = TRUE )
print.default( aa1 )

## cross-section data, efficiency effects frontier, zIntercept
aa2 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", zIntercept = TRUE, code = "R" )
print( aa2 )
coef( aa2, which = "start" )
coef( aa2, which = "ols" )
coef( aa2, which = "grid" )
coef( aa2 )
coef( summary( aa2 ), which = "ols" )
coef( summary( aa2 ) )
vcov( aa2 )
print( summary( aa2 ) )
efficiencies( aa2 )
efficiencies( aa2, asInData = TRUE )
print.default( aa2 )

## cross-section data, efficiency effects frontier, zIntercept, starting values
aa5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", zIntercept = TRUE,
   startVal = c( 0.5, 0.3, 0.5, -0.4, -0.01 , 0.4, 0.9 ), code = "R" )
print( aa5 )
coef( aa5, which = "start" )
coef( aa5, which = "ols" )
coef( aa5, which = "grid" )
coef( aa5 )
coef( summary( aa5 ), which = "ols" )
coef( summary( aa5 ) )
vcov( aa5 )
print( summary( aa5 ) )
efficiencies( aa5 )
efficiencies( aa5, asInData = TRUE )
print.default( aa5 )


## data set of rice producers in the Philippines
data( riceProdPhil )
riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
riceProdPhil$lNPK   <- log( riceProdPhil$NPK )

## cross-section rice data, error components frontier
bb1 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ), code = "R" )
print( bb1 )
coef( bb1, which = "start" )
coef( bb1, which = "ols" )
coef( bb1, which = "grid" )
coef( bb1 )
coef( summary( bb1 ), which = "ols" )
coef( summary( bb1 ) )
vcov( bb1 )
print( summary( bb1 ) )
efficiencies( bb1 )
efficiencies( bb1, asInData = TRUE )
print.default( bb1 )

## cross-section rice data, error components frontier, truncNorm
bb2 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, code = "R" )
print( bb2 )
coef( bb2, which = "start" )
coef( bb2, which = "ols" )
coef( bb2, which = "grid" )
coef( bb2 )
coef( summary( bb2 ), which = "ols" )
coef( summary( bb2 ) )
vcov( bb2 )
print( summary( bb2 ) )
efficiencies( bb2 )
efficiencies( bb2, asInData = TRUE )
print.default( bb2 )

## cross-section rice data, efficiency effects frontier
bb5 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), code = "R" )
print( bb5 )
coef( bb5, which = "start" )
coef( bb5, which = "ols" )
coef( bb5, which = "grid" )
coef( bb5 )
coef( summary( bb5 ), which = "ols" )
coef( summary( bb5 ) )
vcov( bb5 )
print( summary( bb5 ) )
efficiencies( bb5 )
efficiencies( bb5, asInData = TRUE )
print.default( bb5 )

## cross-section rice data, efficiency effects frontier, zIntercept
bb6 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE, code = "R" )
print( bb6 )
coef( bb6, which = "start" )
coef( bb6, which = "ols" )
coef( bb6, which = "grid" )
coef( bb6 )
coef( summary( bb6 ), which = "ols" )
coef( summary( bb6 ) )
vcov( bb6 )
print( summary( bb6 ) )
efficiencies( bb6 )
efficiencies( bb6, asInData = TRUE )
print.default( bb6 )

## cross-section rice data, error components frontier, truncNorm, starting values
bb7 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, code = "R",
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.9, -0.01 ) )
print( bb7 )
coef( bb7, which = "start" )
coef( bb7, which = "ols" )
coef( bb7, which = "grid" )
coef( bb7 )
coef( summary( bb7 ), which = "ols" )
coef( summary( bb7 ) )
vcov( bb7 )
print( summary( bb7 ) )
efficiencies( bb7 )
efficiencies( bb7, asInData = TRUE )
print.default( bb7 )

## cross-section rice data, efficiency effects frontier, zIntercept, starting values
bb8 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), zIntercept = TRUE, code = "R",
   startVal = c( -1, 0.3, 0.3, 0.3, -0.2, -0.01, -0.3, 0.3, 0.8 ) )
print( bb8 )
coef( bb8, which = "start" )
coef( bb8, which = "ols" )
coef( bb8, which = "grid" )
coef( bb8 )
coef( summary( bb8 ), which = "ols" )
coef( summary( bb8 ) )
vcov( bb8 )
print( summary( bb8 ) )
efficiencies( bb8 )
efficiencies( bb8, asInData = TRUE )
print.default( bb8 )


## data about agricultural production in Brazil
data( prodAgrBrazil)
prodAgrBrazil$lProd <- log(prodAgrBrazil$production)
prodAgrBrazil$lLabor <- log(prodAgrBrazil$labor)
prodAgrBrazil$lArea <- log(prodAgrBrazil$area)
prodAgrBrazil$lOther <- log(prodAgrBrazil$other_exp)
prodAgrBrazil$lCapital <- log(prodAgrBrazil$capital)

c1R <- frontier( data = prodAgrBrazil, yName = "lProd", 
    xNames = c( "lArea", "lLabor", "lOther", "lCapital" ), 
    zNames = c( "idh", "gip_pc" ), zIntercept = TRUE, code="R" )
print( c1R )
coef( c1R, which = "start" )
coef( c1R, which = "ols" )
coef( c1R, which = "grid" )
coef( c1R )
coef( summary( c1R ), which = "ols" )
coef( summary( c1R ) )
vcov( c1R )
print( summary( c1R ) )
efficiencies( c1R )
efficiencies( c1R, asInData = TRUE )
print.default( c1R )

c1F <- frontier( data = prodAgrBrazil, yName = "lProd",
    xNames = c( "lArea", "lLabor", "lOther", "lCapital" ),
    zNames = c( "idh", "gip_pc" ), zIntercept = TRUE, code="Fortran")
print( c1F )
coef( c1F, which = "start" )
coef( c1F, which = "ols" )
coef( c1F, which = "grid" )
coef( c1F )
coef( summary( c1F ), which = "ols" )
coef( summary( c1F ) )
vcov( c1F )
print( summary( c1F ) )
efficiencies( c1F )
efficiencies( c1F, asInData = TRUE )
print.default( c1F )

c1RR <- frontier( data = prodAgrBrazil, yName = "lProd", 
    xNames = c( "lArea", "lLabor", "lOther", "lCapital" ), 
    zNames = c( "idh", "gip_pc" ), zIntercept = TRUE, code="R", evalLogLik=TRUE,
    startVal=c1R$mleParam )

c1RF <- frontier( data = prodAgrBrazil, yName = "lProd", 
    xNames = c( "lArea", "lLabor", "lOther", "lCapital" ), 
    zNames = c( "idh", "gip_pc" ), zIntercept = TRUE, code="Fortran", evalLogLik=TRUE,
    startVal=c1R$mleParam)

c1FR <- frontier( data = prodAgrBrazil, yName = "lProd",
    xNames = c( "lArea", "lLabor", "lOther", "lCapital" ),
    zNames = c( "idh", "gip_pc" ), zIntercept = TRUE, code="R", evalLogLik=TRUE,
    startVal=c1F$mleParam )

c1FF <- frontier( data = prodAgrBrazil, yName = "lProd",
    xNames = c( "lArea", "lLabor", "lOther", "lCapital" ),
    zNames = c( "idh", "gip_pc" ), zIntercept = TRUE, code="Fortran", evalLogLik=TRUE,
    startVal=c1F$mleParam)

print(c1R$mleLogl)
print(c1RR$logLike)
print(c1RF$logLike)
print(c1F$mleLogl)
print(c1FR$logLike)
print(c1FF$logLike)

## translog frontiers
## cross-section data, error components frontier, translog
translog <- frontierQuad( data = front41Data, yName = "logOutput",
   xNames = c( "logCapital", "logLabour" ), code = "R" )
print( translog )
coef( translog, which = "start" )
coef( translog, which = "ols" )
coef( translog, which = "grid" )
coef( translog )
coef( summary( translog ), which = "ols" )
coef( summary( translog ) )
vcov( translog )
logLik( translog, which = "ols" )
logLik( translog )
print( summary( translog ) )
efficiencies( translog )
efficiencies( translog, asInData = TRUE )
translogEla <- elas( translog )
print( translogEla )
attributes( translogEla )$variance
attributes( translogEla )$stdDev
print.default( translog )

## cross-section data, error components frontier, translog, shifter
translogShift <- frontierQuad( yName = "logOutput",
   xNames = c( "logCapital", "logLabour" ), shifterNames = "firmNo",
   data = front41Data, code = "R" )
print( translogShift )
coef( translogShift, which = "start" )
coef( translogShift, which = "ols" )
coef( translogShift, which = "grid" )
coef( translogShift )
coef( summary( translogShift ), which = "ols" )
coef( summary( translogShift ) )
vcov( translogShift )
logLik( translogShift, which = "ols" )
logLik( translogShift )
print( summary( translogShift ) )
efficiencies( translogShift )
efficiencies( translogShift, asInData = TRUE )
translogShiftEla <- elas( translogShift )
print( translogShiftEla )
attributes( translogShiftEla )$variance
attributes( translogShiftEla )$stdDev
print.default( translogShift )

## cross-section data, efficiency effects frontier, translog
translogZvar <- frontierQuad( yName = "logOutput",
   xNames = c( "logCapital", "logLabour" ), zNames = "firmNo",
   data = front41Data, code = "R" )
print( translogZvar )
coef( translogZvar, which = "start" )
coef( translogZvar, which = "ols" )
coef( translogZvar, which = "grid" )
coef( translogZvar )
coef( summary( translogZvar ), which = "ols" )
coef( summary( translogZvar ) )
vcov( translogZvar )
logLik( translogZvar, which = "ols" )
logLik( translogZvar )
print( summary( translogZvar ) )
efficiencies( translogZvar )
efficiencies( translogZvar, asInData = TRUE )
translogZvarEla <- elas( translogZvar )
print( translogZvarEla ) 
attributes( translogZvarEla )$variance
attributes( translogZvarEla )$stdDev
print.default( translogZvar )

