library( frontier )
options( digits = 5 )

data( front41Data )
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
print( summary( a1 ) )
print.default( a1 )

## cross-section data, error components frontier, mu != 0
a2 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), mu = TRUE ,code="R")
print( a2 )
coef( a2, which = "start" )
coef( a2, which = "ols" )
coef( a2, which = "grid" )
coef( a2 )
coef( summary( a2 ), which = "ols" )
coef( summary( a2 ) )
vcov( a2 )
print( summary( a2 ) )
print.default( a2 )

## cross-section data, error components frontier, starting values
a5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), mu = TRUE,
   startVal = c( 0.5, 0.3, 0.5, -1 , 0.5, 0.9) , code = "R" )
print( a5 )
coef( a5, which = "start" )
coef( a5, which = "ols" )
coef( a5, which = "grid" )
coef( a5 )
coef( summary( a5 ), which = "ols" )
coef( summary( a5 ) )
vcov( a5 )
print( summary( a5 ) )
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
print.default( aa1 )

## cross-section data, efficiency effects frontier, mu != 0
aa2 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", mu = TRUE, code = "R" )
print( aa2 )
coef( aa2, which = "start" )
coef( aa2, which = "ols" )
coef( aa2, which = "grid" )
coef( aa2 )
coef( summary( aa2 ), which = "ols" )
coef( summary( aa2 ) )
vcov( aa2 )
print( summary( aa2 ) )
print.default( aa2 )

## cross-section data, efficiency effects frontier, starting values
aa5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), zNames = "firmNo", mu = TRUE,
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
print.default( aa5 )


data( riceProdPhil )
riceProdPhil <- plm.data( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )
riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
riceProdPhil$lNPK   <- log( riceProdPhil$NPK )

## panel data, error components frontier
b1 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ), code = "R" )
print( b1 )
coef( b1, which = "start" )
coef( b1, which = "ols" )
coef( b1, which = "grid" )
coef( b1 )
coef( summary( b1 ), which = "ols" )
coef( summary( b1 ) )
vcov( b1 )
print( summary( b1 ) )
print.default( b1 )

## panel data, error components frontier, mu != 0
b2 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   mu = TRUE, code = "R" )
print( b2 )
coef( b2, which = "start" )
coef( b2, which = "ols" )
coef( b2, which = "grid" )
coef( b2 )
coef( summary( b2 ), which = "ols" )
coef( summary( b2 ) )
vcov( b2 )
print( summary( b2 ) )
print.default( b2 )

## panel data, error components frontier, eta != 0
b3 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   eta = TRUE )
print( b3 )
coef( b3, which = "start" )
coef( b3, which = "ols" )
coef( b3, which = "grid" )
coef( b3 )
coef( summary( b3 ), which = "ols" )
coef( summary( b3 ) )
vcov( b3 )
print( summary( b3 ) )
print.default( b3 )

## panel data, error components frontier, mu != 0, eta != 0
b4 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   mu = TRUE, eta = TRUE )
print( b4 )
coef( b4, which = "start" )
coef( b4, which = "ols" )
coef( b4, which = "grid" )
coef( b4 )
coef( summary( b4 ), which = "ols" )
coef( summary( b4 ) )
vcov( b4 )
print( summary( b4 ) )
print.default( b4 )

## panel data, efficiency effects frontier
b5 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), code="R" )
print( b5 )
coef( b5, which = "start" )
coef( b5, which = "ols" )
coef( b5, which = "grid" )
coef( b5 )
coef( summary( b5 ), which = "ols" )
coef( summary( b5 ) )
vcov( b5 )
print( summary( b5 ) )
print.default( b5 )

## panel data, efficiency effects frontier, mu != 0
b6 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), mu = TRUE, code = "R" )
print( b6 )
coef( b6, which = "start" )
coef( b6, which = "ols" )
coef( b6, which = "grid" )
coef( b6 )
coef( summary( b6 ), which = "ols" )
coef( summary( b6 ) )
vcov( b6 )
print( summary( b6 ) )
print.default( b6 )

## panel data, efficiency effects frontier, starting values
b7 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   mu = TRUE, eta = TRUE,
   startVal = c( -1, 0.3, 0.3, 0.3, 0.2, 0.5, -0.3, 0.1 ) )
print( b7 )
coef( b7, which = "start" )
coef( b7, which = "ols" )
coef( b7, which = "grid" )
coef( b7 )
coef( summary( b7 ), which = "ols" )
coef( summary( b7 ) )
vcov( b7 )
print( summary( b7 ) )
print.default( b7 )

## panel data, efficiency effects frontier, mu != 0, starting values
b8 <- frontier( data = riceProdPhil,
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = c( "EDYRS", "BANRAT" ), mu = TRUE, 
   startVal = c( -1, 0.3, 0.3, 0.3, -3, -0.1, -4, 2, 0.8 ) )
print( b8 )
coef( b8, which = "start" )
coef( b8, which = "ols" )
coef( b8, which = "grid" )
coef( b8 )
coef( summary( b8 ), which = "ols" )
coef( summary( b8 ) )
vcov( b8 )
print( summary( b8 ) )
print.default( b8 )

data( prodAgrBrazil)
prodAgrBrazil$lProd <- log(prodAgrBrazil$production)
prodAgrBrazil$lLabor <- log(prodAgrBrazil$labor)
prodAgrBrazil$lArea <- log(prodAgrBrazil$area)
prodAgrBrazil$lOther <- log(prodAgrBrazil$other_exp)
prodAgrBrazil$lCapital <- log(prodAgrBrazil$capital)

c1R <- frontier( data = prodAgrBrazil, yName = "lProd", 
    xNames = c( "lArea", "lLabor", "lOther", "lCapital" ), 
    zNames = c( "idh", "gip_pc" ), mu = TRUE, code="R" )
print( c1R )
coef( c1R, which = "start" )
coef( c1R, which = "ols" )
coef( c1R, which = "grid" )
coef( c1R )
coef( summary( c1R ), which = "ols" )
coef( summary( c1R ) )
vcov( c1R )
print( summary( c1R ) )
print.default( c1R )

c1F <- frontier( data = prodAgrBrazil, yName = "lProd", 
    xNames = c( "lArea", "lLabor", "lOther", "lCapital" ), 
    zNames = c( "idh", "gip_pc" ), mu = TRUE, code="Fortran")
print( c1F )
coef( c1F, which = "start" )
coef( c1F, which = "ols" )
coef( c1F, which = "grid" )
coef( c1F )
coef( summary( c1F ), which = "ols" )
coef( summary( c1F ) )
vcov( c1F )
print( summary( c1F ) )
print.default( c1F )

c1RR <- frontier( data = prodAgrBrazil, yName = "lProd", 
    xNames = c( "lArea", "lLabor", "lOther", "lCapital" ), 
    zNames = c( "idh", "gip_pc" ), mu = TRUE, code="R", evalLogLike=TRUE, 
    startVal=c1R$mleParam )

c1RF <- frontier( data = prodAgrBrazil, yName = "lProd", 
    xNames = c( "lArea", "lLabor", "lOther", "lCapital" ), 
    zNames = c( "idh", "gip_pc" ), mu = TRUE, code="Fortran", evalLogLike=TRUE, 
    startVal=c1R$mleParam)

c1FR <- frontier( data = prodAgrBrazil, yName = "lProd", 
    xNames = c( "lArea", "lLabor", "lOther", "lCapital" ), 
    zNames = c( "idh", "gip_pc" ), mu = TRUE, code="R", evalLogLike=TRUE, 
    startVal=c1F$mleParam )

c1FF <- frontier( data = prodAgrBrazil, yName = "lProd", 
    xNames = c( "lArea", "lLabor", "lOther", "lCapital" ), 
    zNames = c( "idh", "gip_pc" ), mu = TRUE, code="Fortran", evalLogLike=TRUE, 
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
   xNames = c( "logCapital", "logLabour" ) )
print( translog )
coef( translog, which = "start" )
coef( translog, which = "ols" )
coef( translog, which = "grid" )
coef( translog )
coef( summary( translog ), which = "ols" )
coef( summary( translog ) )
vcov( translog )
print( summary( translog ) )
print.default( translog )

## cross-section data, error components frontier, translog, shifter
translogShift <- frontierQuad( yName = "logOutput",
   xNames = c( "logCapital", "logLabour" ), shifterNames = "firmNo",
   data = front41Data )
print( translogShift )
coef( translogShift, which = "start" )
coef( translogShift, which = "ols" )
coef( translogShift, which = "grid" )
coef( translogShift )
coef( summary( translogShift ), which = "ols" )
coef( summary( translogShift ) )
vcov( translogShift )
print( summary( translogShift ) )
print.default( translogShift )

## cross-section data, efficiency effects frontier, translog
translogZvar <- frontierQuad( yName = "logOutput",
   xNames = c( "logCapital", "logLabour" ), zNames = "firmNo",
   data = front41Data )
print( translogZvar )
coef( translogZvar, which = "start" )
coef( translogZvar, which = "ols" )
coef( translogZvar, which = "grid" )
coef( translogZvar )
coef( summary( translogZvar ), which = "ols" )
coef( summary( translogZvar ) )
vcov( translogZvar )
print( summary( translogZvar ) )
print.default( translogZvar )

