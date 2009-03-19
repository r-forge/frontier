library( frontier )
options( digits = 5 )

data( front41Data )
front41Data$logOutput  <- log( front41Data$output )
front41Data$logCapital <- log( front41Data$capital )
front41Data$logLabour  <- log( front41Data$labour )

a1 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), code = "R" )
coef( a1, which = "start" )
coef( a1, which = "ols" )
coef( a1, which = "grid" )
coef( a1 )
vcov( a1 )
print.default( a1 )

a2 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), mu = TRUE, code = "R" )
coef( a2, which = "start" )
coef( a2, which = "ols" )
coef( a2, which = "grid" )
coef( a2 )
vcov( a2 )
print.default( a2 )

a3 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), eta = TRUE, code = "R" )
coef( a3, which = "start" )
coef( a3, which = "ols" )
coef( a3, which = "grid" )
coef( a3 )
vcov( a3 )
print.default( a3 )

a4 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), mu = TRUE, eta = TRUE, code = "R" )
coef( a4, which = "start" )
coef( a4, which = "ols" )
coef( a4, which = "grid" )
coef( a4 )
vcov( a4 )
print.default( a4 )

a5 <- frontier( data = front41Data, "logOutput",
   c( "logCapital", "logLabour" ), mu = TRUE,
   startVal = c( 0.5, 0.3, 0.5, 0.5, 0.9, -1 ), code = "R" )
coef( a5, which = "start" )
coef( a5, which = "ols" )
coef( a5, which = "grid" )
coef( a5 )
vcov( a5 )
print.default( a5 )

# translog
translog <- frontierQuad( data = front41Data, yName = "logOutput",
   xNames = c( "logCapital", "logLabour" ), code = "R" )
coef( translog, which = "start" )
coef( translog, which = "ols" )
coef( translog, which = "grid" )
coef( translog )
vcov( translog )
print.default( translog )

# translog with shifter variable
front41Data$firmNo <- c( 1:nrow( front41Data ) )
translogShift <- frontierQuad( yName = "logOutput",
   xNames = c( "logCapital", "logLabour" ), shifterNames = "firmNo",
   data = front41Data, code = "R" )
coef( translogShift, which = "start" )
coef( translogShift, which = "ols" )
coef( translogShift, which = "grid" )
coef( translogShift )
vcov( translogShift )
print.default( translogShift )
