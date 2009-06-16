library( frontier )

## data set of rice producers in the Philippines
data( riceProdPhil )
riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
riceProdPhil$lNPK   <- log( riceProdPhil$NPK )
riceProdPhil$farm <-
   paste( "F_", ifelse( riceProdPhil$FMERCODE > 9, "", "0" ),
   riceProdPhil$FMERCODE, sep = "" )
riceProdPhil$year <- riceProdPhil$YEARDUM + 1998
riceProdPhil <- plm.data( riceProdPhil, c( "farm", "year" ) )


########## cross-section data #############

## without mu / zIntercept
# Error Components Frontier (ECF)
bb5ecf <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ) )

# Efficiency Effects Frontier (EEF)
bb5eef <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zNames = NA )

# Comparisons
all.equal( coef( bb5ecf ), coef( bb5eef ) )
all.equal( vcov( bb5ecf ), vcov( bb5eef ) )
all.equal( efficiencies( bb5ecf ), efficiencies( bb5eef ) )


## with mu / zIntercept
# Error Components Frontier (ECF)
bb6ecf <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   truncNorm = TRUE, code = "R" )

# Efficiency Effects Frontier (EEF)
bb6eef <- frontier( data = as.data.frame( riceProdPhil ),
   yName = "lPROD", xNames = c( "lAREA", "lLABOR", "lNPK" ),
   zIntercept = TRUE, zNames = NA, code = "R" )

# Comparisons
all.equal( coef( bb6ecf ), coef( bb6eef )[ c( 1:4, 6:7, 5 ) ] )
all.equal( efficiencies( bb6ecf ), efficiencies( bb6eef ) )
