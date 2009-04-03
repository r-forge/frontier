frontier <- function(
      yName, xNames = NULL, zNames = NULL, data,
      showParNames = FALSE,
      firmNames = NULL,
      code="Fortran",
      modelType = ifelse( is.null( zNames ), 1, 2 ), 
      ineffDecrease = TRUE,
      logDepVar = TRUE,
      mu = FALSE,
      eta = FALSE,
      evalLogLike = FALSE,
      iprint = 0,
      indic = 1,
      tol = 0.00001,
      tol2 = 0.001,
      bignum = 1.0E+16,
      step1 = 0.00001,
      gridDouble = TRUE,
      gridno = 0.1,
      maxit = 1000,
      startVal = NULL ) {

   if( ! code %in% c("Fortran","R") ) {
         stop( "argument 'code' must be either 'Fortran' or '2'" )
   }
   if( code=="R" ) {
      if (!ineffDecrease) {
          stop("ineffDecrease cannot be false when code='R'");
      }
      if (!logDepVar) {
          stop("logDepVar cannot be false when code='R'");
      }
      if (eta) {
          stop( "eta cannot be TRUE when code='R'");
      }
   }
   if( !modelType %in% c( 1, 2 ) ) {
      stop( "argument 'modelType' must be either 1 or 2" )
   }
   if( !is.logical( ineffDecrease ) || length( ineffDecrease ) != 1 ) {
      stop( "argument 'ineffDecrease' must be a single logical value" )
   }
   if( !is.logical( logDepVar ) ) {
      stop( "argument 'logDepVar' must be logical" )
   }
   if( !is.logical( mu ) ) {
      stop( "argument 'mu' must be logical" )
   }
   if( !is.logical( eta ) ) {
      stop( "argument 'eta' must be logical" )
   }
   if (evalLogLike && (is.null(startVal) || length(startVal)==0)) {
      stop( "startVal must be provided when evalLogLike is TRUE" );
   }
   # iprint
   if( !is.numeric( iprint ) ) {
      stop( "argument 'iprint' must be numeric" )
   } else if( iprint != round( iprint ) ) {
      stop( "argument 'iprint' must be an iteger" )
   } else if( iprint < 0 ) {
      stop( "argument 'iprint' must be non-negative" )
   }
   iprint <- as.integer( iprint )
   # indic
   if( !is.numeric( indic ) ) {
      stop( "argument 'indic' must be numeric" )
   } else if( indic != round( indic ) ) {
      stop( "argument 'indic' must be an integer" )
   }
   indic <- as.integer( indic )
   # tol
   if( !is.numeric( tol ) ) {
      stop( "argument 'tol' must be numeric" )
   } else if( tol < 0 ) {
      stop( "argument 'tol' must be non-negative" )
   }
   # tol2
   if( !is.numeric( tol2 ) ) {
      stop( "argument 'tol2' must be numeric" )
   } else if( tol2 < 0 ) {
      stop( "argument 'tol2' must be non-negative" )
   }
   # bignum
   if( !is.numeric( bignum ) ) {
      stop( "argument 'bignum' must be numeric" )
   } else if( bignum <= 0 ) {
      stop( "argument 'bignum' must be positive" )
   }
   # step1
   if( !is.numeric( step1 ) ) {
      stop( "argument 'step1' must be numeric" )
   } else if( step1 <= 0 ) {
      stop( "argument 'step1' must be positive" )
   }
   # gridDouble (igrid2)
   if( !is.logical( gridDouble ) || length( gridDouble ) != 1 ) {
      stop( "argument 'gridDouble' must be a single logical value" )
   }
   # gridno
   if( !is.numeric( gridno ) ) {
      stop( "argument 'gridno' must be numeric" )
   } else if( gridno <= 0 ) {
      stop( "argument 'gridno' must be positive" )
   }
   # maxit
   if( !is.numeric( maxit ) ) {
      stop( "argument 'maxit' must be numeric" )
   } else if( maxit != round( maxit ) ) {
      stop( "argument 'maxit' must be an integer" )
   } else if( maxit <= 0 ) {
      stop( "argument 'maxit' must be positive" )
   }
   maxit <- as.integer( maxit )
      
   if( "plm.dim" %in% class( data ) ) {
      nn <- length( unique( data[[ 1 ]] ) )
      nt <- length( unique( data[[ 2 ]] ) )
   } else {
      nn <- nrow( data )
      nt <- 1
   }
   nob <- nrow( data )
   nXvars <- length( xNames )
   nb <- nXvars
   nZvars <- length( zNames )
   if( modelType == 2 ) {
      eta <- nZvars
   }

   # cross section and time period identifier
   if( "plm.dim" %in% class( data ) ) {
      dataTable <- matrix( data[[ 1 ]], ncol = 1 )
      dataTable <- cbind( dataTable, data[[ 2 ]] )
   } else {
      dataTable <- matrix( 1:nrow( data ), ncol = 1 )
      dataTable <- cbind( dataTable, rep( 1, nrow( dataTable ) ) )
   }

   # endogenous variable
   if ( !any( yName == colnames(data) ) ) {
       stop(paste("Y column",yName,"not found"))
   }
   dataTable <- cbind( dataTable, data[[ yName ]] )

   # exogenous variables
   if (showParNames) {
      paramNames <- "b_const";
   } else {
      paramNames <- "beta_0";
   }
   if( nXvars > 0 ) {
      for( i in 1:nXvars ) {
         if ( !any( xNames[i] == colnames(data) ) ) {
              stop(paste("X column",xNames[i],"not found"))
         }
         dataTable <- cbind( dataTable, data[[ xNames[ i ] ]] )
         if (showParNames) {
            paramNames <- c( paramNames, paste( "b", xNames[i], sep = "_" ) )
         } else {
            paramNames <- c( paramNames, paste( "beta", i, sep = "_" ) )
         }
      }
   }

   # variables explaining the efficiency level
   if( nZvars > 0 ) {
      for( i in 1:nZvars ) {
         if ( !any( zNames[i] == colnames(data) ) ) {
              stop(paste("Z column",zNames[i],"not found"))
         }
         dataTable <- cbind( dataTable, data[[ zNames[ i ] ]] )
      }
   }
   
   if ( !is.null(firmNames) ) {
       if ( !any( firmNames == colnames(data) ) ) {
            stop(paste("firmNames column", firmNames, "not found") );
       }
   }

   nParamTotal <- nb + 3 + mu + eta
   if( is.null( startVal ) ) {
      startVal <- 0
   } else {
      if( nParamTotal != length( startVal ) ) {
         stop( "wrong number of starting values (you provided ",
            length( startVal ), " starting values but the model has ",
            nParamTotal, " parameters)" )
      }
   }
   if (code=="Fortran" && !evalLogLike) {
      returnObj <- .Fortran( "front41",
          modelType = as.integer( modelType ),
          ineffDecrease = as.integer( !ineffDecrease + 1 ),
          logDepVar = as.integer( logDepVar ),
          nn = as.integer( nn ),
          nt = as.integer( nt ),
          nob = as.integer( nob ),
          nb = as.integer( nb ),
          mu = as.integer( mu ),
          eta = as.integer( eta ),
          iprint = as.integer( iprint ),
          indic = as.integer( indic ),
          tol = as.double( tol ),
          tol2 = as.double( tol2 ),
          bignum = as.double( bignum ),
          step1 = as.double( step1 ),
          gridDouble = as.integer( gridDouble ),
          gridno = as.double( gridno ),
          maxit = as.integer( maxit ),
          nStartVal = as.integer( length( startVal ) ),
          startVal = as.double( startVal ),
          nRowData = as.integer( nrow( dataTable ) ),
          nColData = as.integer( ncol( dataTable ) ),
          dataTable = matrix( as.double( dataTable ), nrow( dataTable ),
            ncol( dataTable ) ),
          nParamTotal = as.integer( nParamTotal ),
          olsParam = as.double( rep( 0, nParamTotal ) ),
          olsStdEr = as.double( rep( 0, nParamTotal ) ),
          olsLogl = as.double( 0 ),
          gridParam = as.double( rep( 0, nParamTotal ) ),
          mleParam = as.double( rep( 0, nParamTotal ) ),
          mleCov = matrix( as.double( 0 ), nParamTotal, nParamTotal ),
          mleLogl = as.double( 0 ),
          lrTestVal = as.double( 0 ),
          lrTestDf = as.integer( 0 ),
          nIter = as.integer( 0 ),
          effic = matrix( as.double( 0 ), nn, nt ) )
      returnObj$nStartVal <- NULL
      returnObj$nRowData <- NULL
      returnObj$nColData <- NULL
      returnObj$nParamTotal <- NULL
      returnObj$ineffDecrease <- as.logical( 2 - returnObj$ineffDecrease )
      returnObj$gridDouble <- as.logical( returnObj$gridDouble )
      returnObj$olsParam <- returnObj$olsParam[ 1:( nb + 2 ) ]
      returnObj$olsStdEr <- returnObj$olsStdEr[ 1:( nb + 1 ) ]
     } else {  # code = "R"
       dataTable <- cbind(matrix( as.double( dataTable ), nrow( dataTable ),
            ncol( dataTable ) ),rep(1,nrow(dataTable)));
       colnames(dataTable) <- c("seq","t",yName,xNames,zNames,"ones");
       
        y <- dataTable[,yName];
        x <- matrix(dataTable[,c("ones",xNames)],length(y),1+length(xNames));
        #colnames(x) <- c("ones",xNames);
        #colnames(x) <- paste("beta", 1:ncol(x)-1, sep="_");
        z <- matrix(0,nrow(x),0);
        if (length(zNames)>0)  {
             z <-  matrix(dataTable[,zNames],length(y),length(zNames))
             #colnames(z) <- paste("delta", 1:ncol(z), sep="_");
             #colnames(z) <- length(zNames);
        } 
        
        dataR <- list(y=y, x=x, z=z);
        returnObj= list(modelType = modelType,
            ineffDecrease = ineffDecrease,
            logDepVar = as.integer(logDepVar),
            nn = nn,
            nt = nt,
            nob = nob,
            nb = nb,
            mu = as.integer(mu),
            eta = as.integer(eta),
            iprint = iprint,
            indic = indic,
            tol = tol,
            tol2 = tol2,
            bignum = bignum,
            step1 = step1,
            gridDouble = gridDouble,
            gridno = gridno,
            maxit = maxit,
            startVal = startVal,
            dataTable = dataTable);
        rResult <- frontierR(dataR, 
            modelType = modelType,
            code = code,
            mu = mu,
            evalLogLike = evalLogLike,
            gridDouble = gridDouble,
            gridno = gridno, 
            iterlim = maxit,
            startVal = startVal);
        for (i in 1:length(rResult)) {
            returnObj[[ names(rResult)[i] ]] = rResult[[i]]
        }
        returnObj$lrTestDf = as.integer(0)
   }
   
   if (!evalLogLike && maxit==returnObj$nIter) {
      stop("Maximum number of iterations reached");
   }
   
   returnObj$code <- code;
    if( length( startVal ) == 1 ){
        if( modelType == 1 ) {
          returnObj$gridParam <- returnObj$gridParam[ 1:( nb + 3 ) ]
        } else {
          returnObj$gridParam <- returnObj$gridParam[
              c( 1:( nb + 1 ), ( nParamTotal - 1 ):nParamTotal ) ]
        }
    } else {
        returnObj$gridParam <- NULL
    }
    if( modelType == 1 && eta == FALSE ) {
        returnObj$effic <- returnObj$effic[ , 1, drop = FALSE ]
    }
    if( modelType == 2 ) {
        if( mu ){
            if (showParNames) {
                paramNames <- c( paramNames, "d_const" ) 
            } else {
                paramNames <- c( paramNames,  "delta_0" ) 
            }
        }
        if( nZvars > 0 ) {
            if (showParNames) {
                paramNames <- c( paramNames, paste( "d", zNames, sep="_") )
            } else {
                paramNames <- c( paramNames, paste( "delta", c( 1:nZvars ), sep = "_" ) ) 
            }
        }
    }
    
    if( length( startVal ) == 1 ){
          returnObj$startVal <- NULL
     }
      paramNames <- c( paramNames, "sigma-sq", "gamma" )
    if( modelType == 1 ) {
        if( mu ){
          paramNames <- c( paramNames, "mu" )
        }
        if( eta ){
          paramNames <- c( paramNames, "eta" )
        }
    }
    
    names( returnObj$olsParam ) <- c( paramNames[ 1:( nb + 1 ) ],
        "sigma-sq" )
    names( returnObj$olsStdEr ) <- paramNames[ 1:( nb + 1 ) ]
    if( !is.null( returnObj$gridParam ) ) {
        names( returnObj$gridParam ) <- c( paramNames[ 1:( nb + 1 ) ], 
          "sigma-sq", "gamma" )
    }
    if (!evalLogLike) {
        names( returnObj$mleParam ) <- paramNames
        rownames( returnObj$mleCov ) <- paramNames
        colnames( returnObj$mleCov ) <- paramNames
    }
    if( !is.null( returnObj$startVal ) ) {
        names( returnObj$startVal ) <- paramNames
    }
    
    if ( !is.null(firmNames) ) {
        rownames( returnObj$effic ) <- data[[ firmNames ]]
    }

   class( returnObj ) <- "frontier"
   return( returnObj )
}
