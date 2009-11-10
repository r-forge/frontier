## Likelihood Ratio Test
lrtest.frontier <- function( object, ... ) {

   thisCall <- match.call()

   if( ! "frontier" %in% class( object ) ){
      stop( "argument 'object' must be of class 'frontier'" )
   }

   ## list of objects in ...
   objectList <- list( ... )

   ## test if there are further objects in ...
   if( length( objectList ) < 1 ){
      stop( "at least one further argument ('...') must be provided" )
   }

   ## test if all objects are of class "frontier"
   for( i in 1:length( objectList ) ) {
      if( ! "frontier" %in% class( objectList[[ i ]] ) ){
         stop( "all further arguments ('...') must be of class 'frontier'" )
      }
   }

   ## get and save the names of the models (objects)
   object$lrtest.frontier.name <- deparse( substitute( object ) )
   dotsNames <- as.list( thisCall )[ -1 ]
   dotsNames$object <- NULL
   for( i in 1:length( objectList ) ){
      objectList[[ i ]]$lrtest.frontier.name <- deparse( dotsNames[[ i ]] )
   }
   ## function to extract the names of the objects
   extractName <- function( object ){
      return( object$lrtest.frontier.name )
   }

   ## test if all models are of the same model type
   for( i in 1:length( objectList ) ) {
      if( object$modelType != objectList[[ i ]]$modelType ){
         stop( "all models must be of the same type",
            " but model '", extractName( object ), "' is an",
            ifelse( object$modelType == 1,
               " 'Error Components Frontier' (ECF)",
               " 'Efficiency Effects Frontier' (EEF)" ),
            ", while model '", extractName( objectList[[ i ]] ), "' is an",
            ifelse( objectList[[ i ]]$modelType == 1,
               " 'Error Components Frontier' (ECF)",
               " 'Efficiency Effects Frontier' (EEF)" ) )
      }
   }

   ## do the LR tests
   result <- do.call( lrtest.default,
      c( list( object = object ), objectList, list( name = extractName ) ) )

   for( i in 2:nrow( result ) ){
      if( ( result[ i, "#Df" ] - result[ i - 1, "#Df" ] ) *
            ( result[ i, "LogLik" ] - result[ i - 1, "LogLik" ] ) < 0 ) {
         if( result[ i, "LogLik" ] > result[ i - 1, "LogLik" ] ) {
            compareLikelihood <- "larger"
            compareDf <- "less"
         } else {
            compareLikelihood <- "smaller"
            compareDf <- "more"
         }
         warning( "model '", i, "' has a ", compareLikelihood,
            " log-likelihood value than the ", compareDf,
            " restricted model '", i - 1, "'" )
      }
   }

   return( result )
}
