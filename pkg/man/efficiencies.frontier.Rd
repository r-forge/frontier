\name{efficiencies.frontier}
\alias{efficiencies.frontier}

\title{Returning Efficiency Estimates}

\description{
   This method returns efficiency estimates from stochastic frontier models
   estimated with \code{\link{frontier}}.
}

\usage{
\method{efficiencies}{frontier}( object, \dots )
}

\arguments{
   \item{object}{a stochastic frontier model
      returned by \code{\link{frontier}}.}
   \item{\dots}{currently ignored.}
}

\value{
   A matrix of efficiency estimates.
   Each row corresponds to a firm (cross-section unit)
   and each column corresponds to a time period
   (only if efficiency estimates differ between time periods).
}

\author{Arne Henningsen}

\seealso{\code{\link{frontier}}, \code{\link{efficiencies}}.}

\examples{
   # rice producers in the Philippines (panel data)
   data( riceProdPhil )
   riceProdPhil <- plm.data( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )
   riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
   riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
   riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
   riceProdPhil$lNPK   <- log( riceProdPhil$NPK )

   # Error Components Frontier (Battese & Coelli 1992), no time effect
   rice <- frontier( yName = "lPROD",
      xNames = c( "lAREA", "lLABOR", "lNPK" ), data = riceProdPhil )
   efficiencies( rice )

   # Error Components Frontier (Battese & Coelli 1992), with time effect
   riceTime <- frontier( yName = "lPROD",
      xNames = c( "lAREA", "lLABOR", "lNPK" ), data = riceProdPhil, eta = TRUE )
   efficiencies( riceTime )
}

\keyword{methods}
