\name{efficiencies.frontier}
\alias{efficiencies.frontier}

\title{Returning Efficiency Estimates}

\description{
   This method returns efficiency estimates from stochastic frontier models
   estimated with \code{\link{frontier}}.
}

\usage{
\method{efficiencies}{frontier}( object, asInData = FALSE,
   logDepVar = TRUE, farrell = TRUE, \dots )
}

\arguments{
   \item{object}{a stochastic frontier model
      returned by \code{\link{frontier}}.}
   \item{asInData}{logical. If \code{TRUE},
      the efficiency estimates are returned in the same order
      as the corresponding observations in the data set
      used for the estimation
      (see section \sQuote{value} below).}
   \item{logDepVar}{logical. Is the dependent variable logged?}
   \item{farrell}{logical. If \code{TRUE}, Farrel-type efficiencies
      that have values between 0 and 1 are returned.
      If \code{FALSE}, the returned efficiencies have values larger than 
      or equal to one.
      The conversion between the two efficiency measures is done by taking
      the inverse.}
   \item{\dots}{currently ignored.}
}

\value{
   If argument \code{asInData} is \code{FALSE} (default),
   a matrix of efficiency estimates is returned,
   where each row corresponds to a firm (cross-section unit)
   and each column corresponds to a time period
   (only if efficiency estimates differ between time periods).

   If argument \code{asInData} is \code{TRUE},
   a vector of efficiency estimates is returned,
   where the efficiency estimates are in the same order
   as the corresponding observations in the data set
   used for the estimation.
}

\author{Arne Henningsen \email{arne.henningsen@gmail.com}}

\seealso{\code{\link{sfa}}, \code{\link{summary.frontier}},
   and \code{\link{efficiencies}}.}

\examples{
   # rice producers in the Philippines (panel data)
   data( riceProdPhil )
   riceProdPhil <- plm.data( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )

   # Error Components Frontier (Battese & Coelli 1992), no time effect
   rice <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
      data = riceProdPhil )
   efficiencies( rice )
   riceProdPhil$efficiencies <- efficiencies( rice, asInData = TRUE )

   # Error Components Frontier (Battese & Coelli 1992), with time effect
   riceTime <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
      data = riceProdPhil, timeEffect = TRUE )
   efficiencies( riceTime )
   riceProdPhil$efficienciesTime <- efficiencies( riceTime, asInData = TRUE )
}

\keyword{methods}
