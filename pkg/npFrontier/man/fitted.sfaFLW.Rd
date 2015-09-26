\name{fitted.sfaFLW}
\alias{fitted.sfaFLW}

\title{Returning Fitted Values}

\description{
   This method returns the fitted values
   of semiparametric stochastic frontier models 
   estimated with function \code{\link{sfaFLW}}.
}

\usage{
\method{fitted}{sfaFLW}( object, which = "frontier", \dots )
}

\arguments{
   \item{object}{a semiparametric stochastic frontier model
      estimated with function \code{\link{sfaFLW}}.}
   \item{which}{If \code{"first"},
      the fitted values of the nonparametric regression (first step)
      are returned;
      if \code{"frontier"} (the default),
      the (bias-corrected) fitted values of the frontier function
      are returned.}
   \item{\dots}{currently ignored.}
}

\value{
   A numeric vector containing the fitted values.
}

\author{Arne Henningsen}

\seealso{\code{\link{sfaFLW}}, \code{\link{fitted}}.}

\examples{
  # example included in FRONTIER 4.1 (cross-section data)
  data( front41Data, package = "frontier" )

  # semiparametric local-constant Cobb-Douglas production frontier
  FLW_Result <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
    data = front41Data )
    
  # fitted values of the nonparametric regression (first step)
  fitted( FLW_Result, which = "first" )

  # bias-corrected fitted values of the frontier function
  fitted( FLW_Result )
}

\keyword{methods}
