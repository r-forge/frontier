\name{residuals.sfaFLW}
\alias{residuals.sfaFLW}

\title{Returning Residuals}

\description{
   This method returns the residuals
   from semiparametric stochastic frontier models 
   estimated with function \code{\link{sfaFLW}}.
}

\usage{
\method{residuals}{sfaFLW}( object, which = "final", \dots )
}

\arguments{
   \item{object}{a semiparametric stochastic frontier model
      estimated with function \code{\link{sfaFLW}}.}
   \item{which}{If \code{"first"},
      the residuals of the nonparametric regression (first step)
      are returned;
      if \code{"final"} (the default),
      the final bias-corrected residuals are returned}
   \item{\dots}{currently ignored.}
}

\value{
   A numeric vector containing the residuals.
}

\author{Arne Henningsen}

\seealso{\code{\link{sfaFLW}}, \code{\link{residuals}},
  \code{\link{fitted.sfaFLW}}.}

\examples{
  # example included in FRONTIER 4.1 (cross-section data)
  data( front41Data, package = "frontier" )

  # semiparametric local-constant Cobb-Douglas production frontier
  FLW_Result <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
    data = front41Data )
    
  # residuals of the nonparametric regression (first step)
  residuals( FLW_Result, which = "first" )

  # bias-corrected residuals
  residuals( FLW_Result )
}

\keyword{methods}
