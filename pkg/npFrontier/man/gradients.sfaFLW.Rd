\name{gradients.sfaFLW}
\alias{gradients.sfaFLW}

\title{Returning Gradients}

\description{
   This method returns the gradients
   of the dependent variable with respect to the explanatory variables
   obtained from semiparametric stochastic frontier models 
   estimated with function \code{\link{sfaFLW}}.
}

\usage{
\method{gradients}{sfaFLW}( x, \dots )
}

\arguments{
   \item{x}{a semiparametric stochastic frontier model
      estimated with function \code{\link{sfaFLW}}.}
   \item{\dots}{currently ignored.}
}

\value{
   A numeric matrix containing the gradients
   obtained in the nonparametric regression (first step).
   Each row corresponds to one observation
   and each column corresponds to one explanatory variable.
}

\author{Arne Henningsen}

\seealso{\code{\link{sfaFLW}}, \code{\link[np]{gradients}},
  \code{\link{fitted.sfaFLW}}.}

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
