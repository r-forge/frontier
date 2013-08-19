\name{coef.frontier}
\alias{coef.frontier}
\title{coef method for class frontier}

\description{
Extract the coefficients from stochastic frontier models
returned by \code{\link{frontier}}.
}

\usage{
\method{coef}{frontier}( object, which = "mle", extraPar = FALSE, \dots )
}

\arguments{
   \item{object}{an object of class \code{frontier}
      (returned by the function \code{\link{frontier}}).}
   \item{which}{character string. Which coefficients should be returned?
      ('start' for starting values provided by the user,
      'ols' for coefficients estimated by OLS,
      'grid' for coefficients obtained by the grid search,
      or 'mle' for coefficients estimated by Maximum Likelihood).}
   \item{extraPar}{logical. If \code{TRUE}, additional parameters are returned:
      \code{sigmaSqU} = \code{sigmaSq} * \code{gamma} 
      (with \eqn{u} ~ \eqn{N^+}{N+}( \code{mu}, \code{sigmaSqU} )) 
      and \code{sigmaSqV} = \code{sigmaSq} * ( 1 - \code{gamma} )
      (with \eqn{v} ~ N( 0, \code{sigmaSqV} )).
      Please note that \code{sigmaSqU} is not the variance of \eqn{u}.}
   \item{\dots}{currently unused.}
}

\value{
   \code{coef.frontier} returns a named vector of the coefficients.
}

\seealso{\code{\link{coef.summary.frontier}} and \code{\link{sfa}}.}

\author{Arne Henningsen}

\examples{
   # example included in FRONTIER 4.1
   data( front41Data )

   sfaResult <- sfa( log( output ) ~ log( capital ) + log( labour ),
      data = front41Data )
   coef( sfaResult, which = "ols" )
   coef( sfaResult, which = "grid" )
   coef( sfaResult )
}

\keyword{models}
