\name{vcov.frontier}
\alias{vcov.frontier}
\title{vcov method for class frontier}

\description{
Extract the covariance matrix of the maximum likelihood coefficients
of a stochastic frontier model returned by \code{\link{frontier}}.
}

\usage{
\method{vcov}{frontier}( object, extraPar = FALSE, \dots )
}

\arguments{
   \item{object}{an object of class \code{frontier}
      (returned by the function \code{\link{frontier}}).}
   \item{extraPar}{logical. If \code{TRUE}, additional parameters are returned:
      \code{sigmaSqU} = \code{sigmaSq} * \code{gamma} 
      (with \eqn{u} ~ \eqn{N^+}{N+}( \code{mu}, \code{sigmaSqU} )) 
      and \code{sigmaSqV} = \code{sigmaSq} * ( 1 - \code{gamma} )
      (with \eqn{v} ~ N( 0, \code{sigmaSqV} )).
      Please note that \code{sigmaSqU} is not the variance of \eqn{u}.}
   \item{\dots}{currently unused.}
}

\value{
   \code{vcov.frontier} returns the covariance matrix 
   of the maximum likelihood coefficients.
}

\author{Arne Henningsen}

\examples{
   # example included in FRONTIER 4.1
   data( front41Data )

   sfaResult <- sfa( log( output ) ~ log( capital ) + log( labour ),
      data = front41Data )
   vcov( sfaResult )
}

\keyword{models}
