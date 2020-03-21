\name{cooks.distance.frontier}
\alias{cooks.distance.frontier}

\title{Cook's Distance of Stochastic Frontier Models}

\description{
   This method returns the Cook's distances
   from stochastic frontier models
   estimated with the \pkg{frontier} package 
   (e.g. function \code{\link{sfa}}).
}

\usage{
\method{cooks.distance}{frontier}( model, progressBar = TRUE, \dots )
}

\arguments{
   \item{model}{a stochastic frontier model
      estimated with the \pkg{frontier} package 
      (e.g. function \code{\link{sfa}}).}
   \item{progressBar}{logical. Should a progress bar be displayed
      while the Cook's distances are obtained?}
   \item{\dots}{currently ignored.}
}

\value{
   A vector of the Cook's distances for each observation
   that was used in the estimation that returned argument \code{model}.
}

\author{Arne Henningsen}

\seealso{\code{\link{sfa}}, \code{\link{cooks.distance}}.}

\examples{
   # example included in FRONTIER 4.1 (cross-section data)
   data( front41Data )

   # Cobb-Douglas production frontier
   cobbDouglas <- sfa( log( output ) ~ log( capital ) + log( labour ),
      data = front41Data )
   summary( cobbDouglas )
   
   # Cook's distances
   cooks.distance( cobbDouglas )
}

\keyword{methods}
