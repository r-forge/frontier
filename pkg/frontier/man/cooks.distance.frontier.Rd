\name{cooks.distance.frontier}
\alias{cooks.distance.frontier}

\title{Pseudo-Cook's Distance of Stochastic Frontier Models}

\description{
   This method returns the Pseudo-Cook's distances
   from stochastic frontier models
   estimated with the \pkg{frontier} package 
   (e.g., function \code{\link{sfa}}).
}

\usage{
\method{cooks.distance}{frontier}( model, target = "predict",
   progressBar = TRUE, \dots )
}

\arguments{
   \item{model}{a stochastic frontier model
      estimated with the \pkg{frontier} package 
      (e.g. function \code{\link{sfa}}).}
   \item{target}{character string; 
      if \code{"predict"}, the returned values indicate 
      the influence of individual observations on the predicted values;
      if \code{"efficiencies"}, the returned values indicate 
      the influence of individual observations on the efficiency estimates.}
   \item{progressBar}{logical. Should a progress bar be displayed
      while the Cook's distances are obtained?}
   \item{\dots}{currently ignored.}
}

\value{
   A vector of the Pseudo-Cook's distances for each observation
   that was used in the estimation that is provided as argument \code{model}.
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
   
   # Pseudo-Cook's distances for predicted values
   cooks.distance( cobbDouglas )

   # Pseudo-Cook's distances for efficiency estimates
   cooks.distance( cobbDouglas, "efficiencies" )
}

\keyword{methods}
