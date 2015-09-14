\name{FLW}
\alias{FLW}
\title{Semiparameric SFA of Fan, Lee, and Weersink (1996)}

\description{
  Estimate the Semiparameric Stochastic Frontier Model
  suggested by Fan, Lee, and Weersink (1996).
}

\usage{
FLW( y, x, regtype = "lc", bw.sel = "cv.ls" )
}

\arguments{
  \item{y}{vector of output quantities.}
  \item{x}{matrix of input quantities.}
  \item{regtype}{regression type,
    passed to \code{\link[np]{npregbw}}.}
  \item{bw.sel}{bandwidth selection method,
    passed to \code{\link[np]{npregbw}}.}
}

\references{
  Fan, Y., Q. Li, and A. Weersink (1996):
    Semiparametric Estimation of Stochastic Production Frontier Models.
    \emph{Journal of Business and Economic Statistics} 14, 460-468.
}

\seealso{\code{\link[frontier]{sfa}}.}

\author{Christopher F. Parmeter}

\examples{
  # example included in FRONTIER 4.1 (cross-section data)
  data( front41Data, package = "frontier" )

  # Cobb-Douglas production frontier
  FLW_Result <- FLW( log( front41Data$output ), 
    cbind( log( front41Data$capital ),log( front41Data$labour ) ) )
  FLW_Result
}

\keyword{models}
