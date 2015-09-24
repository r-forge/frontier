\name{sfaFLW}
\alias{sfaFLW}
\title{Semiparameric SFA of Fan, Lee, and Weersink (1996)}

\description{
  Estimate the Semiparameric Stochastic Frontier Model
  suggested by Fan, Lee, and Weersink (1996).
}

\usage{
sfaFLW( formula, data = sys.frame( sys.parent() ),
  regtype = "lc", bw.sel = "cv.ls" )
}

\arguments{
  \item{formula}{a symbolic description of the model to be estimated.}
  \item{data}{optional. a data frame that contains the data.}
  \item{regtype}{regression type,
    passed to \code{\link[np]{npregbw}}.}
  \item{bw.sel}{bandwidth selection method:
    if it is \code{"rot"},
    FLW's rule-of-thumb is applied;
    otherwise, this argument is passed
    further to \code{\link[np]{npregbw}}.}
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

  # semiparametric local-constant Cobb-Douglas production frontier
  FLW_Result <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
    data = front41Data )
  FLW_Result
}

\keyword{models}
