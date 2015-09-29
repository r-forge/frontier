\name{sfaFLW}
\alias{sfaFLW}
\title{Semiparameric SFA of Fan, Lee, and Weersink (1996)}

\description{
  Estimate the Semiparameric Stochastic Frontier Model
  suggested by Fan, Lee, and Weersink (1996).
}

\usage{
sfaFLW( formula, data = sys.frame( sys.parent() ),
  bwmethod = "cv.ls", npArg = list() )
}

\arguments{
  \item{formula}{a symbolic description of the model to be estimated.}
  \item{data}{optional data frame that contains the data.}
  \item{bwmethod}{bandwidth selection method:
    if it is \code{"rot"},
    FLW's rule-of-thumb is applied;
    otherwise, this argument is passed
    further to \code{\link[np]{npregbw}}.}
  \item{npArg}{list of additional arguments
    that are passed to \code{\link[np]{npregbw}}.}
}

\value{
  \code{sfaFLW} returns a list of class \code{sfaFLW}
    containing following elements:
  \item{npreg}{object returned by \code{\link[np]{npreg}}
    containing the results of the nonparametric regression (first step).}
  \item{mu}{numeric scalar containing the bias correction
    \eqn{\mu = \sigma \sqrt{ ( 2 \lambda^2 / \pi ) / ( 1 + \lambda^2 ) }},
    obtained in the maximum likelihood estimation (second step).}
  \item{sigma.sq}{numeric scalar.
    \eqn{\sigma^2 = \sigma_u^2 + \sigma_v^2},
    obtained in the maximum likelihood estimation (second step).}
  \item{lambda}{numeric scalar.
    \eqn{\lambda = \sigma_u / \sigma_v},
    obtained in the maximum likelihood estimation (second step).}
  \item{sigma.u}{numeric scalar. \eqn{\sigma_u},
    obtained in the maximum likelihood estimation (second step).}
  \item{sigma.v}{numeric. \eqn{\sigma_v},
    obtained in the maximum likelihood estimation (second step).}
}

\references{
  Fan, Y., Q. Li, and A. Weersink (1996):
    Semiparametric Estimation of Stochastic Production Frontier Models.
    \emph{Journal of Business and Economic Statistics} 14, 460-468.
}

\seealso{
  \code{\link{residuals.sfaFLW}} for obtaining residuals,
  \code{\link{fitted.sfaFLW}} for obtaining fitted values,
  \code{\link{gradients.sfaFLW}} for obtaining gradients, and
  \code{\link[frontier]{sfa}} for parametric stochastic frontier analysis.
}

\author{Christopher F. Parmeter}

\examples{
  # example included in FRONTIER 4.1 (cross-section data)
  data( front41Data, package = "frontier" )

  # semiparametric local-constant Cobb-Douglas production frontier
  FLW_Result <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
    data = front41Data )
  FLW_Result

  # semiparametric local-linear Cobb-Douglas production frontier
  FLW_Result_ll <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
    data = front41Data, npArg = list( regtype = "ll" ) )
  FLW_Result_ll
}

\keyword{models}
