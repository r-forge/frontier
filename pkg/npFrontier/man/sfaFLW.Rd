\name{sfaFLW}
\alias{sfaFLW}
\title{Semiparameric SFA of Fan, Lee, and Weersink (1996)}

\description{
  Estimate the Semiparameric Stochastic Frontier Model
  suggested by Fan, Lee, and Weersink (1996).
}

\usage{
sfaFLW( formula, data = sys.frame( sys.parent() ),
  bw.sel = "cv.ls", npArg = list() )
}

\arguments{
  \item{formula}{a symbolic description of the model to be estimated.}
  \item{data}{optional data frame that contains the data.}
  \item{bw.sel}{bandwidth selection method:
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
  \item{mu}{numeric scalar containing the bias correction
    \eqn{\mu = \sigma \sqrt{ ( 2 \lambda^2 / \pi ) / ( 1 + \lambda^2 ) }},
    obtained in the maximum likelihood estimation (second step).}
  \item{mhat}{numeric vector containing the fitted frontier values,
    i.e. the fitted values from the nonparametric regression (first step)
    plus the bias correction \eqn{\mu}
    obtained in the maximum likelihood estimation (second step).}
  \item{mprime}{numeric matrix containing the first derivatives
    (gradients) of the dependent variable
    with respect to the explanatory variables
    obtained in the nonparametric regression (first step). }
  \item{e}{numeric vector containing the residuals
    from the nonparametric regression (first step).}
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
  \item{bw}{numeric vector of the bandwidths of the explanatory
    variables used in the nonparametric regression (first step).}
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

  # semiparametric local-linear Cobb-Douglas production frontier
  FLW_Result_ll <- sfaFLW( log( output ) ~ log( capital ) + log( labour ),
    data = front41Data, npArg = list( regtype = "ll" ) )
  FLW_Result_ll
}

\keyword{models}