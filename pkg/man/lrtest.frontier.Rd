\name{lrtest.frontier}
\alias{lrtest.frontier}

\title{Likelihood Ratio test for Stochastic Frontier Models}

\description{
   Testing parameter restrictions in stochastic frontier models
   by a Likelihood Ratio test.
}

\usage{
   \method{lrtest}{frontier}( object, ... )
}

\arguments{
   \item{object}{a fitted model object of class \code{frontier}.}
   \item{\dots}{further fitted model objects of class \code{frontier}.}
}

\details{
   \code{lrtest.frontier} consecutively compares
   the fitted model object \code{object}
   with the models passed in \code{...}.
   The test statistic is
   \code{2 * ( logLik( mu ) - logLik( mr ) )},
   where \code{mu} is the unrestricted model
   and \code{mr} is the restricted model.
   Asymptotically, the test statistic has a \eqn{\chi^2}
   distribution with \eqn{j} degrees of freedom
   under the null hypothesis,
   where \eqn{j} is the number of restrictions.
}

\value{
   An object of class \code{anova},
   which contains the log-likelihood value,
   degrees of freedom, the difference in degrees of freedom,
   likelihood ratio Chi-squared statistic and corresponding p value.
   See documentation of \code{\link[lmtest]{lrtest}}
   in package "lmtest".
}

\author{Arne Henningsen \email{arne.henningsen@gmail.com}}

\seealso{\code{\link{sfa}}, \code{\link[lmtest]{lrtest}}}

\examples{
# rice producers in the Philippines (panel data)
data( riceProdPhil )
riceProdPhil <- plm.data( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )

# Error Components Frontier with truncated normal distribution
# and time effects (unrestricted model)
mu <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   truncNorm = TRUE, timeEffect = TRUE, data = riceProdPhil )

# Error Components Frontier with half-normal distribution
# without time effects (restricted model)
mr <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
   data = riceProdPhil )

## perform LR-test
lrtest( mu, mr )
}

\keyword{models}
