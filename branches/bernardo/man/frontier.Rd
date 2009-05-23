\name{frontier}
\alias{frontier}
\alias{print.frontier}
\title{Stochastic Frontier Analysis}

\description{ Maximum Likelihood Estimation of
   Stochastic Frontier Production and Cost Functions.
   Two specifications are available:
   the error components specification with time-varying efficiencies
   (Battese and Coelli 1992)
   and a model specification in which the firm effects are directly 
   influenced by a number of variables (Battese and Coelli 1995).
   This R package uses the Fortran source code of Frontier 4.1
   (Coelli 1996).
}

\usage{
frontier( yName, xNames = NULL, zNames = NULL, data,
   showParNames = FALSE,
   code = "Fortran",
   modelType = ifelse( is.null( zNames ), 1, 2 ),
   ineffDecrease = TRUE, logDepVar = TRUE, mu = FALSE, timeEffect = FALSE,
   evalLogLik = FALSE,
   printIter = 0, gridScale = NA, tol = 0.00001, gridTol = 0.001, bignum = 1.0E+16,
   step1 = 0.00001, gridDouble = TRUE, gridno = 0.1, maxit = 1000,
   startVal = NULL )
\method{print}{frontier}( x, \dots )
}

\arguments{
   \item{yName}{string: name of the endogenous variable.}
   \item{xNames}{a vector of strings containing the names of the X variables
      (exogenous variables of the production or cost function).}
   \item{zNames}{a vector of strings containing the names of the Z variables
      (variables explaining the efficiency level).}
   \item{data}{a (panel) data frame that contains the data;
      if \code{data} is a usual data.frame,
      it is assumed that these are cross-section data;
      if \code{data} is a panel data frame
      (created with \code{\link[plm]{plm.data}}),
      it is assumed that these are panel data.}
   \item{showParNames}{ logical. If \code{TRUE}, the names of parameters beta 
      and delta include the names from xNames and zNames. If \code{FALSE} the 
      names of these parameters are numbered.}
   \item{code}{character string.
      If it is \code{"Fortran"}, the Fortran code of Frontier 4.1
      (Coelli 1996) is used;
      if this argument is \code{"R"}, the R code that has been written
      by Bernardo Mello is used.}
   \item{modelType}{model type: either 1 for an 'Error Components Frontier'
      or 2 for an 'Efficiency Effects Frontier'.}
   \item{ineffDecrease}{logical. If \code{TRUE},
      inefficiency decreases the endogenous variable
      (e.g. for estimating a production function);
      if \code{FALSE},
      inefficiency increases the endogenous variable
      (e.g. for estimating a cost function).}
   \item{logDepVar}{logical. Is the dependent variable logged.}
   \item{mu}{logical. Should a 'mu' (in case of an 'Error Components Frontier',
      i.e. modelType = 1)
      or a delta0 (in case of an 'Efficiency Effects Frontier',
      i.e. modelType = 2)
      be included in the estimation.}
   \item{timeEffect}{logical. If \code{FALSE} (default),
      the efficiency estimates of an \sQuote{Error Components Frontier}
      (\code{modelType = 1})
      are time invariant;
      if \code{TRUE}, time is allowed to have an effect on efficiency
      (this argument is ignored in case of an
      \sQuote{Efficiency Effects Frontier}, i.e. \code{modelType = 2}).}
   \item{evalLogLik}{logical. If \code{TRUE}, only the log likelihood value
      is evaluated and returned (given the starting values and the data).}
   \item{printIter}{numeric. Print info every \code{printIter} iterations;
      if this argument is 0, do not print.}
   \item{gridScale}{logical or \code{NA}. Scaling in unidimensional grid search procedure:
      if \code{TRUE}, the step length is scaled to the length of the last step;
      if \code{FALSE}, the step length is not scaled;
      if \code{NA}, the step length is scaled (to the length of last step)
         only if the last step was smaller.}
   \item{tol}{numeric. Convergence tolerance (proportional).}
   \item{gridTol}{numeric. Tolerance used in uni-dimensional grid search procedure.}
   \item{bignum}{numeric. Used to set bounds on densities and distributions.}
   \item{step1}{numeric. Size of 1st step in search procedure.}
   \item{gridDouble}{logical. If \code{TRUE},
      a second phase grid search for \eqn{\gamma}{gamma} is conducted
      around the \dQuote{best} value obtained in the first phase.}
   \item{gridno}{numeric. Steps taken in single accuracy grid search on gamma.}
   \item{maxit}{numeric. Maximum number of iterations permitted.}
   \item{startVal}{numeric vector. Optional starting values for the ML 
      estimation.}
   \item{x}{an object of class \code{frontier} 
      (returned by the function \code{frontier}).}
   \item{\dots}{currently unused.}
}

\details{The \code{frontier} function uses the Fortran source code of
   Tim Coelli's software FRONTIER 4.1
   (\url{http://www.uq.edu.au/economics/cepa/frontier.htm})
   and hence, provides the same features as FRONTIER 4.1.
   A comprehensive documentation of FRONTIER 4.1 is available
   in the file \code{Front41.pdf}
   that is included in the archive \code{FRONT41-xp1.zip},
   which is available at
   \url{http://www.uq.edu.au/economics/cepa/frontier.htm}.
   It is recommended to read this documentation, 
   because the \code{frontier} function is based on the FRONTIER 4.1 software.
}

\value{
   \code{frontier} returns a list of class \code{frontier}
   containing following elements:
   \item{code}{source code used (see above)}
   \item{modelType}{model type (see above).}
   \item{ineffDecrease}{indicates whether inefficiency decreases (or increases) 
      the endogenous variable (see above).}
   \item{logDepVar}{logical. Is the dependent variable logged?}
   \item{nn}{number of cross-sections.}
   \item{nt}{number of time periods.}
   \item{nob}{number of observations in total.}
   \item{nb}{number of regressor variables (Xs).}
   \item{mu}{logical. Indicates if a 'mu'/'delta0' is included
      in the estimation (see above).}
   \item{nz}{numeric. The number of efficiency effects regressors (Zs)
      (only for the \sQuote{Efficiency Effects Frontier}).}
   \item{timeEffect}{logical. Argument \code{timeEffect}
      (only or the \sQuote{Error Components Frontier}).}
   \item{printIter}{numeric. Printing of information
      on the iterations (see above).}
   \item{gridScale}{numeric. Scaling of unidimensional grid search procedure
      (see above).}
   \item{tol}{numeric. Convergence tolerance.}
   \item{gridTol}{numeric. Tolerance used in uni-dimensional grid search procedure.}
   \item{bignum}{numeric. Used to set bounds on densities and distributions.}
   \item{step1}{numeric. Size of 1st step in search procedure.}
   \item{gridDouble}{logical. Value of argument \code{gridDouble}
      (see above).}
   \item{gridno}{numeric. Steps taken in single accuracy grid search
      on gamma.}
   \item{maxit}{numeric. Maximum number of iterations permitted.}
   \item{startVal}{numeric vector. Starting values for the ML
      estimation (if specified by user).}
   \item{call}{the matched call.}
   \item{dataTable}{matrix. Data matrix sent to Frontier 4.1.}
   \item{olsParam}{numeric vector. OLS estimates.}
   \item{olsStdEr}{numeric vector. Standard errors of OLS estimates.}
   \item{olsLogl}{numeric. Log likelihood value of OLS estimation.}
   \item{gridParam}{numeric vector. Parameters obtained from the grid search
      (if no starting values were specified).}
   \item{mleParam}{numeric vector. Parameters obtained from ML estimation.}
   \item{mleCov}{matrix. Covariance matrix of the parameters obtained
      from the OLS estimation.}
   \item{mleLogl}{numeric. Log likelihood value of the ML estimation.}
   \item{lrTestVal}{LR test of the one-sided error
      (this statistic has a mixed chi-square distribution
      with \code{lrTestDf} degrees of freedom.}
   \item{lrTestDf}{numeric. Degrees of freedom of the LR test.}
   \item{nIter}{numeric. Number of iterations of the ML estimation.}
   \item{effic}{matrix. Efficiency estimates:
      each row corresponds to a cross-section;
      each column corresponds to a time period.}
}

\references{
   Battese, G.E. and T. Coelli (1992), Frontier production functions,
      technical efficiency and panel data: with application to paddy
      farmers in India. \emph{Journal of Productivity Analysis}, 3, 153-169.

   Battese, G.E. and T. Coelli (1995), A model for technical inefficiency effects
      in a stochastic frontier production function for panel data.
      \emph{Empirical Economics}, 20, 325-332.

   Coelli, T. (1996) A Guide to FRONTIER Version 4.1: A Computer
      Program for Stochastic Frontier Production and Cost Function
      Estimation, CEPA Working Paper 96/08,
      \url{http://www.uq.edu.au/economics/cepa/frontier.htm},
      University of New England.
}

\seealso{\code{\link{frontierQuad}} for quadratic/translog frontiers.}

\author{Tim Coelli and Arne Henningsen}

\examples{
   # example included in FRONTIER 4.1 (cross-section data)
   data( front41Data )
   front41Data$logOutput  <- log( front41Data$output )
   front41Data$logCapital <- log( front41Data$capital )
   front41Data$logLabour  <- log( front41Data$labour )

   # Cobb-Douglas production frontier
   cobbDouglas <- frontier( yName = "logOutput",
      xNames = c( "logCapital", "logLabour" ), data = front41Data )
   cobbDouglas

   # rice producers in the Philippines (panel data)
   data( riceProdPhil )
   riceProdPhil <- plm.data( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )
   riceProdPhil$lPROD  <- log( riceProdPhil$PROD )
   riceProdPhil$lAREA  <- log( riceProdPhil$AREA )
   riceProdPhil$lLABOR <- log( riceProdPhil$LABOR )
   riceProdPhil$lNPK   <- log( riceProdPhil$NPK )

   # Error Components Frontier (Battese & Coelli 1992)
   rice <- frontier( yName = "lPROD",
      xNames = c( "lAREA", "lLABOR", "lNPK" ), data = riceProdPhil )
   rice

   # Technical Efficiency Effects Frontier (Battese & Coelli 1995)
   rice2 <- frontier( yName = "lPROD",
      xNames = c( "lAREA", "lLABOR", "lNPK" ),
      zNames = c( "EDYRS", "BANRAT" ), data = riceProdPhil )
   rice2
}

\keyword{models}
