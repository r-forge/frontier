\name{sfa}
\alias{sfa}
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
sfa( formula, data = sys.frame( sys.parent() ),
   ineffDecrease = TRUE, truncNorm = FALSE,
   timeEffect = FALSE, startVal = NULL,
   tol = 0.00001, maxit = 1000, minit = min( 5, maxit ), 
   muBound = 2, bignum = 1.0E+16,
   searchStep = 0.00001, searchTol = 0.001, searchScale = NA,
   gridSize = 0.1, gridDouble = TRUE,
   restartMax = 10, restartFactor = 0.999, printIter = 0 )

frontier( yName, xNames = NULL, zNames = NULL, data,
   zIntercept = FALSE, \dots )

\method{print}{frontier}( x, digits = NULL, \dots )
}

\arguments{
   \item{formula}{a symbolic description of the model to be estimated;
      it can be either a (usual) one-part or a two-part formula
      (see section \sQuote{Details}).}
   \item{data}{a (panel) data frame that contains the data;
      if \code{data} is a usual data.frame,
      it is assumed that these are cross-section data;
      if \code{data} is a panel data frame
      (created with \code{\link[plm]{pdata.frame}}),
      it is assumed that these are panel data.}
   \item{ineffDecrease}{logical. If \code{TRUE},
      inefficiency decreases the endogenous variable
      (e.g. for estimating a production function);
      if \code{FALSE},
      inefficiency increases the endogenous variable
      (e.g. for estimating a cost function).}
   \item{truncNorm}{logical. If \code{TRUE},
      the inefficiencies are assumed to have a truncated normal distribution
      (i.e. parameter \eqn{\mu}{mu} is added);
      if \code{FALSE},
      they are assumed to have a half-normal distribution
      (only relevant for the \sQuote{Error Components Frontier}).}
   \item{timeEffect}{logical. If \code{FALSE} (default),
      the efficiency estimates of an \sQuote{Error Components Frontier}
      are time invariant;
      if \code{TRUE}, time is allowed to have an effect on efficiency
      (this argument is ignored in case of an
      \sQuote{Efficiency Effects Frontier}).}
   \item{startVal}{numeric vector. Optional starting values for the ML
      estimation.}
   \item{tol}{numeric. Convergence tolerance (proportional).}
   \item{maxit}{numeric. Maximum number of iterations permitted.}
   \item{minit}{numeric. Minimum number of iterations 
      (ignored if the search procedure cannot find parameter values 
      that give a higher log-likelihood value 
      than the current parameter values).}
   \item{muBound}{numeric. Bounds on the parameter \eqn{\mu}{mu}
      (see \sQuote{details} section).}
   \item{bignum}{numeric. Used to set bounds on densities and distributions.}
   \item{searchStep}{numeric. Size of the first step in the Coggin
      uni-dimensional search procedure done each iteration
      to determine the optimal step length for the next iteration
      (see Himmelblau 1972).}
   \item{searchTol}{numeric. Tolerance used in the Coggin
      uni-dimensional search procedure done each iteration
      to determine the optimal step length for the next iteration
      (see Himmelblau 1972).}
   \item{searchScale}{logical or \code{NA}. Scaling in the Coggin
      uni-dimensional search procedure done each iteration
      to determine the optimal step length for the next iteration
      (see Himmelblau 1972):
      if \code{TRUE}, the step length is scaled to the length of the last step;
      if \code{FALSE}, the step length is not scaled;
      if \code{NA}, the step length is scaled (to the length of last step)
         only if the last step was smaller.}
   \item{gridSize}{numeric. The size of the increment
      in the first phase grid search on \eqn{\gamma}{gamma}.}
   \item{gridDouble}{logical. If \code{TRUE},
      a second phase grid search on \eqn{\gamma}{gamma} is conducted
      around the \dQuote{best} value obtained in the first phase
      with an increment of \code{gridSize/10}.}
   \item{restartMax}{integer: maximum number of restarts of the search procedure
      when it cannot find a parameter vector
      that results in a log-likelihood value
      larger than the log-likelihood value of the initial parameters.}
   \item{restartFactor}{numeric scalar: if the search procedure
      cannot find a parameter vector that results in a log-likelihood value
      larger than the log-likelihood value of the initial parameters,
      the initial values
      (provided by argument \code{startVal} or obtained by the grid serach)
      are multiplied by this number before the search procedure
      is restarted.}
   \item{printIter}{numeric. Print info every \code{printIter} iterations;
      if this argument is 0, do not print.}
   \item{yName}{string: name of the endogenous variable.}
   \item{xNames}{a vector of strings containing the names of the X variables
      (exogenous variables of the production or cost function).}
   \item{zNames}{a vector of strings containing the names of the Z variables
      (variables explaining the efficiency level).}
   \item{zIntercept}{logical. If \code{TRUE},
      an intercept (with parameter \eqn{\delta_0}{delta_0})
      is added to the Z variables
      (only relevant for the \sQuote{Efficiency Effects Frontier}).}
   \item{x}{an object of class \code{frontier}
      (returned by the function \code{frontier}).}
   \item{digits}{a non-null value for \sQuote{digits} specifies
      the minimum number of significant digits to be printed in values.
      The default, \code{NULL}, uses
      \code{max(3,getOption("digits")-3)}.
      Non-integer values will be rounded down, and only values greater
      than or equal to 1 and no greater than 22 are accepted.}
   \item{\dots}{additional arguments of \code{frontier}
      are passed to \code{sfa};
      additional arguments of the \code{print} method
      are currently ignored.}
}

\details{
   Function \code{frontier} is a wrapper function
   that calls \code{sfa} for the estimation.
   The two functions differ only in the user interface;
   function \code{frontier} has the \dQuote{old} user interface
   and is kept to maintain compatibility with older versions
   of the \code{frontier} package.

   One can use functions \code{sfa} and \code{frontier}
   to calculate the log likelihood value for a given model,
   a given data set, and given parameters
   by using the argument \code{startVal} to specify the parameters
   and using the other arguments to specify the model and the data.
   The log likelihood value can then be retrieved by
   the \code{\link[=logLik.frontier]{logLik}} method
   with argument \code{which} set to \code{"start"}.
   Setting argument \code{maxit} to \code{0} avoids the
   (eventually time-consuming) ML estimation and allows
   to retrieve the log likelihood value
   with the \code{\link[=logLik.frontier]{logLik}} method
   without further arguments.

   The \code{frontier} function uses the Fortran source code of
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

   If argument \code{formula} of \code{sfa} is a (usual) one-part formula
   (or argument \code{zNames} of \code{frontier} is \code{NULL}),
   an \sQuote{Error Components Frontier} (ECF, see Battese and Coelli 1992)
   is estimated.
   If argument \code{formula} is a two-part formula
   (or \code{zNames} is not \code{NULL}),
   an \sQuote{Efficiency Effects Frontier} (EEF, see Battese and Coelli 1995)
   is estimated.
   In this case, the first part of the formula
   (i.e. the part before the \dQuote{|} symbol)
   is used to explain the endogenous variable directly (X variables),
   while the second part of the formula
   (i.e. the part after the \dQuote{|} symbol)
   is used to explain the efficiency levels (Z variables).
   Generally, there should be no reason for estimating an EEF
   without Z variables,
   but this can done by setting the second part of argument \code{formula}
   to \code{1} (with Z intercept) or \code{- 1} (without Z intercept)
   (or by setting argument \code{zNames}) to \code{NA}).

   In case of an Error Components Frontier (ECF)
   with the inefficiency terms \eqn{u} following a
   truncated normal distribution with mean \eqn{\mu}{mu},
   argument \code{muBound} can be used to restrict \eqn{\mu}{mu}
   to be in the interval \eqn{\pm}{+/-}\code{muBound} * \eqn{\sigma_u}{sigma_u},
   where \eqn{\sigma_u}{sigma_u} is the standard deviation of \eqn{u}.
   If \code{muBound} is infinity, zero, or negative,
   no bounds on \eqn{\mu}{mu} are imposed.
}

\value{
   \code{sfa} and \code{frontier} return a list of class \code{frontier}
   containing following elements:
   \item{modelType}{integer.
      A \sQuote{1} denotes an \sQuote{Error Components Frontier} (ECF);
      a \sQuote{2} denotes an \sQuote{Efficiency Effects Frontier} (EFF).}
   \item{ineffDecrease}{logical. Argument \code{ineffDecrease} (see above).}
   \item{nn}{number of cross-sections.}
   \item{nt}{number of time periods.}
   \item{nob}{number of observations in total.}
   \item{nb}{number of regressor variables (Xs).}
   \item{truncNorm}{logical. Argument \code{truncNorm}.}
   \item{zIntercept}{logical. Argument \code{zIntercept}.}
   \item{timeEffect}{logical. Argument \code{timeEffect}.}
   \item{printIter}{numeric. Argument \code{printIter} (see above).}
   \item{searchScale}{numeric. Argument \code{searchScale} (see above).}
   \item{tol}{numeric. Argument \code{tol} (see above).}
   \item{searchTol}{numeric. Argument \code{searchTol} (see above).}
   \item{bignum}{numeric. Argument \code{bignum} (see above).}
   \item{searchStep}{numeric. Argument \code{searchStep} (see above).}
   \item{gridDouble}{logical. Argument \code{gridDouble} (see above).}
   \item{gridSize}{numeric. Argument \code{gridSize} (see above).}
   \item{maxit}{numeric.  Argument \code{maxit} (see above).}
   \item{muBound}{numeric.  Argument \code{muBound} (see above).}
   \item{restartMax}{numeric.  Argument \code{restartMax} (see above).}
   \item{restartFactor}{numeric.  Argument \code{restartFactor} (see above).}
   \item{nRestart}{numeric.  Number of restarts of the search procedure
      when it cannot find a parameter vector
      that results in a log-likelihood value
      larger than the log-likelihood value of the initial parameters.}
   \item{startVal}{numeric vector. Argument \code{startVal}
      (only if specified by user).}
   \item{call}{the matched call.}
   \item{dataTable}{matrix. Data matrix sent to Frontier 4.1.}
   \item{olsParam}{numeric vector. OLS estimates.}
   \item{olsStdEr}{numeric vector. Standard errors of OLS estimates.}
   \item{olsLogl}{numeric. Log likelihood value of OLS estimation.}
   \item{olsResid}{numeric vector. Residuals of the OLS estimation.}
   \item{olsSkewness}{numeric. Skewness of the residuals of the OLS estimation.}
   \item{olsSkewnessOkay}{logical. Indicating if the residuals
      of the OLS estimation have the expected skewness.}
   \item{gridParam}{numeric vector. Parameters obtained from the grid search
      (if no starting values were specified).}
   \item{gridLogl}{numeric. Log likelihood value of the parameters
      obtained from the grid search
      (only if no starting values were specified).}
   \item{startLogl}{numeric. Log likelihood value of the starting values
      for the parameters
      (only if starting values were specified).}
   \item{mleParam}{numeric vector. Parameters obtained from ML estimation.}
   \item{mleCov}{matrix. Covariance matrix of the parameters obtained
      from the OLS estimation.}
   \item{mleLogl}{numeric. Log likelihood value of the ML estimation.}
   \item{nIter}{numeric. Number of iterations of the ML estimation.}
   \item{code}{integer indication the reason for determination:
      \code{1} = log likelihood values and parameters
         of two successive iterations are within the tolerance limits;
      \code{5} = cannot find a parameter vector that results
         in a log-likelihood value larger than
         the log-likelihood value obtained in the previous step;
      \code{6} = search failed on gradient step;
      \code{10} = maximum number of iterations reached.}
   \item{nFuncEval}{Number of evaluations of the log likelihood function
      during the grid search and the iterative ML estimation.}
   \item{fitted}{matrix. Fitted \dQuote{frontier} values
      of the dependent variable:
      each row corresponds to a cross-section;
      each column corresponds to a time period.}
   \item{resid}{matrix. Residuals:
      each row corresponds to a cross-section;
      each column corresponds to a time period.}
   \item{validObs}{vector of logical values indicating which observations
      of the provided data were used for the estimation,
      i.e. do not have values that are not available (\code{NA}, \code{NaN})
      or infinite (\code{Inf}).}
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
      \url{http://www.uq.edu.au/economics/cepa/frontier.php},
      University of New England.

   Himmelblau, D.M. (1972), \emph{Applied Non-Linear Programming},
      McGraw-Hill, New York.
}

\seealso{\code{\link{frontierQuad}} for quadratic/translog frontiers,
   \code{\link{summary.frontier}} for creating and printing summary results,
   \code{\link{efficiencies.frontier}} for calculating efficiency estimates,
   \code{\link{lrtest.frontier}} for comparing models by LR tests,
   \code{\link{fitted.frontier}} for obtaining the fitted \dQuote{frontier} values,
   ang \code{\link{residuals.frontier}} for obtaining the residuals.}

\author{Tim Coelli and Arne Henningsen}

\examples{
   # example included in FRONTIER 4.1 (cross-section data)
   data( front41Data )

   # Cobb-Douglas production frontier
   cobbDouglas <- sfa( log( output ) ~ log( capital ) + log( labour ),
      data = front41Data )
   summary( cobbDouglas )

   # load data about rice producers in the Philippines (panel data)
   data( riceProdPhil )

   # Error Components Frontier (Battese & Coelli 1992)
   # with observation-specific efficiencies (ignoring the panel structure)
   rice <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
      data = riceProdPhil )
   summary( rice )

   # Error Components Frontier (Battese & Coelli 1992)
   # with "true" fixed individual effects and observation-specific efficiencies
   riceTrue <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) + 
      factor( FMERCODE ),  data = riceProdPhil )
   summary( riceTrue )

   # add data set with information about its panel structure
   library( "plm" )
   ricePanel <- pdata.frame( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )

   # Error Components Frontier (Battese & Coelli 1992)
   # with time-invariant efficiencies
   riceTimeInv <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
      data = ricePanel )
   summary( riceTimeInv )

   # Error Components Frontier (Battese & Coelli 1992)
   # with time-variant efficiencies
   riceTimeVar <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ),
      data = ricePanel, timeEffect = TRUE )
   summary( riceTimeVar )

   # Technical Efficiency Effects Frontier (Battese & Coelli 1995)
   # (efficiency effects model with intercept)
   riceZ <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
      EDYRS + BANRAT, data = riceProdPhil )
   summary( riceZ )

   # Technical Efficiency Effects Frontier (Battese & Coelli 1995)
   # (efficiency effects model without intercept)
   riceZ2 <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) |
      EDYRS + BANRAT - 1, data = riceProdPhil )
   summary( riceZ2 )

   # Cost Frontier (with land as quasi-fixed input)
   riceProdPhil$cost <- riceProdPhil$LABOR * riceProdPhil$LABORP +
      riceProdPhil$NPK * riceProdPhil$NPKP
   riceCost <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP )
      + log( NPKP ), data = riceProdPhil, ineffDecrease = FALSE )
   summary( riceCost )
}

\keyword{models}
