\name{prodAgrBrazil}
\docType{data}
\alias{prodAgrBrazil}

\title{Agricultural Production in the Brazilian States}

\description{
   The \code{prodAgrBrazil} data contain data on the agricultural
   production in the 27 Brazilian states in the years 1995 and 2006.
}

\usage{data(prodAgrBrazil)}

\format{
   This data frame contains the following columns:
   \describe{
      \item{state}{acronym of the name of the state.}
      \item{year}{year (1995 or 2006).}
      \item{gip_pc}{gip_pc.}
      \item{idh}{idh.}
      \item{idh_l}{idh_l.}
      \item{idh_e}{idh_e.}
      \item{idh_r}{idh_r.}
      \item{area}{land area.}
      \item{labor}{labor use in agriculture.}
      \item{other_exp}{other expenditure.}
      \item{capital}{capital.}
      \item{production}{agricultural production.}
   }
}

\source{
   Unknown
}

\examples{
   data( prodAgrBrazil )
   # add information on panel structure
   prodAgrBrazil <- plm.data( prodAgrBrazil, c( "state", "year" ) )
   # calculate logarithms
   prodAgrBrazil$lProd    <- log( prodAgrBrazil$production )
   prodAgrBrazil$lLabor   <- log( prodAgrBrazil$labor )
   prodAgrBrazil$lArea    <- log( prodAgrBrazil$area )
   prodAgrBrazil$lOther   <- log( prodAgrBrazil$other_exp )
   prodAgrBrazil$lCapital <- log( prodAgrBrazil$capital )

   # Error Components Frontier
   ecfResult <- frontier( data = prodAgrBrazil, yName = "lProd",
      xNames = c( "lArea", "lLabor", "lOther", "lCapital" ),
      timeEffect = TRUE )
   summary( ecfResult )

   # Efficiency Effects Frontier
   eefResult <- frontier( data = prodAgrBrazil, yName = "lProd",
      xNames = c( "lArea", "lLabor", "lOther", "lCapital" ),
      zNames = c( "idh", "gip_pc" ) )
   summary( eefResult )
}

\keyword{datasets}
