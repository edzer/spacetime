\name{nbMult}
\alias{nbMult}
\alias{nbMult}
\title{ convert a spatial nb object to a matching STF object }
\description{ convert a spatial nb object to a matching STF object }
\usage{ 
nbMult(nb, st, addT = TRUE, addST = FALSE)
}
\arguments{
\item{nb}{ object of class nb (see package spdep), which is valid for
the spatial slot of object \code{st}: \code{length(nb)} should equal
\code{length(st@sp)}}
\item{st}{ object of class STF }
\item{addT}{ logical; should temporal neighbours be added? }
\item{addST}{ logical; should spatio-temporal neighbours be added? }
}
\value{ object of class \code{nb} }
\details{ if both \code{addT} and \code{addST} are false, only
spatial neighbours are added for each time replicate.

details are found in 

Giovana M. de Espindola, Edzer Pebesma, Gilberto
C\^{a}mara, 2011.  Spatio-temporal regression
models for deforestation in the Brazilian Amazon.
STDM 2011, The International Symposium on Spatial-Temporal Analysis
and Data Mining, University College London - 18th-20th July 2011.
}

\author{Edzer Pebesma}
\keyword{manip}
