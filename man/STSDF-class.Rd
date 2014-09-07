\name{STSDF-class}
\docType{class}
\alias{STSDF-class}
\alias{STS-class}
\alias{STSDF}
\alias{STS}
\alias{as.data.frame.STSDF}
\alias{as.data.frame.STS}
\alias{[,STS-method} 
\alias{[,STSDF-method} 
\alias{coerce,STSDF,STFDF-method}
\alias{coerce,STSDF,STIDF-method}
\alias{geometry,STSDF-method}
\alias{plot,STS,missing-method}
\alias{plot,STSDF,missing-method}
\alias{rbind.STSDF}

\title{Class "STSDF"}
\description{ A class for spatio-temporal data with partial
space-time grids; for n spatial locations and m times, an index
table is kept for which nodes observations are available }
\section{Objects from the Class}{
Objects of this class carry sparse space/time grid data }
\section{Slots}{
  \describe{
    \item{\code{sp}:}{Object of class \code{"Spatial"}}
    \item{\code{time}:}{Object holding time information; see \link{ST-class}
	for permitted types }
    \item{\code{index}:}{matrix of dimension n x 2, where n matches the number of rows
	in slot data }
	\item{\code{data}:}{Object of class \code{data.frame}, which holds
	the measured values}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "STSDF")}: selects spatial entities,        temporal entities, and attributes } 
    \item{plot}{\code{signature(x = "STS", y = "missing")}: plots
	space-time layout }
    \item{plot}{\code{signature(x = "STSDF", y = "missing")}: plots
	space-time layout, indicating records partially NA }
	% for spatial objects; does nothing but setting up a plotting region choosing
	% a suitable aspect if not given(see below), colouring the plot background using either a bg= argument or par("bg"), and possibly drawing axes.  }
	% \item{summary}{\code{signature(object = "Spatial")}: summarize object}
  }
}

\usage{
  STS(sp, time, index, endTime = delta(time))
  STSDF(sp, time, data, index, endTime = delta(time))
  \S4method{[}{STSDF}(x, i, j, ..., drop = is(x, "STSDF"))
  \S4method{coerce}{STSDF,STFDF}(from, to, strict=TRUE)
  \S4method{coerce}{STSDF,STIDF}(from, to, strict=TRUE)
}

\arguments{
\item{sp}{object of class \link{Spatial}}
\item{time}{object holding time information; see \link{ST-class}}
\item{data}{data frame with rows corresponding to the observations
(spatial index moving faster than temporal)} 
\item{index}{two-column matrix: rows corresponding to the nodes for
which observations are available, first column giving spatial index,
second column giving temporal index} 
\item{endTime}{ vector of class \code{POSIXct} with end points of time
intervals for the observations }
\item{x}{an object of class STFDF}
\item{i}{selection of spatial entities}
\item{j}{selection of temporal entities (see syntax in package xts) }
\item{...}{selection of attribute(s)}
\item{drop}{if TRUE and a single spatial entity is selected, an object
of class \link{xts} is returned; if TRUE and a single temporal entity is
selected, and object of the appropriate \code{Spatial} class is returned;
if FALSE, no coercion to reduced classes takes place}
\item{from}{object of class STFDF}
\item{to}{target class}
\item{strict}{ignored}
}

%TG: nrow(index) depends on how
%many observations are available

\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }
\references{ http://www.jstatsoft.org/v51/i07/ }
\seealso{ \link{delta} }

\examples{
sp = cbind(x = c(0,0,1), y = c(0,1,1))
row.names(sp) = paste("point", 1:nrow(sp), sep="")
library(sp)
sp = SpatialPoints(sp)
library(xts)
time = xts(1:4, as.POSIXct("2010-08-05")+3600*(10:13))
m = c(10,20,30) # means for each of the 3 point locations
mydata = rnorm(length(sp)*length(time),mean=rep(m, 4))
IDs = paste("ID",1:length(mydata))
mydata = data.frame(values = signif(mydata,3), ID=IDs)
stfdf = STFDF(sp, time, mydata)
stfdf
stsdf = as(stfdf, "STSDF")
stsdf[1:2,]
stsdf[,1:2]
stsdf[,,2]
stsdf[,,"values"]
stsdf[1,]
stsdf[,2]
# examples for [[, [[<-, $ and $<- 
stsdf[[1]]
stsdf[["values"]]
stsdf[["newVal"]] <- rnorm(12)
stsdf$ID
stsdf$ID = paste("OldIDs", 1:12, sep="")
stsdf$NewID = paste("NewIDs", 12:1, sep="")
stsdf
x = stsdf[stsdf,]
x = stsdf[stsdf[1:2,],]
all.equal(x, stsdf[1:2,])
}

\keyword{classes}
