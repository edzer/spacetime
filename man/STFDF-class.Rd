\name{STFDF-class}
\docType{class}
\alias{STFDF-class}
\alias{STF-class}
\alias{STFDF}
\alias{STF}
\alias{as.data.frame.STF}
\alias{[,STF-method} 
\alias{[,STFDF-method} 
\alias{coerce,STFDF,xts-method}
\alias{coerce,STFDF,Spatial-method}
\alias{plot,STF,missing-method}
\alias{plot,STFDF,missing-method}
\alias{geometry,STFDF-method}
\alias{na.omit.STFDF}
\alias{rbind.STFDF}
\alias{as.zoo}
\alias{as.zoo.STFDF}

\title{Class "STFDF"}
\description{ A class for spatio-temporal data with full space-time grid; for
n spatial locations and m times, n x m observations are available }
\section{Objects from the Class}{
Objects of this class represent full space/time data with a full grid 
(or lattice) layout }
\section{Slots}{
  \describe{
    \item{\code{sp}:}{spatial object; see \link{ST-class}}
    \item{\code{time}:}{temporal object; see \link{ST-class}}
	\item{\code{data}:}{Object of class \code{data.frame}, which holds
	the measured values; space index cycling first, time order preserved}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "STFDF")}: selects spatial entities,        temporal entities, and attributes } 
   \item{coerce}{STFDF,xts}
   \item{coerce}{STFDF,Spatial}(from) coerces to (wide form) SpatialXxDataFrame,
   where SpatialXx is the spatial class of from@sp
    \item{plot}{\code{signature(x = "STF", y = "missing")}: plots space-time
	layout }
    \item{plot}{\code{signature(x = "STFDF", y = "missing")}: plots space-time
	layout, indicating full missing valued records }
	% for spatial objects; does nothing but setting up a plotting region choosing
	% a suitable aspect if not given(see below), colouring the plot background using either a bg= argument or par("bg"), and possibly drawing axes.  }
	% \item{summary}{\code{signature(object = "Spatial")}: summarize object}
  }
}

\usage{
  STF(sp, time, endTime = delta(time))
  STFDF(sp, time, data, endTime = delta(time))
  \S4method{[}{STFDF}(x, i, j, ..., drop = is(x, "STFDF"))
  \S4method{coerce}{STFDF,xts}(from, to, strict=TRUE)
  \S4method{coerce}{STFDF,Spatial}(from, to)
}

\arguments{
\item{sp}{object of class \link{Spatial}, having \code{n} elements}
\item{time}{object holding time information, of length \code{m};
see \link{ST} for details}
\item{endTime}{ vector of class \code{POSIXct}, holding end points
of time intervals; by default, time intervals equal the time step
width, see \link{delta} }
\item{data}{data frame with \code{n*m} rows corresponding to the
observations (spatial index moving fastest)}
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

\value{the \code{as.data.frame} coercion returns the full long table,
with purely spatial attributes and purely time attributes replicated
appropriately. }

\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }
\references{ http://www.jstatsoft.org/v51/i07/ }

\examples{
sp = cbind(x = c(0,0,1), y = c(0,1,1))
row.names(sp) = paste("point", 1:nrow(sp), sep="")
library(sp)
sp = SpatialPoints(sp)
time = as.POSIXct("2010-08-05")+3600*(10:13)
m = c(10,20,30) # means for each of the 3 point locations
mydata = rnorm(length(sp)*length(time),mean=rep(m, 4))
IDs = paste("ID",1:length(mydata))
mydata = data.frame(values = signif(mydata,3), ID=IDs)
stfdf = STFDF(sp, time, mydata)
stfdf
stfdf[1:2,]
stfdf[,1:2]
stfdf[,,2]
stfdf[,,"values"]
stfdf[1,]
stfdf[,2]
as(stfdf[,,1], "xts")
as(stfdf[,,2], "xts")
# examples for [[, [[<-, $ and $<- 
stfdf[[1]]
stfdf[["values"]]
stfdf[["newVal"]] <- rnorm(12)
stfdf$ID
stfdf$ID = paste("OldIDs", 1:12, sep="")
stfdf$NewID = paste("NewIDs", 12:1, sep="")
stfdf
x = stfdf[stfdf[1:2,],] 
all.equal(x, stfdf[1:2,]) 
all.equal(stfdf, stfdf[stfdf,]) # converts character to factor...
}

\keyword{classes}
