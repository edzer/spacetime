\name{STIDF-class}
\docType{class}
\alias{STIDF-class}
\alias{STI-class}
\alias{STIDF}
\alias{STI}
\alias{as.data.frame.STIDF}
\alias{as.data.frame.STI}
\alias{[,STIDF-method} 
\alias{[,STI-method} 
\alias{geometry,STIDF-method}
\alias{geometry,STI-method}
\alias{coerce,STIDF,STSDF-method}
\alias{plot,STI,missing-method}
\alias{rbind.STIDF}

\title{Class "STIDF"}
\description{ A class for unstructured spatio-temporal data; for
n spatial locations and times, n observations are available }
\section{Objects from the Class}{
Objects of this class carry full space/time grid data }
\section{Slots}{
  \describe{
    \item{\code{sp}:}{Object of class \code{"Spatial"}}
    \item{\code{time}:}{Object holding time information, see \link{ST-class}}
	\item{\code{data}:}{Object of class \code{data.frame}, which holds
	the measured values }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "STIDF")}: selects spatial-temporal entities, and attributes } 

    % \item{plot}{\code{signature(x = "Spatial", y = "missing")}: plot method
	% for spatial objects; does nothing but setting up a plotting region choosing
	% a suitable aspect if not given(see below), colouring the plot background using either a bg= argument or par("bg"), and possibly drawing axes.  }
	% \item{summary}{\code{signature(object = "Spatial")}: summarize object}
  }
}

\usage{
  STI(sp, time, endTime)
  STIDF(sp, time, data, endTime)
  \S4method{[}{STIDF}(x, i, j, ..., drop = FALSE)
  \S4method{coerce}{STIDF,STSDF}(from, to, strict=TRUE)
}

\arguments{
\item{sp}{object of class \link{Spatial}}
\item{time}{object holding time information; when STIDF is called,
a non-ordered vector with times, e.g. \link{POSIXct} will also work,
and rearrange the \code{sp} and \code{data} slots
according to the ordering of time; for this to work no
ties should exist. }
\item{endTime}{ vector of class \code{POSIXct}, indicating the end
points of time intervals for the observations. By default, for \code{STI}
objects \code{time} is taken, indicating that time intervals have zero width 
(time instances) }
\item{data}{data frame with appropriate number of rows}
\item{x}{an object of class STFDF}
\item{i}{selection of record index (spatial/temporal entities),
or character string with temporal selection }
\item{j}{selection of attributes }
\item{...}{ignored}
\item{drop}{if TRUE and a single spatial entity is selected, an object
of class \link{xts} is returned (NOT yet implemented);
if TRUE and a single temporal entity is
selected, and object of the appropriate \code{Spatial} class is returned;
if FALSE, no coercion to reduced classes takes place}
\item{from}{object of class STFDF}
\item{to}{target class}
\item{strict}{ignored}
}

\note{ arguments \code{sp}, \code{time} and \code{data} need 
to have the same number of records,
and regardless of the class of time (xts or POSIXct) have to be
in correspoinding order: the triple \code{sp[i]}, \code{time[i]}
and \code{data[i,]} refer to the same observation }

\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }
\references{ http://www.jstatsoft.org/v51/i07/ }

\keyword{classes}
