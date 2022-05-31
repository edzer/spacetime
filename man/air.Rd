\name{air}
\alias{air}
\alias{DE_NUTS1}
\alias{DE}
\alias{rural}
\alias{dates}
\alias{stations}
\title{Air quality data, rural background PM10 in Germany, daily averages 1998-2009}
\description{
Air quality data obtained from the airBase European air quality
data base.  Daily averages for rural background stations in Germany,
1998-2009. In addition, NUTS1 regions (states, or Bundeslaender) for Germany to
illustrate spatial aggregation over irregular regions.
}
\usage{
data(air)
}
\author{ air quality data compiled for R by Benedict Graeler; NUTS1
level data obtained from https://www.gadm.org/ . }
\references{ https://www.eionet.europa.eu/etcs/etc-acm/databases/airbase }
\note{ see vignette on overlay and spatio-temporal aggregation in this
package; the vignette on using google charts shows where the
ISO_3166_2_DE table comes from. }
\keyword{datasets}
\examples{
data(air)
rural = STFDF(stations, dates, data.frame(PM10 = as.vector(air)))
# how DE was created from DE_NUTS1:
if (require(rgeos))
	DE = gUnionCascaded(DE_NUTS1)
}
