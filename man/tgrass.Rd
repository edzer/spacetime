\name{read.tgrass}
\alias{read.tgrass}
\alias{write.tgrass}
\title{ read or write tgrass (time-enabled grass) files }
\description{ read or write tgrass (time-enabled grass) files }
\usage{ 
read.tgrass(fname, localName = TRUE, useTempDir = TRUE, isGeoTiff = TRUE)
write.tgrass(obj, fname, ...)
}
\arguments{
\item{fname}{ file name to read from, or write to }
\item{localName}{ logical; if TRUE, \code{fname} is a local file, else
it is a the full path name to the file }
\item{useTempDir}{ logical: use a temporary directory for extraction?}
\item{isGeoTiff}{ logical: are the files in the tar.gz file GeoTIFFs?}
\item{obj}{ object to export, of class \code{STFDF} or \code{RasterStack}}
\item{...}{ arguments passed on to \link[raster]{writeRaster}}
}
\value{
\code{read.tgrass} returns an object of class \code{RasterStack},
\code{writegrass} returns nothing}
\details{ The tgrass format is a gzip'ed tar file (.tar.gz) that
has geotiff files (with suffix .tif), and three files (list.txt, proj.txt 
and init.txt) describing the file names and time slices,
coordinate reference system, and dimensions }
\author{Edzer Pebesma; time-enabled grass by Soeren Gebbert }
\references{ http://dx.doi.org/10.1016/j.envsoft.2013.11.001 }
\examples{
\dontrun{
library(spacetime)
r = read.tgrass("precipitation_1950_2011_yearly.tar.gz", useTempDir = FALSE)
write.tgrass(r, "myfile.tar.gz")
}
}
\keyword{manip}
