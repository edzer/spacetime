\name{mnf}
\alias{mnf}
\alias{mnf.RasterBrick}
\alias{mnf.RasterStack}
\alias{mnf.SpatialGridDataFrame}
\alias{mnf.SpatialPixelsDataFrame}
\alias{mnf.matrix}
\alias{mnf.mts}
\alias{mnf.zoo}
\title{Generic mnf method}
\usage{
mnf(x, ...)
\method{mnf}{matrix}(x, ..., use = "complete.obs")
\method{mnf}{mts}(x, ..., use = "complete.obs")
\method{mnf}{zoo}(x, ..., use = "complete.obs")
\method{mnf}{SpatialPixelsDataFrame}(x, ..., use = "complete.obs")
\method{mnf}{SpatialGridDataFrame}(x, ..., use = "complete.obs")
\method{mnf}{RasterStack}(x, ..., use = "complete.obs")
\method{mnf}{RasterBrick}(x, ..., use = "complete.obs")
}
\arguments{
\item{x}{object for which an mnf method is available}
\item{...}{ignored}
\item{use}{method to deal with missing values when computing covariances; see \link{cov}}
}
\description{
generic to compute mnf from data
}
