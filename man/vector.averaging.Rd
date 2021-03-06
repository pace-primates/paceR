% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pheno-functions.R
\name{vector.averaging}
\alias{vector.averaging}
\title{Calculate mean resultant vector from directional data, weighted by magnitudes
Based on function in SDMTools, but fixes problem with incorrect quadrants}
\usage{
vector.averaging(direction, distance, deg = TRUE)
}
\arguments{
\item{direction}{a vector of directions given in degrees (0 - 360) if
\code{deg}==TRUE or in radians if \code{deg}==FALSE}

\item{distance}{a vector of distances associated with each direction}

\item{deg}{a boolean object defining if \code{direction} is in degrees
(TRUE) or radians (FALSE)}
}
\description{
Calculate mean resultant vector from directional data, weighted by magnitudes
Based on function in SDMTools, but fixes problem with incorrect quadrants
}
\examples{
vector.averaging(c(10,20,70,78,108), distance=10)
vector.averaging(c(159,220,258,273,310),distance=runif(5))
}
