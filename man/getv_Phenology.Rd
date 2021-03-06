% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_paceR_views.R
\name{getv_Phenology}
\alias{getv_Phenology}
\title{Get table with Phenology data}
\usage{
getv_Phenology(paceR_db, full = TRUE, project = "")
}
\arguments{
\item{paceR_db}{The src_mysql connection to the paceR Database.}

\item{full}{Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.}

\item{Project}{Option to return only a subset of phenology data. Valid values include:
\itemize{
\item "GH" for Ghana (BFMS)
\item "MG" for Madagascar
\item "MR" for Monkey River
\item "RC" for Runaway Creek
\item "SR" for Santa Rosa
}}
}
\description{
Get table with Phenology data
}
\examples{
getv_Phenology(paceR_db, project = "SR")
}
