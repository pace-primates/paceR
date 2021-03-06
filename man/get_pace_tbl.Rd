% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load-pace-tables.R
\name{get_pace_tbl}
\alias{get_pace_tbl}
\title{Get any table from PACE without the annoying warning messages.
Note that the foreign key IDs are not matched to their readable equivalents.}
\usage{
get_pace_tbl(pace_db, tbl_name, collect = TRUE)
}
\arguments{
\item{pace_db}{The src_mysql connection to the PACE Database.}

\item{tbl_name}{Name of the table, view, or query you want to get.}

\item{collect}{Option to collect the data or not. Default is TRUE.}
}
\description{
Get any table from PACE without the annoying warning messages.
Note that the foreign key IDs are not matched to their readable equivalents.
}
\examples{
get_pace_tbl(pace_db, "tblGroup")
}
