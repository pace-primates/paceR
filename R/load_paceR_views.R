#' Get table with group-census data
#'
#' @param paceR_db The src_mysql connection to the paceR Database.
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' getv_CensusMonthly(paceR_db)
getv_CensusMonthly <- function(paceR_db, full = TRUE){

  census <- get_pace_tbl(paceR_db, "vCensusMonthly")

  census <- census %>%
    mutate_each(funs(as.Date), CensusDateOf, DateOfBirth)


  if (!full) {
    census <- census %>%
      select(-GroupName, -CensusMonthlyID, -CensusMonthlyComments,
             -CensusResearcherName, -CensusMonthlyGroupIndividID,
             -CensusMonthlyGroupIndividComments, -ToGroupName,
             -FromGroupName)
  }

  return(census)
}

#' Get table with Individual data
#'
#' @param paceR_db The src_mysql connection to the paceR Database.
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' getv_Individual(paceR_db)
getv_Individual <- function(paceR_db, full = TRUE){

  ind <- get_pace_tbl(paceR_db, "vIndividual")

  ind <- ind %>%
    mutate_each(funs(as.Date), starts_with("Date")) %>%
    rename(IndividualID = ID)

  if (!full) {
    ind <- ind %>%
      select(IndividualID, NameOf, Project, DateOfBirth, Sex)
  }

  return(ind)
}

#' Get table with Phenology data
#'
#' @param paceR_db The src_mysql connection to the paceR Database.
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' getv_Phenology(paceR_db)
getv_Phenology <- function(paceR_db, full = TRUE){

  p <- get_pace_tbl(paceR_db, "vPhenology")

  p <- p %>%
    mutate_each(funs(as.Date), contains("Date"))

  if (!full) {
    p <- p %>%
      select(Project, PhenologyDate, TreeLabel, FoodPart, Measurement,
             PhenologyScore, PhenologyPercent, PhenologyCount)
  }

  return(p)
}