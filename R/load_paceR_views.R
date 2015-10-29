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
#' @param Project Option to return only a subset of phenology data. Valid values include:
#' \itemize{
#' \item "GH" for Ghana (BFMS)
#' \item "MG" for Madagascar
#' \item "MR" for Monkey River
#' \item "RC" for Runaway Creek
#' \item "SR" for Santa Rosa
#' }
#'
#' @export
#' @examples
#' getv_Phenology(paceR_db, project = "SR")
getv_Phenology <- function(paceR_db, full = TRUE, project = ""){

  if (length(project) > 1 | !(project %in% c("GH", "MG", "MR", "RC", "SR", ""))) {
    stop("Invalid project.")
  }

  if (project != "") {
    p <- get_pace_tbl(paceR_db, "vPhenology", collect = FALSE) %>%
      filter(Project == project) %>%
      collect()
  }
  else {
    p <- get_pace_tbl(paceR_db, "vPhenology")
  }

  p <- p %>%
    mutate_each(funs(as.Date), contains("Date"))

  if (!full) {
    p <- p %>%
      select(Project, PhenologyDate, TreeLabel, FoodPart, Measurement,
             PhenologyScore, PhenologyPercent, PhenologyCount)
  }

  return(p)
}

#' Get table with alpha male tenures.
#'
#' @param paceR_db The src_mysql connection to the PaceR Database.
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' getv_AlphaMaleTenure(paceR_db)
getv_AlphaMaleTenure <- function(paceR_db, full = TRUE){

  alpha_tenures <- get_pace_tbl(paceR_db, "vAlphaMaleTenure") %>%
    arrange(GroupCode, AMT_DateStart) %>%
    select(GroupCode, GroupName, AMT_DateStart, AMT_DateEnd,
           AlphaMaleID, AlphaMale, AlphaMaleDOB,
           AMT_Comments, AlphaMaleTenureID) %>%
    mutate_each(funs(as.Date), contains("Date"), AlphaMaleDOB)

  if (!full) {
    alpha_tenures <- alpha_tenures %>%
      select(GroupCode, AMT_DateStart, AMT_DateEnd, AlphaMale)
    # sorted out: GroupName, AlphaMaleID, AlphaMaleDOB, AMT_Comments, AlphaMaleTenureID
  }
  return(alpha_tenures)
}

#' Get table with alpha female tenures.
#'
#' @param paceR_db The src_mysql connection to the PaceR Database.
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' getv_AlphaFemaleTenure(paceR_db)
getv_AlphaFemaleTenure <- function(paceR_db, full = TRUE){

  alpha_tenures <- get_pace_tbl(paceR_db, "vAlphaFemaleTenure") %>%
    arrange(GroupCode, AFT_DateStart) %>%
    select(GroupCode, GroupName, AFT_DateStart, AFT_DateEnd,
           AlphaFemaleID, AlphaFemale, AlphaFemaleDOB,
           AFT_Comments, AlphaFemaleTenureID) %>%
    mutate_each(funs(as.Date), contains("Date"), AlphaFemaleDOB)

  if (!full) {
    alpha_tenures <- alpha_tenures %>%
      select(GroupCode, AFT_DateStart, AFT_DateEnd, AlphaFemale)
    # sorted out: GroupName, AlphaMaleID, AlphaMaleDOB, AMT_Comments, AlphaMaleTenureID
  }
  return(alpha_tenures)
}

#' Get table with Dominance hierachies.
#'
#' @param paceR_db The src_mysql connection to the PaceR Database.
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' getv_DominanceHierarchy(paceR_db)
getv_DominanceHierarchy <- function(paceR_db, full = TRUE){

  hierarchy <- get_pace_tbl(paceR_db, "vDominanceHierarchy") %>%
    select(DominanceHierarchyID, SpeciesCommonName, GroupName, GroupCode, HierarchyDateStart,
           HierarchyDateEnd, HierarchyComments, NameOf, DateOfBirth, Sex, Rank, Comments) %>%
    mutate_each(funs(as.Date), contains("Date"), DateOfBirth) %>%
    arrange(GroupCode, HierarchyDateStart, Rank)


  if (!full) {
    hierarchy <- hierarchy %>%
      select(GroupCode, HierarchyDateStart, HierarchyDateEnd,
             HierarchyComments, NameOf, DateOfBirth, Sex, Rank, Comments)
    # sorted out: DominanceHierarchyID, SpeciesCommonName, GroupName,
  }
  return(hierarchy)
}