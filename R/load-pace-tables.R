#' Get any table from PACE without the annoying warning messages.
#' Note that the foreign key IDs are not matched to their readable equivalents.
#'
#' @param pace_db The src_mysql connection to the PACE Database.
#' @param tbl_name Name of the table, view, or query you want to get.
#' @param collect Option to collect the data or not. Default is TRUE.
#'
#' @export
#' @examples
#' get_pace_tbl(pace_db, "tblGroup")

get_pace_tbl <- function(pace_db, tbl_name, collect = TRUE){

  get_pace_tbl_internal <- function(pace_db, tbl_name, collect){

    t1 <- pace_db %>%
      tbl(tbl_name) %>%
      select(everything(), -matches("TimeStamp"), -matches("UserAdd"))

    if (collect) {
      return(collect(t1))
    }
    else {
     return(t1)
    }
  }

  t2 <- suppressWarnings(get_pace_tbl_internal(pace_db, tbl_name, collect))

  return(t2)

}

#' Get the Individuals table.
#'
#' @param pace_db The src_mysql connection to the PACE Database.
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' get_individuals(pace_db)
get_individuals <- function(pace_db, full = TRUE){

  # Get the individuals table
  individual <- get_pace_tbl(pace_db, "tblIndividual")

  # Get various other tables referrenced by tblIndvidual by an ID field
  project <- get_pace_tbl(pace_db, "tblProject") %>%
    select(ProjectID = ID, ProjectName = NameOf)

  species <- get_pace_tbl(pace_db, "tblPrimateSpecies") %>%
    select(PrimateSpeciesID = ID, PrimateSpecies = Code)

  mother <- individual %>%
    select(MotherID = ID, Mother = NameOf)

  sex <- get_pace_tbl(pace_db, "codeSex") %>%
    select(SexID = ID, Sex)

  group_birth <- get_pace_tbl(pace_db, "tblGroup") %>%
    select(GroupAtBirthID = ID, GroupAtBirth = NameLong, GroupAtBirthCode = NameCode)

  age <- get_pace_tbl(pace_db, "codeAgeClass") %>%
    select(AgeClassAtFirstSightingID = ID, AgeClassAtFirstSighting = AgeClass)

  group_sighting <- group_birth %>%
    select(GroupAtFirstSightingID = GroupAtBirthID,
           GroupAtFirstSighting = GroupAtBirth)

  vision <- get_pace_tbl(pace_db, "codeColorVisionPhenotype") %>%
    select(VisionPhenotypeID = ID, VisionPhenotype = Phenotype)


  # Join on ID fields
  ind <- individual %>%
    inner_join(project, by = "ProjectID") %>%
    inner_join(species, by = "PrimateSpeciesID") %>%
    left_join(mother, by = "MotherID") %>%
    left_join(sex, by = "SexID") %>%
    left_join(group_birth, by = "GroupAtBirthID") %>%
    left_join(age, by = "AgeClassAtFirstSightingID") %>%
    left_join(group_sighting, by = "GroupAtFirstSightingID") %>%
    left_join(vision, by = "VisionPhenotypeID") %>%
    select(ProjectID, IndividualID = ID, ProjectName, PrimateSpecies, NameOf, CodeName, DateOfBirth,
           BirthdateSource, Sex, Mother, MatrilineID, GroupAtBirth, GroupAtBirthCode,
           DateOfFirstSighting, DayDifference, AgeClassAtFirstSighting,
           GroupAtFirstSighting, VisionPhenotype)

  ind <- ind %>%
    mutate_each(funs(as.Date), starts_with("Date"))

  # All comments are sorted out, maybe put them back in for the full-full table?
  # I would also suggest to use rename () and select (-) to select the columns for the final table
  # as this would be easier to know what was sorted out

  if (!full) {
    ind <- ind %>%
      select(IndividualID, NameOf, ProjectName, DateOfBirth, Sex)
  }

  return(ind)

}

#' Get table with group-census data - New Version
#'
#' @param paceR_db The src_mysql connection to the paceR Database.
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' get_monthly_census(paceR_db)
get_monthly_census <- function(paceR_db, full = TRUE){
  
  census <- get_pace_tbl(paceR_db, "vCensusMonthly")
  
  str (census)
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


#' Get the Alphamale-tenure table.
#'
#' @param pace_db The src_mysql connection to the PACE Database.
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' get_alphamale_tenures(pace_db)

get_alphamale_tenures <- function(pace_db, full = TRUE){

  amt <- get_pace_tbl(pace_db, "tblAlphaMaleTenure") %>%
    select(AlphaMaleTenureID = ID, GroupID, AlphaMaleID, AMT_DateStart = DateStart,
            AMT_DateEnd = DateEnd, AMT_Comments = Comments)


  # Get the individuals table
  males_amt <- get_individuals(pace_db) %>%
    select(AlphaMaleID = IndividualID, AlphaMale = NameOf, AlphaMaleDOB = DateOfBirth)

  groups_amt <- get_pace_tbl(pace_db, "tblGroup") %>%
    select(GroupID = ID, Group = NameCode)

  alpha_tenures <- amt %>%
    inner_join(males_amt, by = "AlphaMaleID") %>%
    inner_join(groups_amt, by = "GroupID") %>%
    arrange(GroupID, AMT_DateStart) %>%
    select(GroupID, Group, AMT_DateStart, AMT_DateEnd, AlphaMaleID, AlphaMale,
           AlphaMaleDOB, AMT_Comments, AlphaMaleTenureID)

  alpha_tenures <- alpha_tenures %>%
    mutate_each(funs(as.Date), contains("Date"), AlphaMaleDOB)

  if (!full) {
    alpha_tenures <- alpha_tenures %>%
      select(-GroupID, -AlphaMaleID, -AlphaMaleDOB, -AMT_Comments, -AlphaMaleTenureID)
  }

  return(alpha_tenures)

}
