#' Get any table from PACE without the annoying warning messages.
#' Note that the foreign key IDs are not matched to their readable equivalents.
#'
#' @param pace_db The src_mysql connection to the PACE Database.
#' @param tbl_name Name of the table, view, or query you want to get.
#'
#' @export
#' @examples
#' get_pace_tbl(pace_db, "tblGroup")
get_pace_tbl <- function(pace_db, tbl_name){

  get_pace_tbl_internal <- function(pace_db, tbl_name){

    t1 <- pace_db %>%
      tbl(tbl_name) %>%
      select(-starts_with("TimeStamp"), -starts_with("UserAdd")) %>%
      collect()

    return(t1)
  }

  t2 <- suppressWarnings(get_pace_tbl_internal(pace_db, tbl_name))

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

  individual <- get_pace_tbl(pace_db, "tblIndividual")

  project <- get_pace_tbl(pace_db, "tblProject") %>%
    select(ProjectID = ID, ProjectName = NameOf)

  species <- get_pace_tbl(pace_db, "tblPrimateSpecies") %>%
    select(PrimateSpeciesID = ID, PrimateSpecies = Code)

  mother <- individual %>%
    select(MotherID = ID, Mother = NameOf)

  sex <- get_pace_tbl(pace_db, "codeSex") %>%
    select(SexID = ID, Sex)

  group_birth <- get_pace_tbl(pace_db, "tblGroup") %>%
    select(GroupAtBirthID = ID, GroupAtBirth = NameLong)

  age <- get_pace_tbl(pace_db, "codeAgeClass") %>%
    select(AgeClassAtFirstSightingID = ID, AgeClassAtFirstSighting = AgeClass)

  group_sighting <- group_birth %>%
    select(GroupAtFirstSightingID = GroupAtBirthID,
           GroupAtFirstSighting = GroupAtBirth)

  vision <- get_pace_tbl(pace_db, "codeColorVisionPhenotype") %>%
    select(VisionPhenotypeID = ID, VisionPhenotype = Phenotype)


  ind <- individual %>%
    inner_join(project, by = c("ProjectID" = "ProjectID")) %>%
    inner_join(species, by = c("PrimateSpeciesID")) %>%
    left_join(mother, by = c("MotherID" = "MotherID")) %>%
    left_join(sex, by = c("SexID" = "SexID")) %>%
    left_join(group_birth, by = c("GroupAtBirthID" = "GroupAtBirthID")) %>%
    left_join(age, by = c("AgeClassAtFirstSightingID" = "AgeClassAtFirstSightingID")) %>%
    left_join(group_sighting, by = c("GroupAtFirstSightingID" = "GroupAtFirstSightingID")) %>%
    left_join(vision, by = "VisionPhenotypeID", "VisionPhenotypeID") %>%
    select(IndividualID = ID, ProjectName, PrimateSpecies, NameOf, CodeName, DateOfBirth,
           BirthdateSource, Sex, Mother, MatrilineID, GroupAtBirth,
           DateOfFirstSighting, DayDifference, AgeClassAtFirstSighting,
           GroupAtFirstSighting, VisionPhenotype)

  if(!full){
    ind <- ind %>%
      select(IndividualID, NameOf, ProjectName, DateOfBirth, Sex, Mother)
  }

  return(ind)

}
