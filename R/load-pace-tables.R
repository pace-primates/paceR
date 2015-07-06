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
    inner_join(project, by = c("ProjectID" = "ProjectID")) %>% # Why not by = "ProjectID" ?
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
      select(IndividualID, NameOf, ProjectName, DateOfBirth, Sex)
  }

  return(ind)

}

#' Get table with group-census data.
#'
#' @param pace_db The src_mysql connection to the PACE Database.
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#' @param projectID Option to get data only from specific project (1-7?), standard is set to 1 (Santa Rosa)
#' 
#' @export
#' @examples
#' get_biography(pace_db)


get_monthly_census <- function(pace_db, projectID = 1, full = TRUE){
  
  project  <- get_pace_tbl (pace_db, "tblProject") %>%
    select (ProjectID = ID, ProjectNameOf = NameOf) %>%
    #sorted out: Code, LeaderID, Comments
    filter (ProjectID %in% projectID)
  
  groups <-   get_pace_tbl(pace_db, "tblGroup") %>%
    select (GroupID = ID, ProjectID, PrimateSpeciesID, GroupNameLong = NameLong, GroupNameCode = NameCode)
  # Sorted out: NameShort, NameOld, ParentGroupID, DateOfFormation (NA), 
  # CensusNumberCode?, CensusLetterCode?
  # DateOfExtinction (NA), Alias, GpdID, GpdUtm, Description, Comments
  
  species <- get_pace_tbl(pace_db, "tblPrimateSpecies") %>%
    select(PrimateSpeciesID = ID, PrimateSpecies = Code)  
  
  censusmonthly <- get_pace_tbl (pace_db, "tblCensusMonthly") %>%
    select (CensusMonthlyID = ID, GroupID, CensusDateOf = DateOf,
            CensusMonthlyComments = Comments)
  # sorted out: CensusYear (redundant, no discrepancies with DateOF),
  # CensusMonth (redundant, some discrepancies with DateOf)
  
  researcher_census <- get_pace_tbl (pace_db,"tblCensusMonthlyResearcher") %>%
    select (CensusMonthlyID, ResearcherID)
  # ID, Comments (only three)
  
  researcher  <- get_pace_tbl (pace_db, "tblResearcher") %>%
    select (ResearcherID = ID, PersonID, ResearcherComments = Comments)
  #Sorted out: ProjectID
  
  person  <- get_pace_tbl (pace_db, "tblPerson") %>%
    mutate (PersonName = paste (NameFirst, NameLast, sep = "_")) %>%
    select (PersonID = ID, PersonName)
  # Sorted out: EMail, Comments (only 1), NameFirst, NameLast
  
  censusmonthlyindividuals  <- get_pace_tbl (pace_db, "tblCensusMonthlyGroupIndivid") %>%
    select (CensusMonthlyGroupIndividID = ID, CensusMonthlyID, IndividualID, AgeSexClassID,
            StatusID, ToGroupID, FromGroupID, CensusMonthlyGroupIndividComments = Comments)
  
  to_group <-   get_pace_tbl (pace_db, "tblGroup") %>%
    select (ToGroupID = ID, ToGroupNameLong = NameLong, ToGroupNameCode = NameCode)
  
  from_group <-   get_pace_tbl (pace_db, "tblGroup") %>%
    select (FromGroupID = ID, FromGroupNameLong = NameLong, FromGroupNameCode = NameCode)
  
  codeagesexclass  <- get_pace_tbl (pace_db, "codeAgeSexClass") %>%
    select (AgeSexClassID = ID, AgeClassID, SexID)
  
  codeageclass  <- get_pace_tbl (pace_db, "codeAgeClass") %>%
    select (AgeClassID = ID, CensusAgeClass = AgeClass)
  # sorted out: Description
  
  codesex  <- get_pace_tbl (pace_db, "codeSex") %>%
    select (SexID = ID, CensusSex = Description)
  # sorted out: Sex
  
  individuals_census <- get_individuals (pace_db, full = FALSE) %>% 
    select (-ProjectName)
  
  status <- get_pace_tbl (pace_db, "codeCensusMonthlyStatus") %>%
    select (StatusID = ID, Status)
  # Sorted out: StatusCode
  
  census <- project %>% 
    inner_join (., groups, by = "ProjectID") %>%
    inner_join (., species, by = "PrimateSpeciesID") %>% 
    inner_join (., censusmonthly, by = "GroupID") %>% 
    left_join (., researcher_census, by = "CensusMonthlyID") %>% 
    left_join (., researcher, by = "ResearcherID") %>% 
    left_join (., person, by = "PersonID") %>% 
    left_join (., censusmonthlyindividuals, by = "CensusMonthlyID") %>% 
    left_join (., to_group, by = "ToGroupID") %>% 
    left_join (., from_group, by = "FromGroupID") %>% 
    left_join (., codeagesexclass, by = "AgeSexClassID") %>% 
    left_join (., codeageclass, by = "AgeClassID") %>% 
    left_join (., codesex, by = "SexID") %>% 
    left_join (., individuals_census, by = "IndividualID") %>% 
    left_join (., status, by = "StatusID") %>% 
    rename (CensusResearcherName = PersonName) %>% 
    select (-PrimateSpeciesID, -ProjectID, -GroupID,
            -ResearcherID, -ResearcherComments, -PersonID,
            -ToGroupID, - FromGroupID,
            -AgeSexClassID, -AgeClassID, -SexID,
            -IndividualID,
            -StatusID)
  
  
  if(!full){
    census <- census %>%
      select (-GroupNameLong, -CensusMonthlyID, -CensusMonthlyComments, -CensusResearcherName, -CensusMonthlyGroupIndividID, 
              -CensusMonthlyGroupIndividComments, -ToGroupNameLong, -FromGroupNameLong)
  }
  return (census)
}
