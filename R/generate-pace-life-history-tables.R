#' Get table with biography from birth until death, disappearance or end of observation of individuals.
#'
#' @param pace_db The src_mysql connection to the PACE Database.
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#' @param projectID Option to get data only from specific project (1-7?). Default is 1 (Santa Rosa)
#'
#' @export
#' @examples
#' get_biography(pace_db)

get_biography <- function(pace_db, full = TRUE, projectID = 1){

  individuals <- get_individuals (pace_db, full = TRUE) %>%
    select (-DayDifference, - VisionPhenotype) %>%
    filter (ProjectID %in% projectID)
  # Missing from tblIndividual: comment, comment2, commentJFA, maybe include into full-full table

  death <- get_pace_tbl(pace_db, "tblIndividualDeath") %>%
    select (IndividualDeathID = ID, IndividualID, DateOfDeath, CauseOfDeathID, DeathSourceOfInformation = SourceOfInformation,
            DateOfDeathFromCensus, DeathComments = Comments) %>%
    mutate (DateOfDeathFinal = ifelse (!is.na (DateOfDeath), DateOfDeath,
                                       ifelse (!is.na(DateOfDeathFromCensus), DateOfDeathFromCensus, NA))) %>%
    select (-DateOfDeath, - DateOfDeathFromCensus)

  codeCauseOfDeath  <- get_pace_tbl (pace_db, "codeCauseOfDeath") %>%
    select (CauseOfDeathID = ID,  CauseOfDeath)

  monthlycensus <- get_monthly_census (pace_db) %>%
    filter (ProjectID %in% projectID & !is.na (IndividualID))

  lastalive <- monthlycensus %>%
    filter (Status == "Alive") %>%
    group_by (IndividualID) %>%
    arrange (CensusDateOf) %>%
    summarise (firstalive = first (CensusDateOf),
               lastalive = last (CensusDateOf))

  lastcensus <-  monthlycensus %>%
    group_by (IndividualID) %>%
    arrange (desc (CensusDateOf)) %>%
    filter (row_number () == 1) %>%
    select (IndividualID, lastcensus = CensusDateOf, laststatus = Status)

  censusbio <- lastalive %>%
    full_join (lastcensus, by = "IndividualID") %>%
    arrange (IndividualID)
  # mutate (diff = difftime (lastcensus, lastalive, units = "days"))

  biography <- death %>%
    left_join (., codeCauseOfDeath, by = "CauseOfDeathID") %>%
    left_join (individuals, ., by = "IndividualID") %>%
    right_join (censusbio, by = "IndividualID") %>%
    mutate (DepartType = ifelse (laststatus == "Alive", "End Of Observation", laststatus)) %>%
    mutate (DepartDate = ifelse (!is.na (DateOfDeathFinal), DateOfDeathFinal, lastalive)) %>%
    select (-IndividualDeathID, -CauseOfDeathID, -DeathSourceOfInformation,
            -DateOfDeathFinal, -firstalive, -lastalive, -lastcensus, -laststatus) %>%
    mutate_each (funs (as.Date), DateOfBirth, DateOfFirstSighting, DepartDate)

  if(!full){
    biography <- biography %>%
      select (-ProjectID, -PrimateSpecies, -CodeName, -BirthdateSource, -MatrilineID,
              -DateOfFirstSighting, -AgeClassAtFirstSighting, -GroupAtFirstSighting, -DeathComments)
  }

  return (biography)

  # Following things have to be controlled:
  # Compare DateOfFirstSighting with firstalive
  # Compare DateOfDeathFinal with lastcensus
  # Check cases where last censusstatus is "Alive" but long time ago --> really end of observation?
  # Check also if in cases where last censusstatus is "Alive" the individual is not dead.
  # What ifseveral lines in the end have status "Missing"? -> used lastalive as departdate and laststatus as depart type. Any errors because of that?
  # EntryType is missing

}