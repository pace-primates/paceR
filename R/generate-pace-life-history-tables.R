#' Get table with biography from birth until death, disappearance or end of observation of individuals.
#'
#' @param paceR_db The src_mysql connection to the paceR Database (view-collection).
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#' @param projectID Option to get data only from specific project (1-7?). Default is 1 (Santa Rosa)
#'
#' @export
#' @examples
#' get_biography(paceR_db)

get_biography <- function(paceR_db, full = TRUE, projectID = 1){
  
  individs <- getv_Individual(paceR_db) %>% 
    select (-DayDifference, -Phenotype) %>%
    filter (ProjectID %in% projectID)
  
  death <- get_pace_tbl(paceR_db, "vDeath") %>% 
    mutate (DateOfDeathFinal = ifelse (!is.na (DateOfDeath), DateOfDeath,
                                       ifelse (!is.na(DateOfDeathFromCensus), DateOfDeathFromCensus, NA))) %>%
    select (-DateOfDeath, - DateOfDeathFromCensus, -NameOf, -ProjectID)
  
  monthlycensus <- getv_CensusMonthly (paceR_db) %>% 
    filter (ProjectID %in% projectID & !is.na (IndividID))
  
  lastalive <- monthlycensus %>%
    filter (StatusCodeLong == "Alive") %>%
    group_by (IndividID) %>%
    arrange (CensusDateOf) %>%
    summarise (FirstAlive = first (CensusDateOf), 
               LastAlive = last (CensusDateOf) , GroupLastAlive = last (GroupCode))
  
  lastcensus <-  monthlycensus %>%
    group_by (IndividID) %>%
    arrange (desc (CensusDateOf)) %>%
    filter (row_number () == 1) %>%
    ungroup () %>% 
    select (IndividID, GroupLastListed = GroupCode, LastCensus = CensusDateOf, LastStatus = StatusCodeLong)
  
  censusbio <- lastalive %>%
    full_join (lastcensus, by = "IndividID") %>%
    arrange (IndividID)
  # mutate (diff = difftime (lastcensus, lastalive, units = "days"))

  biography <- death %>%
    left_join (individs, ., by = "IndividID") %>%
    right_join (censusbio, by = "IndividID") %>%
    mutate (DepartType = ifelse (LastStatus == "Alive", "End Of Observation", LastStatus)) %>%
    mutate (DepartDate = ifelse (!is.na (DateOfDeathFinal), as.Date (DateOfDeathFinal), as.Date (LastAlive))) %>%
    mutate (DepartDate = as.Date (DepartDate, origin = "1970-01-01")) %>% 
  select (ProjectID, Project, PrimateSpecies, IndividID, NameOf, Sex, DateOfBirth, BirthdateSource,
          Mother, GroupAtBirthName, GroupAtBirthCode, DateOfFirstSighting, AgeClassAtFirstSighting,
          GroupAtFirstSightingName, GroupAtFirstSightingCode,                
          DepartDate, DepartType, CauseOfDeath, DeathComments, GroupLastAlive,  GroupLastListed) %>% 
  # sorted out:  CodeName, IndividualDeathID, CauseOfDeathID, DeathSourceOfInformation
  # DateOfDeathFinal, FirstAlive, LastAlive, LastCensus, LastStatus
  mutate_at(c("DateOfBirth", "DateOfFirstSighting", "DepartDate"), as.Date) 

  if(!full){
    biography <- biography %>%
      select (Project, IndividID, NameOf, Sex, DateOfBirth, Mother, GroupAtBirthName, GroupAtBirthCode, 
              DepartDate, DepartType, CauseOfDeath, GroupLastAlive, GroupLastListed)  
    # sorted out: ProjectID, PrimateSpecies, BirthdateSource, DateOfFirstSighting
    # AgeClassAtFirstSighting, GroupAtFirstSightingName, GroupAtFirstSightingCode, DeathComments
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


#' Get table with infants including the risk of infanticide
#' as a consequence of alpha male reversals
#'
#' Unstable (i.e. risky) periods start with the end of an alpha alpha male tenureship
#' and infants younger than 1 year are assumed to be at risk.
#' This does not count for infants conceived after the takeover
#' (i.e. born more than 5.5 months = 165 days after the takeover)
#' 
#' @param paceR_db The src_mysql connection to the paceR Database (view-collection).
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#' @param projectID Option to get data only from specific project (1-7?), standard is set to 1 (Santa Rosa)
#'
#' @export
#' @examples
#' get_infant_table (paceR_db)

get_infant_table <- function(paceR_db, full = TRUE, projectID = 1){
  
  individuals <- getv_Individual (paceR_db) %>% 
    select (-DayDifference, - Phenotype) %>% 
    filter (ProjectID %in% projectID)
  
  infant_biography <- get_biography (paceR_db) %>%
    filter (!is.na(DateOfBirth)) %>% 
    filter (GroupAtBirthCode %in% c("LV", "EXCL", "GUAN", "SEND", "CP", "CPAD", "CPRM")) %>%
    mutate (InfantDateOfConception = as.Date (DateOfBirth) - 160) %>%
    mutate (AgeAtDepart = round (difftime (DepartDate, DateOfBirth, units = "weeks")/52, 2)) %>% 
    mutate (Survived1Y = ifelse (AgeAtDepart > 1, "Yes",
                                 ifelse (DepartType == "End Of Observation", "<1year at end of observation",  "No"))) %>%
    select (InfantID = IndividID, InfantName = NameOf, Mother, InfantDateOfConception, InfantDOB = DateOfBirth, 
            InfantSex = Sex, InfantGroupAtBirthName = GroupAtBirthName, InfantGroupAtBirthCode = GroupAtBirthCode, 
            Survived1Y, InfantDepartDate = DepartDate, AgeAtDepart, InfantDepartType = DepartType,
            InfantCauseOfDeath = CauseOfDeath, InfantDepartComments = DeathComments) #Use code for groups? check other queries.
  
  amt <- getv_AlphaMaleTenure (paceR_db) %>% 
    filter (GroupCode %in% c("LV", "EXCL", "GUAN", "SEND", "CP", "CPAD", "CPRM")) %>%
    mutate_at(c("AMT_DateBegin", "AMT_DateEnd", "AlphaMaleDOB"), as.Date) 
    
  # CP fissioned into CPAD and CPRM on 2013-01-01 -> query takes into account
  # whether alpha in new groups is the same as before fission
  
  amt$lastmale_CP_DateEnd <- last(amt[amt$GroupCode == "CP",][["AMT_DateEnd"]])
  amt$lastmale_CP <- amt[amt$GroupCode == "CP" & amt$AMT_DateEnd == amt$lastmale_CP_DateEnd,][["AlphaMale"]]
  amt$firstmale_CPxx_DateBegin <-  first (amt[amt$GroupCode == "CPAD",][["AMT_DateBegin"]]) 
 
  amt <- amt %>% 
    group_by(GroupCode) %>% 
    mutate(PreviousAM = ifelse (GroupCode %in% c("CPAD", "CPRM") & is.na (lag(AlphaMale)), lastmale_CP, lag (AlphaMale)),
           PreviousAMT_DateEnd = ifelse (GroupCode %in% c("CPAD", "CPRM") & is.na (lag(AlphaMale)),
                                         lastmale_CP_DateEnd, lag (AMT_DateEnd)),
           PreviousAMT_DateEnd = as.Date (PreviousAMT_DateEnd, origin = "1970-01-01"),
           NextAM = ifelse (GroupCode == "CP" & AlphaMale == "Legolas", "Legolas and Buzz (fission)", lead (AlphaMale)),
           NextAMT_DateBegin = ifelse (GroupCode == "CP" & AlphaMale == "Legolas", firstmale_CPxx_DateBegin,
                                       lead(AMT_DateBegin)),
           NextAMT_DateBegin = as.Date (NextAMT_DateBegin, origin = "1970-01-01"),
           AlphaGap = difftime (AMT_DateBegin, PreviousAMT_DateEnd, units = "days")) %>%
    ungroup %>%
    rename (AM_ID = AlphaMaleID, AM = AlphaMale, AM_DOB = AlphaMaleDOB, AMT_ID = AlphaMaleTenureID) %>% 
    select (-lastmale_CP_DateEnd, -lastmale_CP)
  
  # A birthday within 165 days (5.5 months) after a new male alpha male was established is risky --> Alpha male replacement (AMR)
  # A birthday within 364.25 days before tenure end of an alpha male is risky --> Alpha male replacement (AMR)
  # It's unclear if an (estimated) birthday within the year before or the 165 after the start of group observation is dangerous --> group stability unknown (GS_unknown)
  # During all other periods a birthday is not risky --> group is stable (GS)
  
  tenurestart_risk <- amt %>%
    mutate (TSR = ifelse (is.na(PreviousAM), "GS_unknown",
                          ifelse (GroupCode == "CPRM" & AM == "Legolas", "GS", # He also was the alpha male before the fission
                                  ifelse (AlphaGap > 365, "GS_unknown", "AMR"))),
            # If no previous alpha or gap longer than 365 days (i.e. GS_unknown) --> Begin = AMT_Begin
            # Otherwise include the gap (i.e. use TenureEnd of previous alpha)
            TSR_Begin = ifelse (TSR == "GS_unknown", AMT_DateBegin, PreviousAMT_DateEnd),
            TSR_Begin = as.Date (TSR_Begin, origin = "1970-01-01"),
            # Risk ends 5.5 months after new alpha male got established
            TSR_End = as.Date (AMT_DateBegin) + 165) %>% 
    ungroup %>% 
    filter (TSR != "GS") %>% #Only use AMR and GS_unknown
    rename (TSR_AMT_ID = AMT_ID) %>% 
    group_by (TSR_AMT_ID) %>%
    do (data.frame(GroupCode = .$GroupCode,
                   TSR_AM = .$AM,
                   TSR = .$TSR,
                   TSR_AMT_DateBegin = .$AMT_DateBegin,
                   TSR_PreviousAM = .$PreviousAM,
                   TSR_PreviousAMT_DateEnd = .$PreviousAMT_DateEnd,
                   RiskDate = seq (as.Date (.$TSR_Begin),
                                   as.Date (.$TSR_End), by = 'day'))) %>% 
    ungroup %>% 
    distinct (GroupCode, RiskDate, TSR, .keep_all = TRUE) %>% 
    # RiskDate has to be transformed as otherwise the joining doesn't work
    mutate (RiskDate = as.character(RiskDate))

   tenureend_risk <- amt %>%
     mutate (TER = ifelse (is.na (NextAM), "Censored", "AMR"),
            TER_Begin = as.Date (AMT_DateEnd) - 364.25,
            TER_End = AMT_DateEnd) %>%
    ungroup %>% 
    filter (!(TER %in% c("Censored"))) %>% # Only use AMR
    rename (TER_AMT_ID = AMT_ID) %>% 
    group_by (TER_AMT_ID) %>%
    do (data.frame(GroupCode = .$GroupCode,
                   TER_AM = .$AM,
                   TER_NextAM = .$NextAM,
                   TER_NextAMT_DateBegin = .$NextAMT_DateBegin,
                   TER = .$TER,
                   TER_AMT_DateEnd = .$AMT_DateEnd,
                   RiskDate = seq (as.Date (.$TER_Begin),
                                   as.Date (.$TER_End), by = 'day'))) %>%
    ungroup %>% 
    distinct (GroupCode, RiskDate, .keep_all = TRUE) %>% 
    # RiskDate has to be transformed as otherwise the joining doesn't work
    mutate (RiskDate = as.character(RiskDate))
  
  # Control that no RiskDate with two different risks (i.e. AMR and GS_unknown) is 0 e.g.:
  # length ((tenurestart_risk %>% group_by (Group) %>% filter (duplicated(RiskDate)))$RiskDate)
  # Build in control that always AMR is chosen and GS_unknown removed?
  
  birthrisk <- tenurestart_risk %>% 
    full_join (tenureend_risk, by = c("GroupCode", "RiskDate")) %>% 
    mutate (New_AM = ifelse (!is.na (TSR_AM), TSR_AM, TER_NextAM),
            New_AMT_Begin = ifelse (!is.na (TSR_AMT_DateBegin), TSR_AMT_DateBegin, TER_NextAMT_DateBegin),
            Old_AM = ifelse (!is.na (TSR_PreviousAM), TSR_PreviousAM, TER_AM),
            Old_AMT_End = ifelse (!is.na (TSR_PreviousAMT_DateEnd), TSR_PreviousAMT_DateEnd, TER_AMT_DateEnd)) %>% 
    mutate (RiskDate = as.Date (RiskDate), # Transform back to date as joining is done
            BirthRisk = ifelse (!is.na (TER), as.character(TER), as.character(TSR)),
            InfanticideRisk_Begin = ifelse (BirthRisk == "GS_unknown", New_AMT_Begin, Old_AMT_End)) %>% 
    mutate_at(c("New_AMT_Begin", "Old_AMT_End", "InfanticideRisk_Begin"), funs(as.Date (., origin = "1970-01-01"))) %>% 
    arrange (GroupCode, RiskDate) %>% 
    select (GroupCode, RiskDate, BirthRisk, InfanticideRisk_Begin, New_AM, New_AMT_Begin, Old_AM, Old_AMT_End)
  
  # Determine in which groups infants went after fission of CP
  group_after_CP_fission <- getv_CensusMonthly(paceR_db) %>%   
    filter (grepl ("CP", GroupCode)) %>% 
    filter (DateOfBirth >= as.Date("2012-01-01") & DateOfBirth < as.Date("2013-01-31")) %>% 
    filter (CensusDateOf > as.Date("2013-01-31") & CensusDateOf < as.Date("2013-06-30")) %>% 
    distinct (NameOf, GroupCode, .keep_all = TRUE) %>% 
    select (InfantName = NameOf, GroupDuringInfanticideRisk = GroupCode)
  
  # Check which infants 1) were born at a risky time and
  # 2) were still presents when the infanticide risk was present (i.e. the tenure of the previous alpha ended)
  # For CP, determine next Alpha and Risk depending on the group they went after the fission
  infant_table <- infant_biography %>% 
    left_join (birthrisk, by = c("InfantGroupAtBirthCode" = "GroupCode", "InfantDOB" = "RiskDate")) %>%
    # Infants that departed before the infanticide risk was real need to be sorted out
    mutate (InfanticideRisk = ifelse (InfanticideRisk_Begin < InfantDepartDate, BirthRisk, "GS")) %>% 
    mutate (InfanticideRisk = ifelse (!is.na (InfanticideRisk), InfanticideRisk, "GS")) %>% 
    left_join (group_after_CP_fission, by = "InfantName") %>% 
    mutate (GroupDuringInfanticideRisk = ifelse (!is.na (GroupDuringInfanticideRisk),
                                                 GroupDuringInfanticideRisk, InfantGroupAtBirthCode)) %>% 
    mutate (New_AM = ifelse (New_AM != "Legolas and Buzz (fission)", New_AM,
                             ifelse (InfanticideRisk == "GS", "Legolas",
                                     ifelse (GroupDuringInfanticideRisk == "CPRM", "Legolas", "Buzz")))) %>% 
    mutate (InfanticideRisk = ifelse (New_AM == "Legolas" & Old_AM == "Legolas", "GS", InfanticideRisk)) %>% 
    mutate (InfanticideRisk = ifelse (!is.na (InfanticideRisk), InfanticideRisk, "GS")) %>% 
    select (-BirthRisk, -InfanticideRisk_Begin) %>% 
    select (InfantID, InfantName, InfantSex, Mother, InfantDateOfConception, InfantDOB, InfantGroupAtBirthName, InfantGroupAtBirthCode,
            InfantDepartDate, AgeAtDepart, Survived1Y, InfanticideRisk, GroupDuringInfanticideRisk, New_AM, New_AMT_Begin, Old_AM, Old_AMT_End,
            InfantDepartType, InfantCauseOfDeath, InfantDepartComments)

  # Short version of table
  if(!full){
    infant_table <- infant_table %>%
      select (-InfantSex, -Mother, -InfantDateOfConception, -InfantGroupAtBirthName, -Old_AM, -Old_AMT_End)
  }
  
  return (infant_table)
}
