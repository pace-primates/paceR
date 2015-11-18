#' Get focal behaviour recorded by Sarah Carnegie's for her PhD project (SubProjectID = 12)
#' @param paceR_db The src_mysql connection to the paceR Database (view-collection).
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' get_focaldata_SC(paceR_db)

get_focaldata_SC <- function(paceR_db, full = TRUE) {

  focal_SC <- get_pace_tbl(paceR_db, "vFocalData", collect = FALSE) %>% 
    filter (SubProjectID == 12) %>% 
    # filter (Sex == "F") %>% 
    select (-ResearcherNameLast, -StateSpeciesName, -StateKingdom, -BehaviourClassNameOf, -EndedByBehaviourName) %>% 
    collect %>% 
    arrange (FocalStateID, FocalBehaviourID, FocalBehaviourInteractantID) %>% # Can be sorted like this?
    mutate (linenumber = row_number ()) # For controls
  # All sorted out colums are NA
  # not in the view:
  # Comments from tblTaxon (state, interactant), tblFocalBehaviour, tblEthogram,  

#######  
# Replace Chili by Burrito (the first Chili disappeared in 2003, the Chili in Sarah's data is Burrito in pacelab)
  # TO DO:
  # Only temporary solution, this step should be done before importing Sarah's data into pacelab
  
  burrito <- get_pace_tbl(paceR_db, "vIndividual", collect = FALSE) %>% 
    filter (NameOf == "Burrito") %>% 
    collect ()
  
  focal_SC <- focal_SC %>% 
    mutate (InteractantNameOf = ifelse (InteractantNameOf == "Chili", "Burrito", InteractantNameOf),
            InteractantDateOfBirth = ifelse (InteractantNameOf == "Chilie", burrito$DateOfBirth, InteractantDateOfBirth))

##########
  
  # Create column with Individual role
  individualrole <- focal_SC %>%
    filter (NameOf == InteractantNameOf) %>%
    mutate (IndividualRole = InteractantRole) %>%
    select (FocalBehaviourID, IndividualRole)
  
  focal_SC <- individualrole %>% 
    left_join (focal_SC, ., by = "FocalBehaviourID")
  
  # Remove additional lines from interactant table that are not necessary
  # THIS HAS TO BE CONTROLLED
  focal_SC <- focal_SC %>% 
    group_by (FocalBehaviourID) %>%
    mutate (nFBID = n()) %>%
    ungroup () %>%
    mutate (nFBID = ifelse (is.na(FocalBehaviourID), NA_integer_, nFBID),
            newlinenumber = row_number ()) %>%
    # Filter all lines 
    ## 1. with only one entry in tblFocalBehaviourID, including lines with no tblFocalBehaviourInteractantID:
    filter (nFBID == 1 | 
              # 2. Without FocalBehaviourID:
              is.na(nFBID) | 
              # 3. Where the partner is not the focalindividual if more than 1 line in tblFocalBehaviourID:
              (nFBID > 1 & (InteractantNameOf != NameOf | is.na(InteractantNameOf)))) %>% 
    select (-nFBID)
  
  # Calculate derived variables (done separately for reduced datasets (i.e. using distinct ()) as much faster than using entire dataset
  # Also includes correction for out-of-view time
  
  stateduration <- focal_SC %>% 
    distinct (FocalStateID) %>%
    mutate (StateDuration = round (difftime (StateEnd, StateBegin, units = "mins"), digits = 2)) %>% 
    select (FocalStateID, StateDuration)
  
  outofview <- focal_SC %>% 
    distinct (FocalStateID) %>%
    filter (StateVisibilityStatus == "Out of view") %>%
    mutate (StateDuration = round (difftime (StateEnd, StateBegin, units = "mins"), digits = 2)) %>% 
    group_by (FocalID) %>%
    summarise (OutOfViewDuration = sum (StateDuration))
  
  age_and_focalduration <- focal_SC %>% 
    distinct (FocalID) %>% 
    mutate (AgeAtFocal = round((as.Date (FocalBegin) - as.Date (DateOfBirth))/365.25, digits = 1)) %>% 
    left_join (outofview, by = "FocalID") %>%
    mutate (FocalDuration = round (difftime (FocalEnd, FocalBegin, units = "mins"), digits = 2)) %>% 
    mutate (FocalDurationCorrected = ifelse (is.na(OutOfViewDuration), FocalDuration, FocalDuration - OutOfViewDuration)) %>%
    select (FocalID, AgeAtFocal, FocalDuration, OutOfViewDuration, FocalDurationCorrected)
  
  ageofinteractant <- focal_SC %>% 
    distinct (FocalBehaviourInteractantID) %>% 
    filter (NameOf != InteractantNameOf) %>% 
    mutate (InteractantAgeAtFocal = round((as.Date (FocalBegin) - as.Date (InteractantDateOfBirth))/365.25, digits = 1)) %>% 
    select (FocalBehaviourInteractantID, InteractantAgeAtFocal)
  
    # View (ageofinteractant %>% select (InteractantNameOf, InteractantDateOfBirth, InteractantAgeAtFocal) %>% 
          # distinct (InteractantNameOf, InteractantAgeAtFocal))
  
  behaviourduration <- focal_SC %>% 
    distinct (FocalBehaviourID) %>% 
    #mutate (BehaviourBegin = as.Date (BehaviourBegin), BehaviourEnd = as.Date (BehaviourEnd)) %>% 
    filter (BehaviourBegin != BehaviourEnd) %>% 
    mutate (BehaviourDuration = round (difftime (BehaviourEnd, BehaviourBegin, units = "mins"), digits = 2)) %>% 
    select (FocalBehaviourID, BehaviourDuration)
  
  focal_SC <- focal_SC %>% 
    left_join(stateduration, by = c("FocalStateID")) %>% 
    left_join(age_and_focalduration, by = "FocalID") %>% 
    left_join(ageofinteractant, by = c("FocalBehaviourInteractantID")) %>% 
    left_join(behaviourduration, by = "FocalBehaviourID") %>% 
    mutate (BehaviourDuration = ifelse (is.na (BehaviourDuration), 0, BehaviourDuration))

  #############
  # Add AgeClass from Censusdata
  
  safe.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = class(yes)) 
  
   partnerAC <- getv_CensusMonthly (paceR_db) %>%
    group_by (NameOf, CensusAgeClass, DateOfBirth, Sex) %>% 
    summarise (FirstDateAC = min (CensusDateOf),
               LastDateAC = max (CensusDateOf)) %>% 
    ungroup %>%
    mutate (FirstDateAC = safe.ifelse (CensusAgeClass == "Infant",  DateOfBirth, FirstDateAC)) %>% 
    arrange (NameOf, FirstDateAC) %>% 
    group_by (NameOf) %>% 
    mutate (LeftAC = safe.ifelse (!is.na(lead (FirstDateAC)), lead (FirstDateAC) - 1, LastDateAC)) %>%
    ungroup
  
  getAC <- function (Name, Date) {
    AC <- partnerAC [partnerAC$NameOf == Name & Date %within% new_interval(partnerAC$FirstDateAC, partnerAC$LeftAC), ]$CensusAgeClass
    AC <- ifelse (grepl ("Juv", Name), "Juvenile",
                  ifelse (grepl ("Inf", Name), "Infant",
                          ifelse (grepl ("Adult", Name), "Adult", AC)))
    AC <- ifelse (length(AC) == 0, "not in census at focaldate", AC)
    AC <- ifelse (is.na(AC), "not in census at focaldate", AC)
    return (AC)
  }
  
  sc.temp <- focal_SC %>%
    filter (!is.na (InteractantNameOf)) %>% 
    select (Name = InteractantNameOf, Date = FocalBegin) %>% 
    mutate (Date = as.Date (Date)) %>% 
    distinct (Name, Date)
  
  InteractantAgeClass <- unlist (lapply (1:nrow(sc.temp), function (i) getAC (sc.temp$Name[i], sc.temp$Date[i])))
  sc.temp <- cbind (sc.temp, InteractantAgeClass)
  
  focal_SC <- focal_SC %>% 
    mutate (Date = as.Date (FocalBegin)) %>% 
    left_join(., sc.temp, by = c("InteractantNameOf" = "Name", "Date")) %>% 
    select (-Date)
  ###############
  
  
  focal_SC <- focal_SC %>%
    mutate(BehaviourName = ifelse(BehaviourName == "groom" & Role == "Contact", "groom in contact",
                                  ifelse(BehaviourName == "groom" & Role == "Proximity", "groom in proximity", BehaviourName)))
  
  focal_SC <- focal_SC %>%
    select (linenumber, newlinenumber, PrimateSpeciesCommonName,
            SessionBegin, SessionEnd, ContactBegin, ContactEnd,
            GroupName, GroupCode,
            FocalBegin, FocalEnd, FocalDuration, FocalDurationCorrected, 
            StateBegin, StateEnd, StateDuration,
            StateVisibilityStatus, StateBehaviourName,  
            BehaviourBegin, BehaviourEnd, BehaviourDuration, BehaviourName,
            IndividualID, Sex, DateOfBirth, AgeAtFocal, NameOf,
            Role, IndividualRole, InteractantRole, 
            InteractantID, InteractantNameOf, InteractantSex, InteractantDateOfBirth, InteractantAgeAtFocal, InteractantAgeClass,
            InteractantSpeciesName, InteractantKingdom,
            FocalComments, BehaviourComments,
            SessionDayID, ProjectID, SubProjectID, ContactID,
            FocalID, FocalStateID, FocalBehaviourID, FocalBehaviourInteractantID)
  
  # Short table
  if(!full) {
    focal_SC <- focal_SC %>%
      select (linenumber, GroupCode,
              FocalBegin, FocalEnd, FocalDurationCorrected, 
              StateBegin, StateEnd, StateDuration,
              StateVisibilityStatus, StateBehaviourName,
              BehaviourBegin, BehaviourEnd, BehaviourDuration, BehaviourName,
              DateOfBirth, AgeAtFocal, NameOf,
              Role, IndividualRole, InteractantRole,
              InteractantNameOf, InteractantSex, InteractantDateOfBirth, InteractantAgeAtFocal, InteractantAgeClass,
              InteractantSpeciesName, InteractantKingdom,
              FocalComments, BehaviourComments,
              SessionDayID, ProjectID, SubProjectID, ContactID,
              FocalID, FocalStateID, FocalBehaviourID, FocalBehaviourInteractantID)
    
  }
  return (focal_SC)
}
