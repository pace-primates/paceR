#' Get focal behaviour recorded by Sarah Carnegie's for her PhD project (SubProjectID = 12)
#' @param paceR_db The src_mysql connection to the paceR Database (view-collection).
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' get_focaldata_SC(paceR_db)

get_focaldata_SC <- function(paceR_db, full = TRUE) {
  
  focal_SC <- get_pace_tbl(paceR_db, "vFocalData_SC", collect = FALSE) %>% 
    collect (n = Inf) %>% 
    # filter (Sex == "F") %>% 
    select (-DataObserverNameLast, -StateSpeciesName, -StateKingdom, -BehavClassName, -EndedByBehavName) %>% 
    arrange (FocalStateID, FocalBehavID, FocalBehavInteractID) %>% # Can be sorted like this?
    mutate (linenumber = row_number ()) # For controls
  # All sorted out colums are NA
  # not in the view:
  # Comments from tblTaxon (state, interactant), tblFocalBehaviour, tblEthogram,  
  
  #######  
  # Replace Chili by Burrito (the first Chili disappeared in 2003, the Chili in Sarah's data is Burrito in pacelab)
  # TO DO: Only temporary solution, this step should be done before importing Sarah's data into pacelab
  
  burrito <- get_pace_tbl(paceR_db, "vIndivid", collect = FALSE) %>% 
    filter (NameOf == "Burrito") %>% 
    collect ()
  
  focal_SC <- focal_SC %>% 
    mutate (InteractNameOf = ifelse (InteractNameOf == "Chili", "Burrito", InteractNameOf),
            InteractDateOfBirth = ifelse (InteractNameOf == "Chilie", burrito$DateOfBirth, InteractDateOfBirth))
  
  ##########
  
  # Create column with Individual role
  individrole <- focal_SC %>%
    filter (NameOf == InteractNameOf) %>%
    mutate (IndividRole = InteractRole) %>%
    select (FocalBehavID, IndividRole)
  
  focal_SC <- individrole %>% 
    left_join (focal_SC, ., by = "FocalBehavID")
  
  # Remove additional lines from interactant table that are not necessary
  # THIS HAS TO BE CONTROLLED
  focal_SC <- focal_SC %>% 
    group_by (FocalBehavID) %>%
    mutate (nFBID = n()) %>%
    ungroup () %>%
    mutate (nFBID = ifelse (is.na(FocalBehavID), NA_integer_, nFBID),
            newlinenumber = row_number ()) %>%
    # Filter all lines 
    ## 1. with only one entry in tblFocalBehaviourID, including lines with no tblFocalBehaviourInteractantID:
    filter (nFBID == 1 | 
              # 2. Without FocalBehaviourID:
              is.na(nFBID) | 
              # 3. Where the partner is not the focalindividual if more than 1 line in tblFocalBehaviourID:
              (nFBID > 1 & (InteractNameOf != NameOf | is.na(InteractNameOf)))) %>% 
    select (-nFBID)
  
  # Calculate derived variables (done separately for reduced datasets (i.e. using distinct ()) as much faster than using entire dataset
  # Also includes correction for out-of-view time
  
  stateduration <- focal_SC %>% 
    distinct (FocalStateID, .keep_all = TRUE) %>%
    mutate (StateDuration = round (difftime (StateEnd, StateBegin, units = "mins"), digits = 2)) %>% 
    select (FocalStateID, StateDuration)
  
  outofview <- focal_SC %>% 
    distinct (FocalStateID, .keep_all = TRUE) %>%
    filter (StateVisibilityStatus == "OV") %>%
    mutate (StateDuration = round (difftime (StateEnd, StateBegin, units = "mins"), digits = 2)) %>% 
    group_by (FocalID) %>%
    summarise (OutOfViewDuration = sum (StateDuration))
  
  age_and_focalduration <- focal_SC %>% 
    distinct (FocalID, .keep_all = TRUE) %>% 
    mutate (AgeAtFocal = round((as.Date (FocalBegin) - as.Date (DateOfBirth))/365.25, digits = 1)) %>% 
    left_join (outofview, by = "FocalID") %>%
    mutate (FocalDuration = round (difftime (FocalEnd, FocalBegin, units = "mins"), digits = 2)) %>% 
    mutate (FocalDurationCorrected = ifelse (is.na(OutOfViewDuration), FocalDuration, FocalDuration - OutOfViewDuration)) %>%
    select (FocalID, AgeAtFocal, FocalDuration, OutOfViewDuration, FocalDurationCorrected)
  
  ageofinteract <- focal_SC %>% 
    distinct (FocalBehavInteractID, .keep_all = TRUE) %>% 
    filter (NameOf != InteractNameOf) %>% 
    mutate (InteractAgeAtFocal = round((as.Date (FocalBegin) - as.Date (InteractDateOfBirth))/365.25, digits = 1)) %>% 
    select (FocalBehavInteractID, InteractAgeAtFocal)
  
  # View (ageofinteractant %>% select (InteractantNameOf, InteractantDateOfBirth, InteractantAgeAtFocal) %>% 
  # distinct (InteractantNameOf, InteractantAgeAtFocal))
  
  behavduration <- focal_SC %>% 
    distinct (FocalBehavID, .keep_all = TRUE) %>% 
    #mutate (BehaviourBegin = as.Date (BehaviourBegin), BehaviourEnd = as.Date (BehaviourEnd)) %>% 
    filter (BehavBegin != BehavEnd) %>% 
    mutate (BehavDuration = round (difftime (BehavEnd, BehavBegin, units = "mins"), digits = 2)) %>% 
    select (FocalBehavID, BehavDuration)
  
  focal_SC <- focal_SC %>% 
    left_join(stateduration, by = c("FocalStateID")) %>% 
    left_join(age_and_focalduration, by = "FocalID") %>% 
    left_join(ageofinteract, by = c("FocalBehavInteractID")) %>% 
    left_join(behavduration, by = "FocalBehavID") %>% 
    mutate (BehavDuration = ifelse (is.na (BehavDuration), 0, BehavDuration))
  
  #############
  # Add AgeClass from Censusdata
  
  partnerAC <- getv_CensusMonthly (paceR_db) %>%
    group_by (NameOf, CensusAgeClass, DateOfBirth, Sex) %>% 
    summarise (FirstDateAC = min (CensusDateOf),
               LastDateAC = max (CensusDateOf)) %>% 
    ungroup %>%
    mutate (FirstDateAC = if_else (CensusAgeClass == "Infant",  DateOfBirth, FirstDateAC)) %>% 
    arrange (NameOf, FirstDateAC) %>% 
    group_by (NameOf) %>% 
    mutate (LeftAC = if_else (!is.na(lead (FirstDateAC)), lead (FirstDateAC) - 1, LastDateAC)) %>%
    ungroup
  
  getAC <- function (Name, Date) {
    AC <- partnerAC [partnerAC$NameOf == Name & Date %within% interval(partnerAC$FirstDateAC, partnerAC$LeftAC), ]$CensusAgeClass
    AC <- ifelse (grepl ("Juv", Name), "J",
                  ifelse (grepl ("Inf", Name), "I",
                          ifelse (grepl ("Adult", Name), "A", AC)))
    AC <- ifelse (length(AC) == 0, "not in census at focaldate", AC)
    AC <- ifelse (is.na(AC), "not in census at focaldate", AC)
    return (AC)
  }
  
  sc.temp <- focal_SC %>%
    filter (!is.na (InteractNameOf)) %>% 
    select (Name = InteractNameOf, Date = FocalBegin) %>% 
    mutate (Date = as.Date (Date)) %>% 
    distinct (Name, Date)
  
  InteractAgeClass <- unlist (lapply (1:nrow(sc.temp), function (i) getAC (sc.temp$Name[i], sc.temp$Date[i])))
  sc.temp <- cbind (sc.temp, InteractAgeClass)
  
  focal_SC <- focal_SC %>% 
    mutate (Date = as.Date (FocalBegin)) %>% 
    left_join(., sc.temp, by = c("InteractNameOf" = "Name", "Date")) %>% 
    select (-Date)
  ###############
  
  
  focal_SC <- focal_SC %>%
    mutate(BehavName = ifelse(BehavName == "groom" & Role == "Contact", "groom in contact",
                              ifelse(BehavName == "groom" & Role == "Proximity", "groom in proximity", BehavName))) %>% 
    mutate(BehavName = ifelse(BehavName == "groom solicit" & Role == "Contact", "groom solicit in contact",
                              ifelse(BehavName == "groom" & Role == "Proximity", "groom solicit in proximity", BehavName)))
  
  focal_SC <- focal_SC %>%
    select (linenumber, newlinenumber, PrimateSpeciesCommonName,
            SessionBegin, SessionEnd, ContactBegin, ContactEnd,
            GroupName, GroupCode,
            FocalBegin, FocalEnd, FocalDuration, FocalDurationCorrected, 
            StateBegin, StateEnd, StateDuration,
            StateVisibilityStatus, StateBehavName,  
            BehavBegin, BehavEnd, BehavDuration, BehavName,
            IndividID, Sex, DateOfBirth, AgeAtFocal, NameOf,
            Role, IndividRole, InteractRole, 
            InteractID, InteractNameOf, InteractSex, InteractDateOfBirth, InteractAgeAtFocal, InteractAgeClass,
            InteractSpeciesName, InteractKingdom,
            FocalComments, BehaviourComments,
            SessionDayID, SubProjectID, ContactID,
            FocalID, FocalStateID, FocalBehavID, FocalBehavInteractID)
  
  # Short table
  if(!full) {
    focal_SC <- focal_SC %>%
      select (linenumber, GroupCode,
              FocalBegin, FocalEnd, FocalDurationCorrected, 
              StateBegin, StateEnd, StateDuration,
              StateVisibilityStatus, StateBehavName,
              BehavBegin, BehavEnd, BehavDuration, BehavName,
              DateOfBirth, AgeAtFocal, NameOf,
              Role, IndividRole, InteractRole,
              InteractNameOf, InteractSex, InteractDateOfBirth, InteractAgeAtFocal, InteractAgeClass,
              InteractSpeciesName, InteractKingdom,
              FocalComments, BehaviourComments,
              SessionDayID, SubProjectID, ContactID,
              FocalID, FocalStateID, FocalBehavID, FocalBehavInteractID)
    
  }
  return (focal_SC)
}