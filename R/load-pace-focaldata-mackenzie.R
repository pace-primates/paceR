#' Get focal behaviour recorded by Mackenzie Bergstrom for her MSc & PhD projects (SubProjectID = 13)
#' @param paceR_db The src_mysql connection to the paceR Database (view-collection).
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' get_focaldata_MB(paceR_db)

get_focaldata_MB <- function(paceR_db, full = TRUE){
  
  focal_MB <- get_pace_tbl(paceR_db, "vFocalData", collect = FALSE) %>%
    filter (SubProjectID == 13) %>% 
    # Only use focals with females as focals with males only test-focals
    filter (Sex == "F") %>% 
    # Filter out Focal with "Nymphadora Tonks" --> was focal with unknown female "Tonks" and should be corrected in pacelab
    filter (NameOf != "NymphadoraTonks") %>% 
    collect (n = Inf) %>% 
    arrange (StateBegin, BehavBegin, FocalBehavInteractID) %>% # BehaviourBegin can be used as only NA for unique FocalStateIDs (and therefore StateBegin)
    mutate (linenumber = row_number ()) # For controls
 
  # not in the view:
  # Comments from tblTaxon (state, interactant), tblFocalBehaviour, tblEthogram,  

   # Create column with Individual role
 individrole <- focal_MB%>%
   filter (NameOf == InteractNameOf) %>%
   mutate (IndividRole = InteractRole) %>%
   select (FocalBehavID, IndividRole)
   
 focal_MB <- individrole %>% 
   left_join (focal_MB, ., by = "FocalBehavID")
   
 # Remove additional lines from interactant table that are not necessary
 focal_MB <- focal_MB %>% 
   group_by (FocalBehavID) %>%
   mutate (nFBID = n()) %>%
   ungroup () %>%
   mutate (nFBID = ifelse (is.na(FocalBehavID), NA_integer_, nFBID), newlinenumber = row_number ()) %>%
   # Filter all lines 
   ## ...with only one entry in tblFocalBehaviourID, including lines with no tblFocalBehaviourInteractantID:
   filter (nFBID == 1 | 
             # ...or without FocalBehaviourID:
             is.na(nFBID) | 
             # ...or where the partner is not the focalindividual if more than 1 line in tblFocalBehaviourID:
             (nFBID > 1 & (InteractNameOf != NameOf | is.na(InteractNameOf)))) %>% 
   select (-nFBID)
 
 
 # Calculate derived variables (done separately for reduced datasets (i.e. using distinct ()) as much faster than using entire dataset
 # Also includes correction for out-of-view time
 stateduration <- focal_MB %>% 
   distinct (FocalStateID, .keep_all = TRUE) %>%
   mutate (StateDuration = round (difftime (StateEnd, StateBegin, units = "mins"), digits = 2)) %>% 
   select (FocalStateID, StateDuration)
 
 outofview <- focal_MB %>% 
   distinct (FocalStateID, .keep_all = TRUE) %>%
   filter (StateVisibilityStatus == "OV") %>%
   mutate (StateDuration = round (difftime (StateEnd, StateBegin, units = "mins"), digits = 2)) %>% 
   group_by (FocalID) %>%
   summarise (OutOfViewDuration = sum (StateDuration))

 age_and_focalduration <- focal_MB %>% 
   distinct (FocalID, .keep_all = TRUE) %>% 
   mutate (AgeAtFocal = round((as.Date (FocalBegin) - as.Date (DateOfBirth))/365.25, digits = 1)) %>% 
   left_join (outofview, by = "FocalID") %>%
   mutate (FocalDuration = round (difftime (FocalEnd, FocalBegin, units = "mins"), digits = 2)) %>% 
   mutate (FocalDurationCorrected = ifelse (is.na(OutOfViewDuration), FocalDuration, FocalDuration - OutOfViewDuration)) %>%
   select (FocalID, AgeAtFocal, FocalDuration, FocalDurationCorrected)
 
 ageofinteract <- focal_MB %>% 
   distinct (FocalBehavInteractID, .keep_all = TRUE) %>% 
   filter (NameOf != InteractNameOf) %>% 
   mutate (InteractAgeAtFocal = round((as.Date (FocalBegin) - as.Date (InteractDateOfBirth))/365.25, digits = 1)) %>% 
   select (FocalBehavInteractID, InteractAgeAtFocal)
 
 behavduration <- focal_MB %>% 
   distinct (FocalBehavID, .keep_all = TRUE) %>% 
   filter (BehavBegin != BehavEnd) %>% 
   mutate (BehavDuration = round (difftime (BehavEnd, BehavBegin, units = "mins"), digits = 2)) %>% 
   select (FocalBehavID, BehavDuration)
 
 focal_MB <- focal_MB %>% 
   left_join(stateduration, by = c("FocalStateID")) %>% 
   left_join(age_and_focalduration, by = "FocalID") %>% 
   left_join(ageofinteract, by = c("FocalBehavInteractID")) %>% 
   left_join(behavduration, by = "FocalBehavID") %>% 
   mutate (BehavDuration = ifelse (is.na (BehavDuration), 0, BehavDuration))
 
 focal_MB <- focal_MB %>%
   select (linenumber, newlinenumber, PrimateSpeciesCommonName,
           SessionBegin, SessionEnd, ContactBegin, ContactEnd,
           GroupName, GroupCode,
           FocalBegin, FocalEnd, FocalDuration, FocalDurationCorrected, 
           StateBegin, StateEnd, StateDuration,
           StateVisibilityStatus, StateSpeciesName, StateKingdom, StateBehavName,  
           BehavBegin, BehavEnd, BehavDuration,
           BehavClassNameOf, BehavName, EndedByBehavName,
           IndividID, Sex, DateOfBirth, AgeAtFocal, NameOf,
           Role, IndividRole, InteractRole, 
           InteractID, InteractNameOf, InteractSex, InteractDateOfBirth, InteractAgeAtFocal, 
           InteractSpeciesName, InteractKingdom,
           FocalComments, BehaviourComments, ResearcherNameLast,
           SessionDayID, ProjectID, SubProjectID, ContactID,
           FocalID, FocalStateID, FocalBehavID, FocalBehavInteractID)
 
 
 # Short table
 if(!full){
   focal_MB <- focal_MB %>%
     select (linenumber, GroupCode,
             FocalBegin, FocalEnd, FocalDurationCorrected, 
             StateBegin, StateEnd, StateDuration,
             StateVisibilityStatus, StateSpeciesName, StateKingdom, StateBehavName,
             BehavBegin, BehavEnd, BehavDuration,
             BehavClassNameOf, BehavName,
             DateOfBirth, AgeAtFocal, NameOf,
             Role, IndividRole, InteractRole,
             InteractNameOf, InteractSex, InteractDateOfBirth, InteractAgeAtFocal,
             InteractSpeciesName, InteractKingdom,
             FocalComments, BehaviourComments, ResearcherNameLast,
             SessionDayID, ProjectID, SubProjectID, ContactID,
             FocalID, FocalStateID, FocalBehavID, FocalBehavInteractID)
    
 }
 print("Rafiki and RafikiNew are not distinguished --> Check for date of his disappearance replace him by RafikiNew afterwards")
return (focal_MB)
}


#' Get proximity scans recorded by Mackenzie Bergstrom for her MSc & PhD projects (SubProjectID = 13)
#' @param paceR_db The src_mysql connection to the paceR Database (view-collection).
#'
#' @export
#' @examples
#' get_proximity_MB(paceR_db)

get_proximity_MB <- function(paceR_db){
  
  prox_MB <- get_pace_tbl(paceR_db, "vProximity", collect = FALSE) %>% 
    filter (SubProjectID == 13) %>% 
    # Only use focals with females as focals with males only test-focals
    filter (Sex == "F") %>% 
    # Filter out Focal with "Nymphadora Tonks" --> was focal with unknown female "Tonks" and should be corrected in pacelab
    filter (NameOf != "NymphadoraTonks") %>% 
    collect (n = Inf) %>% 
    select(GroupNameCode, ContactID, FocalID, NameOf, Sex, contains("FocalDate"),
           FocalScanID, FocalScanDateTime, FocalScanIsInSight,
           FocalScanIndividID, ProximityNameOf,
           ProximitySex, ProximityDOB, ProximityCode) %>% 
    arrange(FocalScanDateTime, ProximityNameOf)

  return (prox_MB)
}
