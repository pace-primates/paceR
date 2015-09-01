#' Get focal behaviour recorded by Mackenzie Bergstrom for her MSc & PhD projects (SubProjectID = 13)
#' @param paceR_db The src_mysql connection to the paceR Database (view-collection).
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' get_focaldata_MB(paceR_db)

get_focaldata_MB <- function(paceR_db, full = TRUE) {
  
  focal_MB <- get_pace_tbl(paceR_db, "vFocalData", collect = FALSE) %>% 
    filter (SubProjectID == 13) %>% 
    # Only use focals with females as focals with males only test-focals
    filter (Sex == "Female") %>% 
    collect %>% 
    arrange (FocalStateID, FocalBehaviourID, FocalBehaviourInteractantID) %>% # FocalBehaviourID can be used as only NA for unique FocalStateIDs
    mutate (linenumber = row_number ()) # For controls
  
  # not in the view:
  # Comments from tblTaxon (state, interactant), tblFocalBehaviour, tblEthogram,  

   # Create column with Individual role
 individualrole <- focal_MB%>%
   filter (NameOf == InteractantNameOf) %>%
   mutate (IndividualRole = InteractantRole) %>%
   select (FocalBehaviourID, IndividualRole)
   
 focal_MB <- individualrole %>% 
   left_join (focal_MB, ., by = "FocalBehaviourID")
   
 # Remove additional lines from interactant table that are not necessary
 focal_MB <- focal_MB %>% 
   group_by (FocalBehaviourID) %>%
   mutate (nFBID = n()) %>%
   ungroup () %>%
   mutate (nFBID = ifelse (is.na(FocalBehaviourID), NA_integer_, nFBID), newlinenumber = row_number ()) %>%
   # Filter all lines 
   ## ...with only one entry in tblFocalBehaviourID, including lines with no tblFocalBehaviourInteractantID:
   filter (nFBID == 1 | 
             # ...or without FocalBehaviourID:
             is.na(nFBID) | 
             # ...or where the partner is not the focalindividual if more than 1 line in tblFocalBehaviourID:
             (nFBID > 1 & (InteractantNameOf != NameOf | is.na(InteractantNameOf)))) %>% 
   select (-nFBID)
 
 
 # Calculate derived variables (done separately for reduced datasets (i.e. using distinct ()) as much faster than using entire dataset
 # Also includes correction for out-of-view time
 stateduration <- focal_MB %>% 
   distinct (FocalStateID) %>%
   mutate (StateDuration = round (difftime (StateEnd, StateBegin, units = "mins"), digits = 2)) %>% 
   select (FocalStateID, StateDuration)
 
 outofview <- focal_MB %>% 
   distinct (FocalStateID) %>%
   filter (StateVisibilityStatus == "Out of view") %>%
   mutate (StateDuration = round (difftime (StateEnd, StateBegin, units = "mins"), digits = 2)) %>% 
   group_by (FocalID) %>%
   summarise (OutOfViewDuration = sum (StateDuration))

 age_and_focalduration <- focal_MB %>% 
   distinct (FocalID) %>% 
   mutate (AgeAtFocal = round((as.Date (FocalBegin) - as.Date (DateOfBirth))/365.25, digits = 1)) %>% 
   left_join (outofview, by = "FocalID") %>%
   mutate (FocalDuration = round (difftime (FocalEnd, FocalBegin, units = "mins"), digits = 2)) %>% 
   mutate (FocalDurationCorrected = ifelse (is.na(OutOfViewDuration), FocalDuration, FocalDuration - OutOfViewDuration)) %>%
   select (FocalID, AgeAtFocal, FocalDuration, FocalDurationCorrected)
 
 ageofinteractant <- focal_MB %>% 
   distinct (FocalBehaviourInteractantID) %>% 
   filter (NameOf != InteractantNameOf) %>% 
   mutate (InteractantAgeAtFocal = round((as.Date (FocalBegin) - as.Date (InteractantDateOfBirth))/365.25, digits = 1)) %>% 
   select (FocalBehaviourInteractantID, InteractantAgeAtFocal)
 
 behaviourduration <- focal_MB %>% 
   distinct (FocalBehaviourID) %>% 
   filter (BehaviourBegin != BehaviourEnd) %>% 
   mutate (BehaviourDuration = round (difftime (BehaviourEnd, BehaviourBegin, units = "mins"), digits = 2)) %>% 
   select (FocalBehaviourID, BehaviourDuration)
 
 focal_MB <- focal_MB %>% 
   left_join(stateduration, by = c("FocalStateID")) %>% 
   left_join(age_and_focalduration, by = "FocalID") %>% 
   left_join(ageofinteractant, by = c("FocalBehaviourInteractantID")) %>% 
   left_join(behaviourduration, by = "FocalBehaviourID") %>% 
   mutate (BehaviourDuration = ifelse (is.na (BehaviourDuration), 0, BehaviourDuration))
 
 focal_MB <- focal_MB %>%
   select (linenumber, newlinenumber, PrimateSpeciesCommonName,
           SessionBegin, SessionEnd, ContactBegin, ContactEnd,
           GroupName, GroupCode,
           FocalBegin, FocalEnd, FocalDuration, FocalDurationCorrected, 
           StateBegin, StateEnd, StateDuration,
           StateVisibilityStatus, StateSpeciesName, StateKingdom, StateBehaviourName,  
           BehaviourBegin, BehaviourEnd, BehaviourDuration,
           BehaviourClassNameOf, BehaviourName, EndedByBehaviourName,
           IndividualID, Sex, DateOfBirth, AgeAtFocal, NameOf,
           Role, IndividualRole, InteractantRole, 
           InteractantID, InteractantNameOf, InteractantSex, InteractantDateOfBirth, InteractantAgeAtFocal, 
           InteractantSpeciesName, InteractantKingdom,
           FocalComments, BehaviourComments, ResearcherNameLast,
           SessionDayID, ProjectID, SubProjectID, ContactID,
           FocalID, FocalStateID, FocalBehaviourID, FocalBehaviourInteractantID)
 
 
 # Short table
 if(!full){
   focal_MB <- focal_MB %>%
     select (linenumber, GroupCode,
             FocalBegin, FocalEnd, FocalDurationCorrected, 
             StateBegin, StateEnd, StateDuration,
             StateVisibilityStatus, StateSpeciesName, StateKingdom, StateBehaviourName,
             BehaviourBegin, BehaviourEnd, BehaviourDuration,
             BehaviourClassNameOf, BehaviourName,
             DateOfBirth, AgeAtFocal, NameOf,
             Role, IndividualRole, InteractantRole,
             InteractantNameOf, InteractantSex, InteractantDateOfBirth, InteractantAgeAtFocal,
             InteractantSpeciesName, InteractantKingdom,
             FocalComments, BehaviourComments, ResearcherNameLast,
             SessionDayID, ProjectID, SubProjectID, ContactID,
             FocalID, FocalStateID, FocalBehaviourID, FocalBehaviourInteractantID)
    
    }
  return (focal_MB)
}
