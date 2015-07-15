#' Get focal behaviour recorded by Mackenzie Bergstrom for her MSc & PhD projects (SubProjectID = 13)
#'
#' @param pace_db The src_mysql connection to the PACE Database.
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' get_biography(pace_db)

get_focaldata_MB <- function(pace_db, full = TRUE){
  
  # PART 1: Everything with exception of interactant table
  
  sessionday <- get_pace_tbl(pace_db, "tblSessionDay") %>% # was mk.1
    select (SessionDayID = ID, ProjectID, SubProjectID, SessionDayDateOf = DateOf, SessionBegin = DateTimeBegin,
            SessionEnd = DateTimeEnd) %>%
    filter (SubProjectID == 13) # Bergstrom MSc & PhD Project: 13 
    # SortedOut: SeqNum, Comments
  
  contact <- get_pace_tbl(pace_db, "tblContact") %>% # was mk.2
    select (ContactID = ID, SessionDayID, PrimateSpeciesID, GroupID, ContactBegin = DateTimeBegin,
            ContactEnd = DateTimeEnd)
    # Sorted out: GpsStartID,  GpsEndID,	ContactEventTypeID,	Comments,

  focals <- get_pace_tbl(pace_db, "tblFocal") %>% #Was mk.3a
    select (FocalID = ID,  ContactID,  FocalBegin = DateTimeBegin, FocalEnd = DateTimeEnd,
            IndividualID,  FocalComment = Comments,	DataObserverID)
  # Columns sorted out: SeqNum, IndividualAgeSexClassID, GpsID, GpsUtm, DataRecorderID, DataEnteredByID, DataLastEditedByID,
  # DateTimeEntered, DateTimeLastEdited
  
  individuals <- get_individuals (pace_db, full = TRUE) %>%
    select (IndividualID, NameOf, Sex, DateOfBirth)
  
  groups <- get_pace_tbl(pace_db, "tblGroup") %>%
    select(GroupID = ID, GroupName = NameLong, GroupNameCode = NameCode)
  
  researcher <- get_pace_tbl(pace_db, "tblResearcher") %>% 
    select (ResearcherID = ID, PersonID)
  
  person <- get_pace_tbl(pace_db, "tblPerson") %>%
    mutate (PersonName = paste (NameFirst, sep = "_", NameLast)) %>%
    select (PersonID = ID, PersonName)
 
  focalstate <- get_pace_tbl(pace_db, "tblFocalState") %>% # First table that could be filtered by used FocalIDs
    select (FocalStateID = ID, FocalID, StateBehaviourID,
            StateBegin = DateTimeBegin, StateEnd = DateTimeEnd,
            StateVisibilityStatusID = VisibilityStatusID, StateTaxonID = TaxonID)
    
  visibilitystatus <- get_pace_tbl(pace_db, "codeVisibilityStatus") %>% 
    select (StateVisibilityStatusID = ID, StateVisibilityStatus = VisibilityStatus)
  
  statetaxon  <- get_pace_tbl(pace_db, "tblTaxon") %>%
    mutate (TaxonComments = paste (Comments, Comments1, Comments2, sep = " --- ")) %>%
    mutate (TaxonComments = gsub(" --- NA", "", TaxonComments)) %>%
    mutate (TaxonComments = gsub("NA --- ", "", TaxonComments)) %>%
    select (StateTaxonID = ID, StateSpeciesName = SpeciesName, StateKingdom = Kingdom, StateTaxonComments = TaxonComments)
  
  statebehaviour <- get_pace_tbl(pace_db, "codeStateBehaviour") %>%
    select (StateBehaviourID = ID, StateBehaviour)
   
  focalbehaviour <- get_pace_tbl(pace_db, "tblFocalBehaviour") %>% # Could be filtered with FocalStateID
    select (FocalBehaviourID = ID, FocalStateID, SeqNum, BehaviourID, DirectionID,
            BehaviourBegin = DateTimeBegin, BehaviourEnd = DateTimeEnd,
            BehaviourComments = Comments)
    # TimerStart (NA), TimerEnd (NA), NInteractants (NA)
    # PlantTaxonID, PlantPartID, FoodItemID
  
  rolecode <- get_pace_tbl(pace_db, "codeBehaviourRole") %>%
    select (DirectionID = ID, Role)

  ethogram <- get_pace_tbl(pace_db, "tblEthogram")    %>% 
    select (BehaviourID = ID, BehaviourName, IsState, BehaviourClassID,
            EndedByBehaviourID, EthogramBehaviourComment = Comments)

  ethogram_end <- get_pace_tbl(pace_db, "tblEthogram") %>%
    select (EndedByBehaviourID = ID, EndedByBehaviourName = BehaviourName)
        
  classcode <- get_pace_tbl(pace_db, "codeBehaviourClass") %>%
    select (BehaviourClassID = ID, BehaviourClassNameOf = NameOf)
  
  focals_mac.1 <- sessionday %>% 
    left_join (contact, by = "SessionDayID") %>% 
    inner_join (focals, by = "ContactID") %>% 
    left_join (individuals, by = "IndividualID") %>% 
    left_join (groups, by = "GroupID") %>% 
    left_join(researcher, by = c("DataObserverID" = "ResearcherID")) %>% 
    left_join (person, by = "PersonID") %>% 
    left_join (focalstate, by = "FocalID") %>% 
    left_join (visibilitystatus, by = "StateVisibilityStatusID") %>%
    left_join (statetaxon, by = "StateTaxonID") %>%
    left_join (statebehaviour, by = "StateBehaviourID") %>% 
    left_join(focalbehaviour, by = "FocalStateID") %>% 
    left_join(rolecode, by = "DirectionID") %>%
    left_join (ethogram, by = "BehaviourID") %>%
    left_join(ethogram_end, by = "EndedByBehaviourID") %>% 
    left_join (classcode, by = "BehaviourClassID") %>% 
    select (-GroupID, -DataObserverID, -PersonID, -StateVisibilityStatusID,
            -StateTaxonID, -DirectionID, -BehaviourID, -EndedByBehaviourID, -BehaviourClassID) %>% 
    mutate (linenumber = row_number ()) %>% # For controls
    mutate (StateDuration = round (difftime (StateEnd, StateBegin, units = "mins"), digits = 2)) %>% 
    mutate (FocalDuration = round (difftime (FocalEnd, FocalBegin, units = "mins"), digits = 2))
  
  #PART 2: Add the interactant table
  interactantbehaviour <- get_pace_tbl(pace_db, "tblFocalBehaviourInteractant") %>% # Could be filtered though there were problems to collect the filtered tabl
    select (FocalBehaviourInteractantID = ID, FocalBehaviourID,
            InteractantID, InteractantAgeSexClassID = AgeSexClassID, InteractantRoleID = RoleID,
            InteractantAlloSpecificID = AlloSpecificID, InteractantSeqNum = SeqNum)
  
  interactants <- get_individuals (pace_db, full = TRUE) %>%
    select (InteractantID = IndividualID, InteractantNameOf = NameOf, InteractantSex = Sex, InteractantDateOfBirth = DateOfBirth)
  
  interactanttaxon <- get_pace_tbl(pace_db, "tblTaxon") %>%
    mutate (TaxonComments = paste (Comments, Comments1, Comments2, sep = " --- ")) %>%
    mutate (TaxonComments = gsub(" --- NA", "", TaxonComments)) %>%
    mutate (TaxonComments = gsub("NA --- ", "", TaxonComments)) %>%
    select (InteractantAlloSpecificID = ID, InteractantSpeciesName = SpeciesName, InteractantKingdom = Kingdom, InteractantTaxonComments = TaxonComments)
    
 interactantrole <- get_pace_tbl(pace_db, "codeBehaviourRole") %>%
    select (InteractantRoleID = ID, InteractantRole = Role)
    
 focals_mac.2 <- interactantbehaviour %>% 
    left_join(interactants, by = "InteractantID") %>% 
    select (-InteractantID) %>% 
    left_join (interactanttaxon, by = "InteractantAlloSpecificID") %>%
    select (-InteractantAlloSpecificID) %>%
    left_join (interactantrole, by = "InteractantRoleID") %>%
    select (-InteractantRoleID) %>% 
    left_join (focals_mac.1, ., by = "FocalBehaviourID")
 
 # Create column with Individual role
 focals_mac.3 <- focals_mac.2 %>%
   filter (NameOf == InteractantNameOf) %>%
   mutate (IndividualRole = InteractantRole) %>%
   select (FocalBehaviourID, IndividualRole) %>%
   left_join (focals_mac.2, ., by = "FocalBehaviourID") %>% 
   mutate (AgeAtFocal = round((as.Date (FocalBegin) - as.Date (DateOfBirth))/364.25, digits = 1),
           InteractantAgeAtFocal = round((as.Date (FocalBegin) - as.Date (InteractantDateOfBirth))/364.25, digits = 1))
  
 
 # Remove additional lines from interactant table that are not necessary
 focals_mac.4 <- focals_mac.3 %>% 
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
 
  if(!full){
    focals_mac.4 <- focals_mac.4 %>%
      select (linenumber, GroupNameCode,
              FocalBegin, FocalEnd, FocalDuration,
              StateBegin, StateEnd, StateDuration, StateVisibilityStatus, StateSpeciesName, StateBehaviour,
              SeqNum, BehaviourBegin, BehaviourEnd, BehaviourClassNameOf, BehaviourName, EndedByBehaviourName, 
              Sex, DateOfBirth, AgeAtFocal, NameOf,
              Role, InteractantRole, IndividualRole,
              InteractantSeqNum, InteractantNameOf, InteractantSex, InteractantDateOfBirth, InteractantAgeAtFocal, InteractantSpeciesName, InteractantKingdom,
              FocalID, FocalStateID, FocalBehaviourID, FocalBehaviourInteractantID,
              PersonName, FocalComment, StateTaxonComments, BehaviourComments, EthogramBehaviourComment, InteractantTaxonComments)
    
    }
  return (focals_mac.4)
}
