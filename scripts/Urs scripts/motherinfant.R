

####################
# HELPER TABLES
codeAgeClass  <- pace_db %>%
  tbl ("codeAgeClass") %>%
  select (AgeClassID = ID, AgeClass) %>%
  collect ()

tblGroup  <- pace_db %>%
  tbl ("tblGroup") %>%
  select (GroupID = ID, GroupCode = NameCode) %>%
  collect ()

codeCauseOfDeath  <- pace_db %>%
  tbl ("codeCauseOfDeath") %>%
  select (deathID = ID,  CauseOfDeath) %>%
  collect ()

codeSex <- pace_db %>%
  tbl ("codeSex") %>%
  select (SexID = ID, Sex, Description) %>%
  collect ()

tblIndividual  <- pace_db %>%
  tbl ("tblIndividual") %>%
  collect ()

tblIndividualDeath  <- pace_db %>%
  tbl ("tblIndividualDeath") %>%
  collect ()

######################
# 1. tblIndividual for Mothers
  # Columns: MotherName, MotherDOB, MotherComments,...

repro.1  <- tblIndividual %>%
  filter (ProjectID == 1, SexID == 2) %>% #Only female (2) capuchins from Santa Rosa (1)
  select (mother.ID = ID, mother.name = NameOf, mother.DOB = DateOfBirth,
          mother.DOBsrc = BirthdateSource,
          mother.1stsighting = DateOfFirstSighting, mother.ageclass1stsightingID = AgeClassAtFirstSightingID,
          mother.group1stsightingID = GroupAtFirstSightingID, mother.comment = Comments,
          mother.comment2 = Comments_2, mother.commentJFA = CommentsJFA)

### Add age-classes
repro.1b  <- repro.1 %>%
  left_join (., codeAgeClass, by = c ("mother.ageclass1stsightingID" = "AgeClassID")) %>%
  select (-mother.ageclass1stsightingID) %>%
  rename (mother.ageclass1stsighting = AgeClass)

### Add groupcodes
repro.1c  <- repro.1b %>%
  left_join (., tblGroup, by = c("mother.group1stsightingID" = "GroupID")) %>%
  select (-mother.group1stsightingID) %>%
  rename (mother.group1stsighting = GroupCode)

repro.1  <- repro.1c
rm (list = ls (pattern = "repro.1."))

###############################
# 2. tblIndivualDeath for Mothers
  # Columns: MotherDOD, MotherDepartType, MotherCauseDeath, SourceOfInformation,...

repro.2  <- tblIndividualDeath %>%
  select (mother.DID = ID, mother.ID = IndividualID, mother.DOD = DateOfDeath, mother.causeDID = CauseOfDeathID,
          mother.Dsrc = SourceOfInformation, mother.DODfromcensus = DateOfDeathFromCensus,
          mother.Dcomment = Comments)

### Add cause of death
repro.2b  <- repro.2 %>%
  left_join (., codeCauseOfDeath, by = c("mother.causeDID" =  "deathID")) %>%
  select (-mother.causeDID) %>%
  rename (mother.causeD =  CauseOfDeath)

repro.2  <- repro.2b
rm (list = ls (pattern = "repro.2."))

#############################
# 3. JOIN mother.tblIndividual with mother.tblIndividualDeath
  # Derived columns:
  # MotherIsAlive (i.e. no entry for mother in tblIndividualDeath)
  # MotherDateOfLastSighting (if entry in tblIndividualDeath use DOD or DODfromcensus, otherwise now)
# PUT THIS IN DERIVES MOTHER VARIABLES (~Line 250)?

repro.3  <- repro.2 %>%
  left_join (repro.1, ., by = "mother.ID") %>%
  mutate (mother.isalive = ifelse (is.na(mother.DID), 1, 0)) %>%
  mutate (mother.lastsighting = ifelse (!is.na(mother.DOD), mother.DOD,
                                        ifelse (!is.na(mother.DODfromcensus), mother.DODfromcensus, format (Sys.time(), "%Y-%m-%d")))) %>%
  select (mother.ID, mother.name, mother.DOB, mother.DOBsrc,
          mother.1stsighting, mother.ageclass1stsighting, mother.group1stsighting,
          mother.isalive, mother.lastsighting, mother.DOD, mother.Dsrc, mother.DODfromcensus, mother.causeD,
          mother.comment, mother.comment2, mother.commentJFA, mother.Dcomment)

####################################################
# 4. tblIndividual for Infants
  # Columns: InfantName, InfantSex, InfantDOB, InfantDOBSource, InfantDOE (i.e. DateAtFirstSighting),
  #         GroupAtBirth, GroupAtFirstSighting, InfantComments


repro.4  <- tblIndividual %>%
  filter (ProjectID == 1) %>% #Only capuchins from Santa Rosa (1)
  select (mother.ID = MotherID, infant.ID = ID, infant.name = NameOf, infant.DOB = DateOfBirth,
          infant.DOBsrc = BirthdateSource, infant.birthgroupID = GroupAtBirthID, infant.sexID = SexID,
          infant.1stsighting = DateOfFirstSighting, infant.ageclass1stsightingID = AgeClassAtFirstSightingID,
          infant.group1stsightingID = GroupAtFirstSightingID, infant.comment = Comments,
          infant.comment2 = Comments_2, infant.commentJFA = CommentsJFA)

#####
# add age classes
repro.4b  <- repro.4 %>%
  left_join (., codeAgeClass, by = c ("infant.ageclass1stsightingID" = "AgeClassID")) %>%
  select (-infant.ageclass1stsightingID) %>%
  rename (infant.ageclass1stsighting = AgeClass)

######
# add 1stsighting- and birthgroups
repro.4c  <- repro.4b %>%
  left_join (., tblGroup, by = c("infant.group1stsightingID" = "GroupID")) %>%
  select (-infant.group1stsightingID) %>%
  rename (infant.group1stsighting = GroupCode) %>%
  left_join (., tblGroup, by = c("infant.birthgroupID" = "GroupID")) %>%
  select (-infant.birthgroupID) %>%
  rename (infant.birthgroup = GroupCode)

######
# add infant.sex
repro.4d  <- repro.4c %>%
left_join (., codeSex, by = c("infant.sexID" = "SexID")) %>%
  select (-infant.sexID, -Sex)  %>%
  rename (infant.sex = Description)

repro.4  <- repro.4d %>%
  filter (!(is.na(infant.DOB) & !is.na(mother.ID))) # One infant probably never existes, the other two belong to non-study-group

rm (list = ls (pattern = "repro.4."))

########################################################################
#5. tblIndivualDeath for Infant
  # Columns: InfantDateOfDeath, InfantDepartType, InfantCauseOfDeath, SourceOfInformation, DateOfDeathFromCensus, Comments

repro.5  <- tblIndividualDeath %>%
  select (infant.DID = ID, infant.ID = IndividualID, infant.DOD = DateOfDeath, infant.causeDID = CauseOfDeathID,
          infant.Dsrc = SourceOfInformation, infant.DODfromcensus = DateOfDeathFromCensus,
          infant.Dcomment = Comments)

repro.5b  <- repro.5 %>%
  left_join (., codeCauseOfDeath, by = c("infant.causeDID" =  "deathID")) %>%
  select (-infant.causeDID) %>%
  rename (infant.causeD =  CauseOfDeath)

repro.5  <- repro.5b
rm (list = ls (pattern = "repro.5."))


#############################
# 6. JOIN infant.tblindividual with infant.tbldeath
  # Derived columns:
  # InfantIsAlive (i.e. no entry for infant in tblIndividualDeath)
  # InfantDateOfLastSighting (if entry in tblIndividualDeath use DOD or DODfromcensus, otherwise now)
# PUT THIS IN DERIVED INFANT VARIABLES (~Line 215)?
repro.6  <- repro.5 %>%
  left_join (repro.4, ., by = "infant.ID") %>%
  mutate (infant.isalive = ifelse (is.na(infant.DID), 1, 0)) %>%
  mutate (infant.lastsighting = ifelse (!is.na(infant.DOD), infant.DOD, ### MODIFY THIS PROCEDURES AND TAKE OLDEST DATE?
                                        ifelse (!is.na(infant.DODfromcensus), infant.DODfromcensus, format (Sys.time(), "%Y-%m-%d")))) %>%
  select (mother.ID, infant.ID, infant.name, infant.DOB, infant.DOBsrc, infant.birthgroup, infant.sex,
          infant.1stsighting, infant.ageclass1stsighting, infant.group1stsighting,
          infant.isalive, infant.lastsighting, infant.DOD, infant.Dsrc, infant.DODfromcensus, infant.causeD,
          infant.comment, infant.comment2, infant.commentJFA, infant.Dcomment)


####################################################################################
#7. JOIN MOTHER AND INFANT-TABLES

repro.7  <- repro.6 %>%
  inner_join (repro.3, ., by = "mother.ID")


################
# CHECK infant.DATE OF DEATH/DEPART
# WHY???
DODdiffs  <- repro.7 %>%
  filter (infant.isalive == 0) %>%
  mutate (infant.DODdiff = floor (difftime (infant.DODfromcensus, infant.DOD, units = "days"))) %>%
  mutate (infant.DODfinal = ifelse (is.na(infant.DOD), infant.DODfromcensus,
                                    ifelse (is.na(infant.DODfromcensus), infant.DOD,
                                            ifelse (infant.DOD < infant.DODfromcensus, infant.DOD, infant.DODfromcensus)))) %>%
  mutate (infant.ageDODfinal = round (difftime (infant.DODfinal, infant.DOB, unit = "days")/364.25, 1)) %>%
  #filter (infant.ageDODfinal > 4) %>%
  select (mother.name, infant.name, infant.sex, infant.DOD, infant.DODfromcensus, infant.DODdiff,
          infant.DODfinal, infant.ageDODfinal, infant.causeD,
          contains ("infant.comment"))

# View (DODdiffs)
rm (DODdiffs)

#######################
# DERIVE INFANT-VARIABLES
  # Age1stSighting, AgeAtEntry (age1stsighting), EntryType (i.e. seen within 120 days after birth or not)
  # DODfinal (alive==0 -> earlier of DOD or DODfromcensus), ageatdepart
  # Departtype (Death or, if no DODfinal, Observation), AgeAtDepart, Ndaysobserved
  # surv1Y, surv2Y, surv5Y

repro.7b <- repro.7 %>%
  mutate (infant.age1stsighting = floor (difftime (infant.1stsighting, infant.DOB, unit = "days"))) %>%
  mutate (infant.entrytype = ifelse (infant.age1stsighting < 120, "Birth", "Observation")) %>%
  mutate (infant.DODfinal = ifelse (infant.isalive == 1, NA_character_,
                                    ifelse (is.na(infant.DOD), infant.DODfromcensus,
                                            ifelse (is.na(infant.DODfromcensus), infant.DOD,
                                                    ifelse (infant.DOD < infant.DODfromcensus, infant.DOD, infant.DODfromcensus))))) %>%
  mutate (infant.departtype = ifelse (is.na(infant.DODfinal), "Observation", "Death"))  %>%
  mutate (infant.ageatdepart = ifelse (!is.na(infant.DODfinal),
                                       difftime (infant.DODfinal, infant.DOB, unit = "days"),
                                       difftime (Sys.Date() , infant.DOB, unit = "days")),
          infant.Ndaysobserved = floor (infant.ageatdepart - infant.age1stsighting), # Needs to be done before rounding of ageatdepart
          infant.ageatdepart = round (infant.ageatdepart/364.25, 2)) %>%
  mutate (infant.surv1Y = ifelse (infant.ageatdepart >= 1, 1, 0),
          infant.surv2Y = ifelse (infant.ageatdepart >= 2, 1, 0),
          infant.surv5Y = ifelse (infant.ageatdepart >= 5, 1, 0)) #%>%
  #select (-contains ("comment"), -contains ("mother")) # ONLY remove this columns for better overview

# Control this
#infantsNAagefirstsighting  <- repro.7b %>%
#  filter (is.na(infant.age1stsighting)) %>%
#  select (infant.ID,  infant.name,	infant.DOB,	infant.1stsighting, infant.ageclass1stsighting, infant.isalive, infant.lastsighting,
#          infant.age1stsighting,	infant.entrytype) %>%
#  View ()

#########################################
# DERIVE MOTHER-VARIABLES
  # age1stsighting, entrytype (Birth, Observation), DODfinal
  # departtype (Observation, Death), ageatdepart, Ndaysobserved
repro.7c  <- repro.7b %>%
  mutate (mother.age1stsighting = floor (difftime (mother.1stsighting, mother.DOB, unit = "days"))) %>%
  mutate (mother.entrytype = ifelse (mother.age1stsighting < 120, "Birth", "Observation")) %>%
  mutate (mother.DODfinal = ifelse (mother.isalive == 1, NA_character_,
                                    ifelse (is.na(mother.DOD), mother.DODfromcensus,
                                            ifelse (is.na(mother.DODfromcensus), mother.DOD,
                                                    ifelse (mother.DOD < mother.DODfromcensus, mother.DOD, mother.DODfromcensus))))) %>%
  mutate (mother.departtype = ifelse (is.na(mother.DODfinal), "Observation", "Death"))  %>%
  mutate (mother.ageatdepart = ifelse (!is.na(mother.DODfinal),
                                       difftime (mother.DODfinal, mother.DOB, unit = "days"),
                                       difftime (Sys.Date() , mother.DOB, unit = "days")),
          mother.NDaysObserved = floor (mother.ageatdepart - mother.age1stsighting), # has to be calculated before rounding ateatdepart
          mother.ageatdepart = round (mother.ageatdepart/364.25, 2))

#########################################
# DERIVE MOTHER-INFANT-VARIABLES
  # MotherAgeAtBirth
  # InfantBirthOrder, PreviousBirth, DateOfFirstBirth (mother.DOFB), AgeAtFirstBirth (mother.ageatFB),
  # Nbirths per mother (mother.Nbirths)
  # AvgAgeOfDepart = mother.infantageatdepart.avg
  # NSurvivalGT1Yr and 2Yr (mother.Nsurv1Y, mother.Nsurv2Y)
  # NCensored (mother.NCensored; number of infants still alive)
  # Survivalscore is still missing.

repro.7d  <- repro.7c %>%
  mutate (mother.ageatbirth = round (difftime (infant.DOB, mother.DOB, units = "days")/364.25, 1)) %>%
  group_by (mother.ID) %>%
  arrange (mother.ID, infant.DOB) %>%
  mutate (infant.birthorder = as.integer(rank (infant.DOB)),
          infant.previousbirth = as.integer (infant.birthorder -1),
          mother.DOFB = first (infant.DOB),
          mother.ageatFB = round (difftime (mother.DOFB, mother.DOB, units = "days")/364.25, 1),
          mother.Nbirths = n(),
          mother.infantageatdepart.avg = mean (infant.ageatdepart),
          mother.Nsurv1Y = sum (infant.surv1Y),
          mother.Nsurv2Y = sum (infant.surv2Y),
          mother.Ncensored = sum (infant.departtype == "Observation")) # Would also work with 'sum (infant.isalive)'


mother.infant  <- repro.7d
rm (list = ls (pattern = "repro.|tblGroup|tblIndividual|code"))

