
# Run Script AgeSexClasses.R
# source ("AgeSexClasses.R")
# Run Script Individuals.R
# source ("Individuals.R")

groups <- pace_db %>%
  tbl ("tblGroup") %>%
  select (census.group.ID = ID, project.ID = ProjectID, species.ID = PrimateSpeciesID,
          census.group.name = NameLong,
          census.group.code = NameCode) %>%
  # sorted out: NameShort, NameOld, ParentGroupID, DateOfFormation (NA),
  # CensusNumberCode?, CensusLetterCode?
  # DateOfExtinction (NA), Alias, GpdID, GpdUtm, Description, Comments
  # TimeStampAdd, UserAddEdit, TimeStampEdit
  collect ()

#Parameters
Project  <- 1
#Groups  <- c(3, 6)

#########PROJECT
census.1  <- pace_db %>%
  tbl ("tblProject") %>%
  select (project.ID = ID, project.name = NameOf) %>%
  #sorted out: Code, LeaderID, Comments, TimeStampAdd, UserAddEdit, TimeStampEdit
  collect () %>%
  filter (project.ID %in% Project)

#########GROUP
#census.2  <- groups %>%
 # filter (census.group.ID %in% Groups) # Remove this filter?

census.2b   <-  groups %>%
  left_join (census.1, ., by = "project.ID") %>%
  select (-species.ID, -project.ID)

census.2  <- census.2b
rm (census.2b)

#######CENSUSMONTHLY
census.3 <- pace_db %>%
  tbl ("tblCensusMonthly") %>%
  select (census.ID = ID, census.group.ID = GroupID, census.date = DateOf,
          census.comment = Comments) %>%
  # sorted out: CensusYear (redundant, no discrepancies with DateOF),
  # CensusMonth (redundant, some discrepancies with DateOf)
  # "TimeStampAdd", "UserAddEdit", "TimeStampEdit"
  collect ()

census.3b  <- census.3 %>%
  left_join (census.2, ., by = "census.group.ID")

census.3  <- census.3b
rm (census.3b)

#########CENSUSMONTHLYGROUPINDIVID
usedCensusMonthly  <- (census.3 %>% distinct (census.ID))$census.ID
census.4  <- pace_db %>%
  tbl ("tblCensusMonthlyGroupIndivid") %>%
  filter (CensusMonthlyID %in% usedCensusMonthly) %>%
  select (census.ind.ID = ID, census.ID = CensusMonthlyID, individual.ID = IndividualID,
          census.ind.agesexclassID = AgeSexClassID,
          census.ind.statusID = StatusID, census.ind.togroupID = ToGroupID,
          census.ind.fromgroupID = FromGroupID, census.ind.comment = Comments) %>%
  # sorted out: TimeStampAdd, UserAddEdit, TimeStampEdit, UpdatedByID, DateTimeUpdated
  collect ()

census.4b  <- census.4 %>%
  left_join (., AgeSexClass, by = c("census.ind.agesexclassID" = "AgeSexClassID")) %>%
  rename (census.ind.agesexclass = AgeSexClass, census.ind.ageclass = AgeClass, census.ind.sex = Sex_ASC) %>%
  select (-census.ind.agesexclassID) %>%
  left_join (., IDs, by = c("individual.ID" = "IndividualID")) %>%
  select (-ProjectID) %>%
  rename (individual.name = Name, individual.DOB = DOB, individual.sex = IDSex)

#Some sex classes observed differ from sex classes from tblIndividual
#individual.sex from tblIndividual will be used
#But census.agesexclass not yet sorted out
# Left also census.ind.sex in for unknown individuals, especially young ones not seen later on
# But it seems that there are no adult individuals with unknown sex -> check again.

census.4c <- groups %>%
  select (census.group.ID, -census.group.name,  census.ind.togroup.code = census.group.code) %>%
  left_join (census.4b, ., by = c("census.ind.togroupID" = "census.group.ID"))

census.4d <- groups %>%
  select (census.group.ID, -census.group.name,  census.ind.fromgroup.code = census.group.code) %>%
  left_join (census.4c, ., by = c("census.ind.fromgroupID" = "census.group.ID"))

census.4  <- census.4d
rm (list = ls(pattern = "census.4\\w|usedCensusMonthly"))

####CODECENSUSMONTHLYSTATUS
census.5  <- pace_db %>%
  tbl("codeCensusMonthlyStatus") %>%
  select (StatusID = ID, census.ind.status = Status) %>%
  # StatusCode
  collect ()

census.5b  <- census.5 %>%
  left_join (census.4, ., by = c("census.ind.statusID" = "StatusID")) %>%
  select (-census.ind.statusID) %>%
  select (contains ("census"), contains ("ind"))

census.5  <- census.5b
rm (census.5b)

#########COMBINE CensusMonthly and CensusMonthlyGroupInd
census.6  <- census.5 %>%
  left_join(census.3, ., by = "census.ID") %>%
  select (-project.name, -census.group.ID)

######### CENSUSMONTHLYRESEARCHER
census.7 <- pace_db %>%
  tbl ("tblCensusMonthlyResearcher") %>%
  select (census.ID = CensusMonthlyID, census.researcher.ID = ResearcherID) %>%
  # ID, Comments (only three), TimeStampAdd, UserAddEdit, TimeStampEdit
  collect () %>%
  left_join (census.6, ., by = "census.ID")

census.7b  <- pace_db %>%
  tbl ("tblResearcher") %>%
  select (researcher.ID = ID, project.ID = ProjectID,
          person.ID = PersonID, researcher.comment = Comments) %>%
  #TimeStampAdd, UserAddEdit, TimeStampEdit
  collect ()

census.7c  <- pace_db %>%
  tbl ("tblPerson") %>%
  select (person.ID = ID, NameLast, NameFirst) %>%
  #EMail, TimeStampAdd, UserAddEdit, TimeStampEdit, Comments (only 1)
  collect () %>%
  mutate (person.name = paste (NameFirst, NameLast)) %>%
  select (-NameLast, -NameFirst)

census.7d  <- census.7b %>%
  left_join (., census.7c, by = "person.ID") %>%
  select (-person.ID) %>%
  select (-researcher.comment)

census.7e  <- census.7d %>%
  left_join (census.7, ., by = c("census.researcher.ID" = "researcher.ID")) %>%
  select (-census.researcher.ID, -project.ID) %>%
  rename (census.researcher.name = person.name)

census.monthly  <- census.7e %>%
  select (census.ID, census.date,
        census.researcher.name,
        census.group.name, census.group.code,
        census.ind.ID,
        individual.ID, individual.name, individual.DOB, individual.sex,
        census.ind.sex, -census.ind.agesexclass, census.ind.ageclass, census.ind.status,
        census.ind.fromgroup.code, census.ind.togroup.code, census.comment, census.ind.comment)

rm (list = ls(pattern="census.\\d"))
rm (list = c("groups", "Project", "AgeSexClass", "IDs"))