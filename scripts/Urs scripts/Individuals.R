IDs.1 <- pace_db %>%
  tbl ("tblIndividual") %>%
  select (IndividualID = ID, Name = NameOf, ProjectID, DOB = DateOfBirth, SexID) %>%
  # PrimateSpeciesID, CodeName, BirthdateSource, MotherID, MatrilineID, GroupAtBirthID,
  # DateOfFirstSighting, DayDifference, AgeClassAtFirstSightingID,
  # GroupAtFirstSightingID, VisionPhenotypeID, Comments, Comments_2,
  # CommentsJFA, CommentsGenetics, TimeStampAdd, UserAddEdit, TimeStampEdit  
  collect ()


IDs.2 <- pace_db %>%
  tbl ("codeSex") %>%
  select (SexID = ID, Sex, Description) %>%
  # no column excluded
  collect () %>%
  left_join (IDs.1, ., by = "SexID") %>%
  select (-SexID, -Sex)  %>%
  rename (IDSex = Description)

IDs <- IDs.2
rm (list = ls(pattern="IDs."))
# View (IDs)
