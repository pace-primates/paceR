getAgeSexClass_w <- function(pace_db){

  AgeSexClass <- pace_db %>%
    tbl("codeAgeSexClass") %>%
    select(AgeSexClassID = ID, AgeClassID, SexID) %>%
    # excluded: TimeStampAdd, UserAddEdit, TimeStampEdit
    collect()

  AgeSexClass.2 <- pace_db %>%
    tbl("codeAgeClass") %>%
    select(AgeClassID = ID, AgeClass, Description) %>%
    # excluded: Code, Order, Comments, TimeStampAdd, UserAddEdit, TimeStampEdit
    collect() %>%
    left_join(AgeSexClass, ., by = "AgeClassID") %>%
    select(-AgeClassID, -Description)

  AgeSexClass.3 <- pace_db %>%
    tbl("codeSex") %>%
    select(SexID = ID, Sex) %>%
    # excluded: Description, TimeStampAdd, UserAddEdit, TimeStampEdit
    collect() %>%
    left_join(AgeSexClass.2, ., by = "SexID") %>%
    mutate(Age_SexClass = paste(AgeClass, sep = "_", Sex)) %>%
    select(-SexID)

  return(AgeSexClass.3)

}

getAgeSexClass <- function(pace_db){

 asc <- suppressWarnings(getAgeSexClass_w(pace_db))

 return(asc)

}


getIndividual_w <- function(pace_db){

  IDs.1 <- pace_db %>%
    tbl("tblIndividual") %>%
    select(IndividualID = ID, NameOf, ProjectID, DOB = DateOfBirth, SexID) %>%
    # excluded: PrimateSpeciesID, CodeName, BirthdateSource, MotherID, MatrilineID,
    # GroupAtBirthID, DateOfFirstSighting, DayDifference, AgeClassAtFirstSightingID,
    # GroupAtFirstSightingID, VisionPhenotypeID, Comments, Comments_2,
    # CommentsJFA, CommentsGenetics, TimeStampAdd, UserAddEdit, TimeStampEdit
    collect()


  IDs.2 <- pace_db %>%
    tbl("codeSex") %>%
    select(SexID = ID, Sex) %>%
    collect() %>%
    left_join(IDs.1, ., by = "SexID") %>%
    select(-SexID)
}

getIndividual <- function(pace_db){

  ind <- suppressWarnings(getIndividual_w(pace_db))

  return(ind)
}