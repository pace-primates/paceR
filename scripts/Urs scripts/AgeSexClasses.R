# pace_db <- src_mysql(group = "PACE", user = "kalbitzeru", dbname = "monkey", password = NULL)

AgeSexClass  <- pace_db %>% 
  tbl("codeAgeSexClass") %>%
  select (AgeSexClassID = ID, AgeClassID,  SexID) %>%
  # excluded TimeStampAdd, UserAddEdit, TimeStampEdit
  collect ()

AgeSexClass.2  <- pace_db %>%
  tbl ("codeAgeClass") %>%
  select (AgeClassID = ID, AgeClass,  Description) %>%
  # Code, Order, Comments, TimeStampAdd, UserAddEdit, TimeStampEdit
  collect () %>%
  left_join (AgeSexClass, ., by = "AgeClassID") %>%
  select (-AgeClassID, -Description)

AgeSexClass.3  <- pace_db %>%
  tbl ("codeSex") %>%
  select (SexID = ID, Sex = Description) %>%
  # excluded: Sex, TimeStampAdd, UserAddEdit, TimeStampEdit
  collect () %>%
  left_join (AgeSexClass.2, ., by = "SexID") %>%
  mutate (AgeSexClass = paste (AgeClass, sep = "", Sex)) %>%
  select (-SexID)  %>% #, -AgeClass, -Sex)
  rename (Sex_ASC = Sex)

AgeSexClass  <- AgeSexClass.3
rm (list = ls(pattern = "AgeSexClass."))

# View (AgeSexClass)
