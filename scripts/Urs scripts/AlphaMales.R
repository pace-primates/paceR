

# source ("Individuals.R")
# source ("motherinfant.R")


##### IMPORT KATHY'S TABLE
amr.kathy <- read.table ("amr.kathy.txt", sep = "\t", header = TRUE)
amr.kathy <- amr.kathy %>%
  mutate (risk.k = ifelse (risk.k != "ARM", as.character (risk.k), "AMR"))

#####
tblGroup  <- pace_db %>%
  tbl ("tblGroup") %>%
  select (GroupID = ID, GroupCode = NameCode) %>%
  collect ()

####

alpha.1 <- mother.infant %>%
  mutate (infant.DOC = as.Date (infant.DOB) - 160) %>%
  mutate (status = ifelse (infant.ageatdepart > 1, 1, 0)) %>%
  select (status, infant.ID, infant.name, infant.DOB, infant.DOC, mother.ID, mother.name,
        infant.sex, infant.birthgroup, infant.lastsighting, infant.DOD, infant.DODfromcensus, infant.causeD,
        infant.isalive, infant.ageatdepart) %>%
  ungroup ()
# View (alpha.1)

alpha.2  <- pace_db %>%
  tbl ("tblAlphaMaleTenure") %>%
  select (AMRID = ID, GroupID, AlphaMaleID, DateStart, DateEnd, Comments) %>%
  # TimeStampAdd, UserAddEdit, TimeStampEdit
  collect () %>%
  left_join (., tblGroup, by = "GroupID")




risky.birthday <- alpha.2 %>%
  mutate (AMR = ifelse (is.na (DateEnd), format (Sys.time (), "%Y-%m-%d"), DateEnd)) %>%
  select (AMRID, group = GroupCode, AMR) %>%
  mutate (start = as.Date(AMR) - 364.25, end = as.Date (AMR) + 165) %>%
  group_by (AMRID, AMR, group) %>%
  do (data.frame (risky.birthday = seq (as.Date (.$start), as.Date (.$end), by = 'day'))) %>%
  mutate (risky.birth = "Yes",
          risky.birthday = as.character (risky.birthday)) %>%
  ungroup

alpha.3 <- alpha.1 %>%
  arrange (infant.birthgroup, infant.DOB) %>%
  left_join (., risky.birthday, by = c("infant.birthgroup" = "group", "infant.DOB" = "risky.birthday")) %>%
  mutate (risky.time = ifelse (infant.lastsighting > AMR, "AMR", "GS")) %>%
  mutate (risky.time = ifelse (is.na (risky.time), "GS", risky.time)) %>%
  group_by (infant.name) %>%
  filter (row_number () == 1) %>%
  ungroup


comparison <- amr.kathy %>%
  left_join (alpha.3, ., by = c("infant.ID" = "infant.ID.k")) %>%
  mutate (risky.time = as.factor (risky.time), risk.k = as.factor (risk.k),
          takeover.date.k = as.Date (takeover.date.k, format = "%y-%m-%d"),
          AMR = as.Date (AMR))

comparison <- comparison %>%
  select (infant.survived1stYr = status, infant.ID, infant.name, infant.DOB, infant.DateOfConception = infant.DOC,
          mother.ID, mother.name, infant.sex, infant.birthgroup, infant.lastsighting, infant.DOD, infant.DODfromcensus, infant.causeD,
          infant.isalive, infant.ageatdepart, AlphaMaleTenureID = AMRID, takeover = AMR, risky.birth, group.stability = risky.time, group.stability.k = risk.k,
          alpha.at.conception.k, sire.k, infant.name.k, infant.DOB.k, infant.DateOfConception.k = infant.DOC.k, infant.mother.k,
          infant.sex.k, infant.birthgroup.k, infant.DOD.k, infant.CauseOfDeath.k, infant.alive.k, infant.AgeAtDeath.k, takeover.date.k,
          infant.ageattakeover.k, infant.over1yr.k) %>%
  mutate_each (funs (as.Date), infant.DOB, infant.DateOfConception, infant.lastsighting, infant.DOD,
               infant.DODfromcensus, takeover, infant.DOB.k, infant.DateOfConception.k, infant.DOD.k, takeover.date.k)

different.stability <- comparison %>% filter (group.stability != group.stability.k)

different.stability %>%
  select (infant.name, infant.birthgroup, risky.birth, group.stability, group.stability.k, infant.DOB, infant.DOB.k,
          infant.DOD, infant.DODfromcensus, infant.causeD, infant.DOD.k, takeover, takeover.date.k) %>%
  mutate (wrong = difftime (takeover, takeover.date.k, units = "days"),
          dobdiff = difftime (infant.DOB, infant.DOB.k, units = "days")) %>%
  View ()


alpha.3 <- alpha.3 %>%
  mutate(AMR = ymd(AMR),
         infant.DOD = ymd(infant.DOD),
         probable_infanticide = (risky.birth == "Yes" & infant.ageatdepart < 1 & infant.DOD > AMR) | infant.causeD == "Infanticide")
         # probable_infanticide = infant.causeD == "Infanticide")
