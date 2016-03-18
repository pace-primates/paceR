library(paceR)
library(RMySQL)

load_pace_packages()
source("pace_utils.R")

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", password = NULL, dbname = "monkey")


bfms_c <- tbl_df(read.csv("census/bfms_annual_census.csv"))

bfms_c$DateOfCount <- ymd(bfms_c$DateOfCount)

# tblCensusAnnual ---------------------------------------------------------

tblCensusAnnual <- get_pace_tbl(pace_db, "tblCensusAnnual")
nextID <- max(tblCensusAnnual$ID)

tmpCensusAnnual <- bfms_c %>%
  group_by(YearOfCensus) %>%
  summarise(ResearcherID = min(LeaderID),
            DateStart = min(DateOfCount),
            DateEnd = max(DateOfCount)) %>%
  mutate(ProjectID = 5,
         SeqNum = 1)

tmpCensusAnnual$ID <- (1:nrow(tmpCensusAnnual) + nextID)



# ---- CensusAnnualCount --------------------------------------------------

tblCensusAnnualCount <- get_pace_tbl(pace_db, "tblCensusAnnualCount")
nextID <- max(tblCensusAnnualCount$ID)

tmpCensusAnnualCount <- bfms_c %>%
  rename(DateOf = DateOfCount, NTotal = Total, Comments = Comment)

tmpCensusAnnualCount <- inner_join(tmpCensusAnnualCount,
                                   select(tmpCensusAnnual, YearOfCensus, CensusAnnualID = ID))

tmpCensusAnnualCount <- tmpCensusAnnualCount %>%
  group_by(CensusAnnualID, GroupID) %>%
  arrange(CensusAnnualID, GroupID, DateOf, TimeOfCount) %>%
  do(make_seq(.))

tmpCensusAnnualCount$ID <- (1:nrow(tmpCensusAnnualCount) + nextID)


# ---- CensusAnnualCountAgeSexN -------------------------------------------

tblCensusAnnualCountAgeSexN <- get_pace_tbl(pace_db, "tblCensusAnnualCountAgeSexN")
nextID <- max(tblCensusAnnualCountAgeSexN$ID)


asc <- get_pace_tbl(pace_db, "codeAgeSexClass")
ac <- get_pace_tbl(pace_db, "codeAgeClass")
s <- get_pace_tbl(pace_db, "codeSex")

asc <- asc %>%
  inner_join(select(ac, AgeClassID = ID, AgeClass)) %>%
  inner_join(select(s, SexID = ID, Sex)) %>%
  mutate(AgeSexClass = paste(AgeClass, Sex, sep = ""))

l <- intersect(AgeSexClass$AgeSexClass, names(bfms_c))

tmpCensusAnnualCountAgeSexN <- tmpCensusAnnualCount %>%
  rename(CensusAnnualCountID = ID) %>%
  gather(AgeSexClass, N, one_of(l), -CensusAnnualCountID) %>%
  select(CensusAnnualCountID, AgeSexClass, N) %>%
  inner_join(select(asc, AgeSexClassID = ID, AgeSexClass)) %>%
  select(-AgeSexClass) %>%
  filter(!is.na(N))

tmpCensusAnnualCountAgeSexN$ID <- (1:nrow(tmpCensusAnnualCountAgeSexN) + nextID)


# ---- cleanup ------------------------------------------------------------

l <- intersect(names(tblCensusAnnualCount), names(tmpCensusAnnualCount))
tmpCensusAnnualCount <- select(tmpCensusAnnualCount, one_of(l))



# ---- append_tables ------------------------------------------------------

con <- dbConnect(RMySQL::MySQL(), group = "PACE", host = "127.0.0.1", port = 3307,
                 user = "camposf", password = NULL, dbname = "monkey")

# Append to tblPhenologySession
dbWriteTable(con, "tblCensusAnnual", data.frame(tmpCensusAnnual),
             row.names = FALSE, append = TRUE)

# Append to tblPhenologyTree
dbWriteTable(con, "tblCensusAnnualCount", data.frame(tmpCensusAnnualCount),
             row.names = FALSE, append = TRUE)

# Append to tblPhenologyRecord
dbWriteTable(con, "tblCensusAnnualCountAgeSexN", data.frame(tmpCensusAnnualCountAgeSexN),
             row.names = FALSE, append = TRUE)
