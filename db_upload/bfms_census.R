library(paceR)
library(RMySQL)

load_pace_packages()

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", password = NULL, dbname = "monkey")
paceR_db <- src_mysql(group = "PACE", user = "camposf", password = NULL, dbname = "paceR")

gi <- tbl_df(read.csv("census/BFMS_tblCensusMonthlyGroupIndivid.csv"))
gi$ID <- 1:nrow(gi)

gi$CensusDate <- ymd(gi$CensusDate)

temp <- gi %>%
  group_by(GroupID, CensusDate, IndividualID) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  ungroup() %>%
  left_join(gi)

# Duplicate records, age class changes
temp %>%
  group_by(GroupID, CensusDate, IndividualID) %>%
  distinct(AgeSexClassID) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  left_join(temp)

# Duplicate records, status changes
temp %>%
  group_by(GroupID, CensusDate, IndividualID) %>%
  distinct(StatusID) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  left_join(temp)

# Duplicate records, comments change
temp %>%
  group_by(GroupID, CensusDate, IndividualID) %>%
  distinct(Comments) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  left_join(temp)

tblCensusMonthlyGroupIndivid <- gi %>%
  group_by(GroupID, CensusDate, IndividualID) %>%
  distinct(GroupID, CensusDate, IndividualID, AgeSexClassID, StatusID, ToGroupID, Comments) %>%
  data.frame() %>%
  tbl_df() %>%
  rename(DateOf = CensusDate)

# Test to make sure no more duplicates
tblCensusMonthlyGroupIndivid %>%
  group_by(GroupID, DateOf, IndividualID) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# Make tblCensusMonthly
tblCensusMonthly <- tblCensusMonthlyGroupIndivid %>%
  select(GroupID, DateOf) %>%
  distinct(GroupID, DateOf) %>%
  mutate(CensusYear = year(DateOf),
         CensusMonth = month(DateOf))

tblCensusMonthly$ID <- c(1049:(1049 + nrow(tblCensusMonthly) - 1))

# Write tables
con <- dbConnect(RMySQL::MySQL(), group = "PACE", host = "127.0.0.1", port = 3307,
                 user = "camposf", password = NULL, dbname = "monkey")

dbWriteTable(con, "tblCensusMonthly", data.frame(tblCensusMonthly),
             row.names = FALSE, append = TRUE)

# Get new CensusMonthID
cm <- get_pace_tbl(pace_db, "tblCensusMonthly")

cm$DateOf <- ymd(cm$DateOf)

tblCensusMonthlyGroupIndivid <- tblCensusMonthlyGroupIndivid %>%
  inner_join(select(cm, CensusMonthlyID = ID, GroupID, DateOf), by = c("GroupID", "DateOf")) %>%
  select(-ID, -GroupID, -DateOf)

dbWriteTable(con, "tblCensusMonthlyGroupIndivid", data.frame(tblCensusMonthlyGroupIndivid),
             row.names = FALSE, append = TRUE)
