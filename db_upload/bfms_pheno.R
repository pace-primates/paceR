library(readr)
library(paceR)
library(RMySQL)

load_pace_packages()
source("pace_utils.R")

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", password = NULL, dbname = "monkey")

foodpart <- get_pace_tbl(pace_db, "codeFoodPart")
score <- get_pace_tbl(pace_db, "codePhenologyScore")
r <- get_pace_tbl(pace_db, "tblResearcher")
p <- get_pace_tbl(pace_db, "tblPerson")

max_ps <- max(get_pace_tbl(pace_db, "tblPhenologySession")$ID)
max_pr <- max(get_pace_tbl(pace_db, "tblPhenologyRecord")$ID)
max_ta <- max(get_pace_tbl(pace_db, "tblTaxon")$ID)
max_tr <- max(get_pace_tbl(pace_db, "tblPhenologyTree")$ID)

res <- inner_join(r, p, by = c("PersonID" = "ID")) %>%
  filter(ProjectID == 5) %>%
  mutate(initials = paste(substr(NameFirst, 1, 1),
                          substr(NameLast, 1, 1),
                          sep = ""))

ta <- tbl_df(read.csv("pheno/bfms/tblTaxon.csv"))
tr <- tbl_df(read.csv("pheno/bfms/tblPhenologyTree.csv"))
ph <- tbl_df(read.csv("pheno/bfms/tblPhenologyMonthly.csv"))

ta_p <- get_pace_tbl(pace_db, "tblTaxon")
tr_p <- get_pace_tbl(pace_db, "tblPhenologyTree")

# Fix table taxon!!!!!!!!!
ta <- ta %>%
  rename(OldID = ID) %>%
  mutate(ProjectID = 5,
         ID = 1:nrow(.) + max_ta) %>%
  select(ID, everything())

# Set tblTree
tr <- tr %>%
  rename(OldTreeID = ID, OldTaxonID = TaxonID) %>%
  inner_join(select(ta, TaxonID = ID, OldID),
             by = c("OldTaxonID" = "OldID")) %>%
  mutate(ID = 1:nrow(.) + max_tr) %>%
  select(ID, SiteID, TaxonID, everything())


anti_join(ph, tr, by = c("PhenologyTreeID" = "OldTreeID"))
anti_join(tr, ta, by = c("OldTaxonID" = "OldID"))

# PhenologySession
# ID, ProjectID, PhenologyDate, Comments
bfms_ps <- ph %>%
  mutate(PhenologyDate = ymd(DateOf)) %>%
  distinct(PhenologyDate) %>%
  mutate(ProjectID = 5,
         ID = 1:nrow(.) + max_ps,
         Comments = "") %>%
  select(ID, ProjectID, PhenologyDate, Comments) %>%
  arrange(PhenologyDate)

# PhenologyRecord
# ID, PhenologySessionID, PhenologyTreeID, DateOf, ResearcherID, FoodPart,
# Measurement, PhenologyScore, PhenologyPercent, PhenologyCount, Comments
bfms_pr <- ph %>%
  mutate(DateOf = ymd(DateOf),
         ID = ID + max_pr) %>%
  inner_join(select(bfms_ps, PhenologySessionID = ID, PhenologyDate),
             by = c("DateOf" = "PhenologyDate")) %>%
  rename(Researcher = ResearcherID)

temp1 <- bfms_pr %>%
  select(-contains("Quantity"), -Year, -Month) %>%
  gather(FoodPart, PhenologyScore, contains("Ripe"), contains("Unripe")) %>%
  mutate(Measurement = "Cover", PhenologyCount = NA)

temp2 <- bfms_pr %>%
  select(-contains("Ripe"), -contains("Unripe"), -Year, -Month, -LeafQuantity) %>%
  gather(FoodPart, PhenologyCount, contains("Quantity")) %>%
  mutate(Measurement = "Count", PhenologyScore = NA)

temp3 <- bfms_pr %>%
  select(-contains("Ripe"), -contains("Unripe"), -contains("Quantity"),
         -Year, -Month, LeafQuantity) %>%
  gather(FoodPart, PhenologyScore, contains("LeafQuantity")) %>%
  mutate(Measurement = "Cover", PhenologyCount = NA)

temp <- rbind(temp1, temp2, temp3)
temp <- filter(temp, !(is.na(PhenologyScore) & is.na(PhenologyCount)))

temp$FoodPart <- mapvalues(temp$FoodPart,
                           from = c("FlowerUnripe", "FlowerRipe", "FruitUnripe",
                                    "FruitRipe", "SeedUnripe", "SeedRipe",
                                    "LeafUnripe", "LeafRipe", "FlowerQuantity",
                                    "FruitQuantity", "SeedQuantity", "LeafQuantity"),
                           to = c("Flower Bud", "Flower Mature", "Fruit Unripe",
                                  "Fruit Ripe", "Seed Unripe", "Seed Ripe",
                                  "Leaf New", "Leaf Mature", "Flower", "Fruit",
                                  "Seed", "Leaf"))


# Get new foreign keys

# codeFoodPart
temp <- left_join(temp, select(foodpart, FoodPartID = ID, Part),
                   by = c("FoodPart" = "Part"))

# codePhenologyScore
temp$PhenologyScore <- as.character(temp$PhenologyScore)
temp <- left_join(temp, select(score, PhenologyScoreID = ID,
                               Code, Measurement),
                  by = c("PhenologyScore" = "Code", "Measurement"))

# tblResearcher
temp <- left_join(temp, select(res, ResearcherID = ID, initials),
                  by = c("Researcher" = "initials"))

# tblPhenologyTree
temp <- temp %>%
  rename(OldTreeID = PhenologyTreeID) %>%
  left_join(select(tr, PhenologyTreeID = ID, OldTreeID),
            by = "OldTreeID")

bfms_pr <- temp %>%
  select(PhenologySessionID, PhenologyTreeID, DateOf,
         ResearcherID, FoodPartID, PhenologyScoreID,
         PhenologyCount, Comments)






# Write tables
con <- dbConnect(RMySQL::MySQL(), group = "PACE", host = "127.0.0.1", port = 3307,
                 user = "camposf", password = NULL, dbname = "monkey")

# Append to tblPhenologySession
dbWriteTable(con, "tblPhenologySession", data.frame(bfms_ps),
             row.names = FALSE, append = TRUE)

# Append to tblTaxon
ta <- select(ta, -OldID)
dbWriteTable(con, "tblTaxon", data.frame(ta),
             row.names = FALSE, append = TRUE)

# Append to tblPhenologyTree
tr <- select(tr, ID, SiteID, TaxonID, Label, TaxonCode, Comments)
tr$TaxonCode <- tr$Label
dbWriteTable(con, "tblPhenologyTree", data.frame(tr),
             row.names = FALSE, append = TRUE)

# Append to tblPhenologyRecord
# dbWriteTable(con, "tblPhenologyRecord", data.frame(bfms_pr),
#              row.names = FALSE, append = TRUE)



# Make Sequence Numbers
pr <- get_pace_tbl(pace_db, "tblPhenologyRecord_copy")

temp <- pr %>%
  arrange(PhenologySessionID, PhenologyTreeID, DateOf) %>%
  group_by(PhenologySessionID, PhenologyTreeID, DateOf) %>%
  do(make_seq(.))

temp <- temp %>%
  ungroup() %>%
  arrange(PhenologySessionID, PhenologyTreeID, DateOf) %>%
  mutate(ID = 1:nrow(.)) %>%
  select(ID, PhenologySessionID, PhenologyTreeID, DateOf, SeqNum, everything())


# Overwrite tblPhenologyRecord
dbWriteTable(con, "tblPhenologyRecord", data.frame(temp),
             row.names = FALSE, append = TRUE)
