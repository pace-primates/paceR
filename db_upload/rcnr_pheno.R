library(paceR)
library(RMySQL)

load_pace_packages()
source("pace_utils.R")

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", password = NULL, dbname = "monkey")


pheno_s <- tbl_df(read.csv("pheno/RCNR/RCNR_pheno.csv"))
pace_researcher <- get_pace_tbl(pace_db, "tblResearcher")
pace_taxon <- get_pace_tbl(pace_db, "tblTaxon") %>% filter(ProjectID == 3)
pace_tree <- get_pace_tbl(pace_db, "tblPhenologyTree")

pace_foodpart <- get_pace_tbl(pace_db, "codeFoodPart")
pace_score <- get_pace_tbl(pace_db, "codePhenologyScore") %>%
  filter(Measurement == "Cover")

# Discard useless columns
pheno_s <- pheno_s %>%
  mutate(DateOf = ymd(DateOf),
         NewName = paste("Set", Set, "_", TreeNumber, sep = "")) %>%
  select(DateOf, NewName, everything(),
         -Species, -OldTreeNumber, -ID, -Set, -TreeNumber, -FireDamage,
         -HurricaneDamage, -Comments)

pheno_s <- rename(pheno_s, Comments = NewComments)



# ============== tblPhenologyTree

tmp_tblPhenologyTree <- pheno_s %>%
  group_by(NewName) %>%
  select(NewName, TaxonID) %>%
  distinct(NewName, TaxonID)


# Test
tmp_tblPhenologyTree %>%
  ungroup() %>%
  group_by(NewName) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  inner_join(pheno_s) %>%
  ungroup() %>%
  group_by(NewName, TaxonID) %>%
  summarise(n_records = n()) %>%
  View()


# Build new tree table
max_tr <- max(get_pace_tbl(pace_db, "tblPhenologyTree")$ID)

tr <- tmp_tblPhenologyTree %>%
  rename(Label = NewName)

tr$SiteID <- 34
tr$TaxonCode <- tr$Label
tr$ID <- (1:nrow(tr) + max_tr)

tr <- tr %>% select(ID, SiteID, TaxonID, everything())


# ============= tblPhenologySession

max_ps <- max(get_pace_tbl(pace_db, "tblPhenologySession")$ID)

ps <- pheno_s %>%
  select(DateOf) %>%
  distinct(DateOf) %>%
  arrange(DateOf)

ps$ProjectID <- 3
ps$ID <- (1:nrow(ps) + max_ps)

ps <- ps %>% select(ID, ProjectID, PhenologyDate = DateOf)


# ============= tblPhenologyRecord

max_pr <- max(get_pace_tbl(pace_db, "tblPhenologyRecord")$ID)

pheno_s$ResearcherID <- 92

pr <- pheno_s %>%
  select(DateOf, Label = NewName, ResearcherID,
         contains("Leaf"), contains("Fruit"), contains("Flower"), Comments)

# Set PhenologyTreeID
pr <- pr %>%
  inner_join(select(tr, PhenologyTreeID = ID, Label)) %>%
  select(-Label)

# Set SessionID
pr <- pr %>%
  inner_join(select(ps, PhenologySessionID = ID, DateOf = PhenologyDate))

# Reshape
pr <- pr %>%
  gather(FoodPart, PhenologyScore, -DateOf, -contains("ID"), -Comments) %>%
  select(PhenologySessionID, PhenologyTreeID, DateOf, ResearcherID, FoodPart, PhenologyScore, Comments)

# Remove blanks
pr <- filter(pr, !is.na(PhenologyScore))

fp1 <- levels(factor(pr$FoodPart))
pr$FoodPart <- mapvalues(pr$FoodPart, from = fp1,
                         to = c("Leaf Bud", "Leaf New", "Leaf Mature", "Leaf Old",
                                "Fruit Unripe", "Fruit Ripe",
                                "Flower Bud", "Flower Mature"))

# Set FoodPartID
pr <- pr %>%
  inner_join(select(pace_foodpart, FoodPartID = ID, FoodPart = Part)) %>%
  select(-FoodPart)

# Set PhenologyScoreID
pr$PhenologyScore <- mapvalues(pr$PhenologyScore,
                               from = c("0", "25", "50", "75", "100"),
                         to = c("0", "1", "2", "3", "4"))

pr <- pr %>%
  mutate(PhenologyScore = as.character(PhenologyScore)) %>%
  inner_join(select(pace_score, PhenologyScoreID = ID, PhenologyScore = Code)) %>%
  select(-PhenologyScore)

# Set SeqNum
temp <- pr %>%
  arrange(PhenologySessionID, PhenologyTreeID, DateOf) %>%
  group_by(PhenologySessionID, PhenologyTreeID, DateOf) %>%
  do(make_seq(.))

temp <- temp %>%
  ungroup() %>%
  arrange(PhenologySessionID, PhenologyTreeID, DateOf) %>%
  mutate(ID = 1:nrow(.) + max_pr) %>%
  select(ID, PhenologySessionID, PhenologyTreeID, DateOf, SeqNum, everything())


# =============== Write tables

con <- dbConnect(RMySQL::MySQL(), group = "PACE", host = "127.0.0.1", port = 3307,
                 user = "camposf", password = NULL, dbname = "monkey")

# Append to tblPhenologySession
dbWriteTable(con, "tblPhenologySession", data.frame(ps),
             row.names = FALSE, append = TRUE)

# Append to tblPhenologyTree
dbWriteTable(con, "tblPhenologyTree", data.frame(tr),
             row.names = FALSE, append = TRUE)

# Append to tblPhenologyRecord
dbWriteTable(con, "tblPhenologyRecord", data.frame(temp),
             row.names = FALSE, append = TRUE)

