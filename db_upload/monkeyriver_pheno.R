library(paceR)
library(RMySQL)

load_pace_packages()
source("pace_utils.R")

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", password = NULL, dbname = "monkey")
howler_db <- src_mysql(group = "PACE", user = "camposf", password = NULL, dbname = "HowlerData")

# View tables
src_tbls(howler_db)

pheno_af <- get_pace_tbl(howler_db, "tblPhenology-AF")

code_ps <- get_pace_tbl(howler_db, "codePhenologyScore")
code_po <- get_pace_tbl(howler_db, "codePhenologyObject")

howler_researcher <- get_pace_tbl(howler_db, "tblResearcher")
howler_taxon <- get_pace_tbl(howler_db, "tblTaxon")
howler_tree <- get_pace_tbl(howler_db, "tblTree")

pace_researcher <- get_pace_tbl(pace_db, "tblResearcher")
pace_taxon <- get_pace_tbl(pace_db, "tblTaxon") %>% filter(ProjectID == 2)
pace_tree <- get_pace_tbl(pace_db, "tblPhenologyTree")

pace_foodpart <- get_pace_tbl(pace_db, "codeFoodPart")
pace_score <- get_pace_tbl(pace_db, "codePhenologyScore") %>%
  filter(Measurement == "Cover")

plants <- read.csv("pheno/LUPlants.csv")

# Discard useless columns
pheno_af <- pheno_af %>%
  mutate(DateOf = ymd(Date)) %>%
  select(DateOf, everything(), -SessionDayID, -SeqNum, -Date)


# Fix Missing TaxonID
temp <- pheno_af %>%
  left_join(select(howler_tree, TreeID = ID, TaxonID, monkey_TaxonID))

prob1 <- temp %>%
  filter(is.na(TaxonID))

fix1 <- prob1 %>%
  select(-TaxonID, -monkey_TaxonID) %>%
  left_join(select(howler_tree, TreeTagNew = TreeTag, TaxonID, monkey_TaxonID)) %>%
  filter(!is.na(monkey_TaxonID))

prob2 <- prob1 %>%
  select(-TaxonID, -monkey_TaxonID) %>%
  left_join(select(howler_tree, TreeTagNew = TreeTag, TaxonID, monkey_TaxonID)) %>%
  filter(is.na(monkey_TaxonID))

fix2 <- prob2 %>%
  select(-TaxonID, -monkey_TaxonID) %>%
  left_join(select(plants, SpeciesID = ID, monkey_TaxonID))


# Combine
s1 <- temp %>% filter(!is.na(monkey_TaxonID))
pheno_af <- bind_rows(s1, fix1, fix2)

# Test--should be blank
pheno_af %>% filter(is.na(monkey_TaxonID))



# ============== tblPhenologyTree

tmp_tblPhenologyTree <- pheno_af %>%
  group_by(TreeTagNew) %>%
  select(TreeTagNew, Utm, monkey_TaxonID) %>%
  distinct(monkey_TaxonID)


# Test
tmp_tblPhenologyTree %>%
  ungroup() %>%
  group_by(TreeTagNew) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  inner_join(pheno_af) %>%
  ungroup() %>%
  group_by(TreeTagNew, SpeciesID) %>%
  summarise(n_records = n()) %>%
  View()

# Build new tree table
max_tr <- max(get_pace_tbl(pace_db, "tblPhenologyTree")$ID)

tr <- tmp_tblPhenologyTree %>%
  rename(Label = TreeTagNew, TaxonID = monkey_TaxonID, GpsUtm = Utm)

tr$SiteID <- 33
tr$TaxonCode <- tr$Label
tr$ID <- (1:nrow(tr) + max_tr)

tr <- tr %>% select(ID, SiteID, TaxonID, everything())


# ============= tblPhenologySession

max_ps <- max(get_pace_tbl(pace_db, "tblPhenologySession")$ID)

ps <- pheno_af %>%
  select(DateOf) %>%
  distinct(DateOf) %>%
  arrange(DateOf)

ps$ProjectID <- 2
ps$ID <- (1:nrow(ps) + max_ps)

ps <- ps %>% select(ID, ProjectID, PhenologyDate = DateOf)


# ============= tblPhenologyRecord

max_pr <- max(get_pace_tbl(pace_db, "tblPhenologyRecord")$ID)

pr <- pheno_af %>%
  select(DateOf, Label = TreeTagNew, h_ResearcherID = ResearcherID,
         contains("Leaf"), contains("Fruit"), contains("Flower"), Comments)

# Set PhenologyTreeID
pr <- pr %>%
  inner_join(select(tr, PhenologyTreeID = ID, Label)) %>%
  select(-Label)

# Set researcher
howler_researcher <- howler_researcher %>%
  rename(h_ResearcherID = ID, ResearcherID = monkey_ResearcherID)

pr[pr$h_ResearcherID == 0, ]$h_ResearcherID <- 20

pr <- pr %>%
  inner_join(select(howler_researcher, h_ResearcherID, ResearcherID)) %>%
  select(-h_ResearcherID)

# Set SessionID
pr <- pr %>%
  inner_join(select(ps, PhenologySessionID = ID, DateOf))

# Reshape
pr <- pr %>%
  gather(FoodPart, PhenologyScore, -DateOf, -contains("ID"), -Comments) %>%
  select(PhenologySessionID, PhenologyTreeID, DateOf, ResearcherID, FoodPart, PhenologyScore, Comments)

# Remove blanks
pr <- filter(pr, !is.na(PhenologyScore))

# Check for errors
pr %>% filter(PhenologyScore > 4)

fp1 <- levels(factor(pr$FoodPart))
pr$FoodPart <- mapvalues(pr$FoodPart, from = fp1,
                         to = c("Leaf Bud", "Leaf New", "Leaf Mature", "Leaf Old",
                                "Fruit Unripe", "Fruit Ripe", "Fruit Overripe",
                                "Flower Bud", "Flower Mature", "Flower No Petals"))

# Set FoodPartID
pr <- pr %>%
  inner_join(select(pace_foodpart, FoodPartID = ID, FoodPart = Part)) %>%
  select(-FoodPart)

# Set PhenologyScoreID
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

