library(paceR)
library(RMySQL)

load_pace_packages()

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", password = NULL, dbname = "monkey")

v <- tbl_df(read.csv("pheno/Vatovavy.csv"))
v <- rename(v, DateOf = Date, FoodItem = Tag)
v$Site <- "Vatovavy"

v$FoodItem <- mapvalues(v$FoodItem,
                        from = c("flowers", "fruit", "Fruit", "Ripe Fruit", "Unripe Fruit"),
                        to = c("Flowers", "AllFruit", "AllFruit", "RipeFruit", "UnripeFruit"))

v <- gather(v, TreeID, Score, -Site, -DateOf, -FoodItem)

v$DateOf <- ymd(v$DateOf)
v$TreeID <- str_replace(v$TreeID, fixed("X"), fixed(""))

v <- spread(v, FoodItem, Score)



s <- tbl_df(read.csv("pheno/Sangasanga.csv"))
s <- rename(s, DateOf = Date, FoodItem = Tag)
s$Site <- "Sangasanga"

s$FoodItem <- mapvalues(s$FoodItem,
                        from = c("Ripe Fruit", "Ripe fruit", "Unripe fruit", "Unripe Fruit"),
                        to = c("RipeFruit", "RipeFruit", "UnripeFruit", "UnripeFruit"))

s <- gather(s, TreeID, Score, -Site, -DateOf, -FoodItem)

s$DateOf <- ymd(s$DateOf)
s$TreeID <- str_replace(s$TreeID, fixed("X"), fixed(""))

s <- spread(s, FoodItem, Score)


m_pheno <- bind_rows(s, v)

m_pheno <- m_pheno %>%
  arrange(DateOf, Site, TreeID)

# Fix cases where ripe fruit recorded but unripe fruit is NA
# These are actually all fruit (not just ripe)
m_pheno[!is.na(m_pheno$RipeFruit) & is.na(m_pheno$UnripeFruit), ]$AllFruit <- m_pheno[!is.na(m_pheno$RipeFruit) & is.na(m_pheno$UnripeFruit), ]$RipeFruit
m_pheno[!is.na(m_pheno$RipeFruit) & is.na(m_pheno$UnripeFruit), ]$RipeFruit <- NA
# write.csv(m_pheno, "~/Desktop/MadagascarPhenology.csv", row.names = FALSE)

fruit <- select(m_pheno, -Flowers, -AllFruit)

fruit$FruitTotal <- rowSums(fruit[, 4:5], na.rm = TRUE)


m_pheno_long <- gather(m_pheno, FoodItem, PhenologyScore, -DateOf, -Site, -TreeID)
# m_pheno_long$Measurement <- "Coverage"

m_pheno_long <- select(m_pheno_long,
                       OldPhenologyTreeID = TreeID, Site,
                       DateOf, FoodItem,
                       PhenologyScore)

max_ps <- max(get_pace_tbl(pace_db, "tblPhenologySession")$ID)

m_pheno_session <- m_pheno_long %>%
  group_by(DateOf) %>%
  summarise(ProjectID = 4) %>%
  ungroup() %>%
  arrange(DateOf) %>%
  mutate(ID = (max_ps + 1):(max_ps + nrow(.))) %>%
  select(ID, ProjectID, PhenologyDate = DateOf)


m_pheno_record <- m_pheno_long %>%
  inner_join(select(m_pheno_session, ID, PhenologyDate),
             by = c("DateOf" = "PhenologyDate")) %>%
  rename(PhenologySessionID = ID) %>%
  arrange(PhenologySessionID, DateOf, OldPhenologyTreeID, FoodItem) %>%
  select(PhenologySessionID, everything(), -Site)

m_pheno_record[m_pheno_record$OldPhenologyTreeID %in% c(585, 586, 587) & m_pheno_record$DateOf == ymd("2013-07-09"), ]$PhenologyScore <- NA

# FoodPart
fp <- get_pace_tbl(pace_db, "codeFoodPart") %>%
  select(FoodPartID = ID, FoodPart = Part)

m_pheno_record$FoodItem <- mapvalues(m_pheno_record$FoodItem,
                        from = c("Flowers", "RipeFruit", "UnripeFruit", "AllFruit"),
                        to = c("Flower", "Fruit Ripe", "Fruit Unripe", "Fruit"))

m_pheno_record <- m_pheno_record %>%
  inner_join(fp, by = c("FoodItem" = "FoodPart")) %>%
  select(-FoodItem)

# PhenologyTree
pt <- get_pace_tbl(pace_db, "tblPhenologyTree") %>%
  filter(SiteID %in% c(31, 32)) %>%
  select(PhenologyTreeID = ID, OldPhenologyTreeID = Label) %>%
  mutate(OldPhenologyTreeID = as.character(OldPhenologyTreeID))

m_pheno_record <- m_pheno_record %>%
  inner_join(pt) %>%
  select(-OldPhenologyTreeID)

# Researcher
m_pheno_record$ResearcherID <- 189

# PhenologyScore
ps <- get_pace_tbl(pace_db, "codePhenologyScore") %>%
  filter(Measurement == "Cover") %>%
  select(PhenologyScoreID = ID, PhenologyScore = Code)

m_pheno_record <- m_pheno_record %>%
  mutate(PhenologyScore = as.character(PhenologyScore)) %>%
  left_join(ps) %>%
  select(-PhenologyScore)

# Remove NAs
m_pheno_record <- filter(m_pheno_record, !is.na(PhenologyScoreID))

max_pr <- max(get_pace_tbl(pace_db, "tblPhenologyRecord")$ID)

make_seq <- function(df){
  res <- df %>%
    mutate(SeqNum = 1:nrow(.))
  return(res)
}

m_pheno_record <- m_pheno_record %>%
  mutate(ID = (max_pr + 1):(max_pr + nrow(.))) %>%
  group_by(PhenologySessionID, PhenologyTreeID) %>%
  arrange(PhenologySessionID, PhenologyTreeID, FoodPartID) %>%
  do(make_seq(.))


# Arrange columns
m_pheno_record <- m_pheno_record %>%
  select(ID, PhenologySessionID, PhenologyTreeID, DateOf, SeqNum, ResearcherID,
         FoodPartID, PhenologyScoreID)


# Append to database
con <- dbConnect(RMySQL::MySQL(), group = "PACE", host = "127.0.0.1", port = 3307,
                 user = "camposf", password = NULL, dbname = "monkey")

dbWriteTable(con, "tblPhenologySession", data.frame(m_pheno_session),
             row.names = FALSE, append = TRUE)

dbWriteTable(con, "tblPhenologyRecord", data.frame(m_pheno_record),
             row.names = FALSE, append = TRUE)

# write.csv(m_pheno_record, "m_pheno_record.csv", row.names = FALSE)

