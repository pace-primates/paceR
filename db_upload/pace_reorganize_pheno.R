library(paceR)
library(RMySQL)

load_pace_packages()
# load(".RData")
source("pace_utils.R")

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", password = NULL, dbname = "monkey")

p <- get_pace_tbl(pace_db, "tblPhenologyMonthly")
pt <- get_pace_tbl(pace_db, "tblPhenologyTree") %>% select(-Comments)
site <- get_pace_tbl(pace_db, "tblSite")
foodpart <- get_pace_tbl(pace_db, "codeFoodPart")
score <- get_pace_tbl(pace_db, "tmp_codePhenologyScore")

ppt <- p %>%
  inner_join(pt, by = c("PhenologyTreeID" = "ID"))

p <- select(ppt, 1:14, Label)

# Create tblPhenologyRecord

# Get plant item types from old column names (strip "ID")
names(p)[6:12] <- str_replace(names(p)[6:12], "ID", "")
names(p)[12] <- "FlowerBudCover"

# Gather up all score columns
p1 <- gather(p, FoodPart, PhenologyScore, 6:12)

# Assign measurement type (cover/maturity)
p1$Measurement <- ifelse(str_detect(p1$FoodPart, "Cover"), "Cover", "Maturity")

# Refine food item names
p1$FoodPart <- str_replace(p1$FoodPart, "Cover", "")
p1$FoodPart <- str_replace(p1$FoodPart, "Maturity", "")

# Remove blanks, sort, remove old ID, add new columns
p2 <- p1 %>%
  filter(!is.na(PhenologyScore)) %>%
  arrange(PhenologyTreeID, Year, Month, FoodPart, Measurement) %>%
  select(-ID) %>%
  mutate(PhenologyPercent = NA,
         PhenologyCount = NA) %>%
  select(1, 7, 2:5, 8, 10, 9, 11, 12, Comments)

# Create tblPhenologySession
# With route
# p3 <- p2 %>%
#   mutate(RouteName = substr(Label, 0, 2)) %>%
#   distinct(Year, Month, RouteName) %>%
#   mutate(ProjectID = 1,
#          Comments = "",
#          PhenologyDate = ymd(paste(Year, Month, "01", sep = "-"))) %>%
#   select(ProjectID, RouteName, PhenologyDate, Year, Month, Comments) %>%
#   arrange(ProjectID, RouteName, PhenologyDate) %>%
#   mutate(ID = 1:nrow(.))

# Without route
 p3 <- p2 %>%
  distinct(Year, Month) %>%
  mutate(ProjectID = 1,
         Comments = "",
         PhenologyDate = ymd(paste(Year, Month, "01", sep = "-"))) %>%
  select(ProjectID, PhenologyDate, Year, Month, Comments) %>%
  arrange(ProjectID, PhenologyDate) %>%
  mutate(ID = 1:nrow(.))

# Set ID for tblPhenologyRecord
p2 <- inner_join(p2, select(p3, ID, Year, Month), by = c("Year", "Month"))


# Make table
tblPhenologyRecord <- p2 %>%
  rename(PhenologySessionID = ID) %>%
  arrange(PhenologySessionID, PhenologyTreeID, FoodPart, Measurement) %>%
  mutate(ID = 1:nrow(.))

tblPhenologySession <- select(p3, ID, ProjectID, everything(), -Year, -Month)

# Change names of food parts to correspond with codeFoodPart table
tblPhenologyRecord$FoodPart <- mapvalues(tblPhenologyRecord$FoodPart,
                                         from = c("FlowerBud", "Flowers", "Fruit",
                                                  "Leaves"),
                                         to = c("Flower Bud", "Flower", "Fruit",
                                                "Leaf"))

tblPhenologyRecord <- left_join(tblPhenologyRecord,
                                 select(foodpart, FoodPartID = ID, FoodPart = Part),
                                 by = "FoodPart")

tblPhenologyRecord$PhenologyScore <- as.character(tblPhenologyRecord$PhenologyScore)
tblPhenologyRecord <- left_join(tblPhenologyRecord, select(score, PhenologyScoreID = ID,
                                                           Code, Measurement),
                                by = c("PhenologyScore" = "Code", "Measurement"))

tblPhenologyRecord <- tblPhenologyRecord %>%
  select(ID, PhenologySessionID, PhenologyTreeID, DateOf, ResearcherID,
         FoodPartID, PhenologyScoreID, PhenologyPercent, PhenologyCount,
         Comments)

# Write tables
con <- dbConnect(RMySQL::MySQL(), group = "PACE", host = "127.0.0.1", port = 3307,
                 user = "camposf", password = NULL, dbname = "monkey")

dbWriteTable(con, "tblPhenologySession", data.frame(tblPhenologySession),
             row.names = FALSE, overwrite = TRUE)

dbWriteTable(con, "tblPhenologyRecord", data.frame(tblPhenologyRecord),
             row.names = FALSE, overwrite = TRUE)
