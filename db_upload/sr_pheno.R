library(xlsx)
library(paceR)
library(RMySQL)

load_pace_packages()

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", password = NULL, dbname = "monkey")
paceR_db <- src_mysql(group = "PACE", user = "camposf", password = NULL, dbname = "paceR")

#### If there are connection errors, open terminal and run this
# ps aux | grep ssh
# Look for connections to pacelab and kill them with:
# kill <id number>

`%ni%` = Negate(`%in%`)

p_cp <- tbl_df(read.xlsx("~/Desktop/CP Phenology 2015-12.xlsx", sheetIndex = 1))
p_lv <- tbl_df(read.xlsx("~/Desktop/LV Phenology 2015-12.xlsx", sheetIndex = 1))
p1 <- suppressWarnings(bind_rows(p_cp, p_lv))
p1 <- select(p1, -GPS)
# p1$DateOf <- parse_date_time(p1$DateOf, orders = "%m/%d/%Y")
p1$DateOf <- ymd(p1$DateOf)


# CHECK DATA ==================

# Make sure column labels are correct
n <- c("DateOf", "LastName", "Trail", "TagLabel", "NewMapNum", "Status",
       "LeafCover", "LeafMaturity", "FruitCover", "FruitMaturity", "FlowerCover",
       "FlowerMaturity", "Location", "CommentsPhenology")

if (!identical(names(p1), n)) {
  message("Error: column names don't match template.")
} else {
  message("Column labels fine!")
}

# Check for new trees
pt <- get_pace_tbl(paceR_db, "vPhenologyTree", collect = FALSE) %>%
  filter(ProjectID == 1) %>%
  collect()

# Should be zero!!
if (length(p1[which(p1$TagLabel %ni% pt$Label), ]$TagLabel) != 0) {
  message("Error: unknown tree labels. Fix the following records.")
  p1[which(p1$TagLabel %ni% pt$Label), ]
} else {
    message("Tree labels fine!")
}

# Check for new researchers or incorrectly entered names
r <- get_pace_tbl(pace_db, "lstResearcher", collect = FALSE) %>%
  filter(Project == "SR") %>%
  collect() %>%
  separate(Researcher, c("FirstName", "LastName"), sep = " ")

# Should be zero!!
if (length(p1[which(toupper(p1$LastName) %ni% toupper(r$LastName)), ]$LastName) != 0) {
  message("Error: researchers. Fix the following records.")
  p1[which(toupper(p1$LastName) %ni% toupper(r$LastName)), ]
} else {
  message("Researcher names fine!")
}

# Reshape from wide to long
p1 <- p1 %>%
  select(DateOf, LastName, Label = TagLabel, contains("Cover"), contains("Maturity"),
         CommentsPhenology) %>%
  gather(FoodPart, PhenologyScore, contains("Cover"), contains("Maturity"))

# Assign measurement type (cover/maturity)
p1$Measurement <- ifelse(str_detect(p1$FoodPart, "Cover"), "Cover", "Maturity")

# Refine food item names
p1$FoodPart <- str_replace(p1$FoodPart, "Cover", "")
p1$FoodPart <- str_replace(p1$FoodPart, "Maturity", "")

p1 <- p1 %>%
  mutate(PhenologyDate = round_date(DateOf, "month"),
         Year = year(PhenologyDate),
         Month = month(PhenologyDate))

# Create record for phenology session
max_ps <- max(get_pace_tbl(pace_db, "tblPhenologySession")$ID)

ps <- p1 %>%
  distinct(Year, Month) %>%
  mutate(ProjectID = 1,
         Comments = "",
         PhenologyDate = ymd(paste(Year, Month, "01", sep = "-"))) %>%
  arrange(ProjectID, PhenologyDate) %>%
  mutate(ID = (max_ps + 1):(max_ps + nrow(.))) %>%
  select(ID, ProjectID, PhenologyDate, Comments, Year, Month)

tmpPhenologySession <- select(ps, -Year, -Month)


# Set foreign keys (PhenologySessionID, PhenologyTreeID, ResearcherID, FoodPartID, PhenologyScoreID)

# PhenologySessionID
p1 <- inner_join(p1, select(ps, PhenologySessionID = ID, Year, Month),
                 by = c("Year", "Month"))

# PhenologyTreeID
p1 <- inner_join(p1, select(pt, PhenologyTreeID = TreeID, Label), by = "Label")

# ResearcherID
r <- r %>% mutate(match_name = toupper(LastName))

p1 <- p1 %>%
  mutate(match_name = toupper(LastName)) %>%
  rename(OldLastName = LastName) %>%
  inner_join(select(r, ResearcherID = ID, match_name, LastName)) %>%
  select(-match_name, -OldLastName)

# FoodPartID
fp <- get_pace_tbl(pace_db, "codeFoodPart") %>%
  select(FoodPartID = ID, FoodPart = Part)

p1 <- p1 %>%
  inner_join(fp)

# PhenologyScoreID
s <- get_pace_tbl(pace_db, "codePhenologyScore") %>%
  select(PhenologyScoreID = ID, PhenologyScore = Code, Measurement)

p1 <- p1 %>%
  mutate(PhenologyScore = as.character(PhenologyScore)) %>%
  inner_join(s)


# Make final tmpPhenologyRecord
pr <- get_pace_tbl(pace_db, "tblPhenologyRecord")
max_pr <- max(pr$ID)

make_seq <- function(df){
  res <- df %>%
    mutate(SeqNum = 1:nrow(.))
  return(res)
}

tmpPhenologyRecord <- p1 %>%
  group_by(PhenologySessionID, PhenologyTreeID) %>%
  arrange(PhenologySessionID, PhenologyTreeID, FoodPartID) %>%
  do(make_seq(.))

tmpPhenologyRecord$ID <- (max_pr + 1):(max_pr + nrow(tmpPhenologyRecord))

tmpPhenologyRecord <- tmpPhenologyRecord %>%
  select(ID, PhenologySessionID, PhenologyTreeID, DateOf, SeqNum, ResearcherID,
         FoodPartID, PhenologyScoreID, Comments = CommentsPhenology)

# Write tables
con <- dbConnect(RMySQL::MySQL(), group = "PACE", host = "127.0.0.1", port = 3307,
                 user = "camposf", password = NULL, dbname = "monkey")

dbWriteTable(con, "tblPhenologySession", data.frame(tmpPhenologySession),
             row.names = FALSE, append = TRUE)

dbWriteTable(con, "tblPhenologyRecord", data.frame(tmpPhenologyRecord),
             row.names = FALSE, append = TRUE)
