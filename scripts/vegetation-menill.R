dplyr::db_list_tables(pace_db) %>% str_subset("Transect")

v1 <- get_pace_tbl(paceR_db, "vVegetationTransect_detailed")
# v2 <- get_pace_tbl(paceR_db, "vVegetationTransect2")
# v3 <- get_pace_tbl(paceR_db, "vVegetationTransect")

# get_pace_tbl(pace_db, "qryVegetationTransectDayTree")

v <- get_pace_tbl(paceR_db, "vVegetationTransect")

tr <- read_csv("~/Github/pace-data/csv/all_transects.csv")

v_tr <- tr %>%
  filter(str_detect(transect, "v_"))

v_tr <- v %>%
  semi_join(v_tr, by = c("TransectID" = "tr_id"))

v_tr <- v_tr %>%
  group_by(DateOf, TransectID, SpeciesName, TreeID, ProportionOfTreeInTransect) %>%
  mutate(abh = (pi * Dbh ^ 2) / 4) %>%
  summarise(n_stems = n(),
            abh_total = sum(abh),
            v_dbh = sqrt(4 * abh_total / pi)) %>%
  select(TransectID, everything()) %>%
  ungroup()

names(v_tr) <- c("tr_id", "date_of", "species", "tree_id", "prop_in_transect",
                 "n_stems", "abh_total", "virtual_dbh")

write_csv(v_tr, "~/Desktop/v_transect_trees.csv")

temp1 <- get_pace_tbl(pace_db, "tblVegetationTransectDayTree")
temp <- get_pace_tbl(pace_db, "tblVegetationTransectDayTreeDbh")

# Problems
filter(v1, is.na(SpeciesName) & Site.NameOf == "SRNP" & !is.na(Cbh) & !is.na(StemSeqNum))
