fpv$code_name <- mapvalues(fpv$code_name,
                           from = c("ZGUE", "GUMI"),
                           to = c("ZGUI", "GULM"))

th <- fpv %>%
  group_by(code_name) %>%
  select(1:2, dbh) %>%
  summarise(q = quantile(dbh, probs = 0.1),
            m = min(dbh),
            n = n())

ta <- get_pace_tbl(pace_db, "tblTaxon") %>% filter(ProjectID == 1)

th <- left_join(th, select(ta, CodeName, SpeciesName),
                  by = c("code_name" = "CodeName"))

write.csv(th, "~/Desktop/thresh.csv", row.names = FALSE)
