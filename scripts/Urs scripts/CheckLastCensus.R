# source ("motherinfant.R")
# source ("censusmonthly.source.R")

last.census <- census.monthly %>%
  group_by (census.group.code) %>%
  summarise (last.group.census = last (census.date),
             first.group.census = first (census.date))

last.seen <- census.monthly %>%
  group_by (census.group.code, individual.name) %>%
  summarise (last.seen = last (census.date)) %>%
  ungroup %>%
  arrange (individual.name, desc (last.seen)) %>%
  group_by (individual.name) %>%
  filter (row_number () == 1)


mother.infant.2 <- mother.infant %>%
  filter (is.na(infant.DOD)|is.na(infant.DODfromcensus)) %>%
  ungroup () %>%
  left_join (., last.seen, by = c("infant.name" = "individual.name")) %>%
  left_join (., last.census, by = "census.group.code") %>%
  select (infant.name, infant.DOB, infant.birthgroup, last.group = census.group.code, infant.sex,
          infant.isalive, infant.lastsighting, infant.DOD, infant.DODfromcensus, last.seen, last.group.census,
          infant.causeD, infant.DODfinal, infant.departtype,
          infant.ageatdepart, infant.Dcomment) %>%
  mutate (infant.departtype.2 = ifelse (last.seen == last.group.census, "End of group-observation", infant.departtype))