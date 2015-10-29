exclude_species <- c("AEDU", "AOCC", "BPLU", "BPIN", "BUNG", "CGUA", "CPAN",
                     "FUNK", "JPUN", "MARB", "MARG", "MCAL", "PGUA", "RMON",
                     "RTHU", "SPAV", "ACOL")

pheno <- pheno_prep_sr(ph, exclude_species, item = "Leaf", maturity = "Immature")

indices_lo <- pheno_avail_indices_sr(pheno, smooth = "loess")

indices_lo[which(indices_lo$avail > 1), ]$avail <- 1


ggplot(indices_lo, aes(x = month_of, y = as.numeric(as.character(year_of)), fill = avail)) +
  geom_tile(color = "gray50") +
  scale_fill_gradientn(colours = c("#FFFFFF", brewer.pal(9, "YlOrRd")),
                       trans = scales::sqrt_trans(),
                       limits = c(0, 1),
                       name = "Availability Index") +
  scale_y_continuous(limits = c(2000, 2016)) +
  facet_wrap(~SpeciesName, ncol = 7) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        panel.grid = element_blank(),
        # axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key.width = grid::unit(2.5, "cm")) +
  labs(x = "Month", y = "Year") +
  coord_polar()




# Vectors
indices_lo$SpeciesName <- mapvalues(indices_lo$SpeciesName,
                                    from = "Ficus morazaniana",
                                    to = "Ficus bullenei*")

temp <- indices_lo %>%
  group_by(SpeciesName) %>%
  mutate(date_of = parse_date_time(paste(year_of, month_of, "01", sep = "-"),
                                   orders = "%y-%b-%d"),
         y_date = decimal_date(date_of) - year(date_of),
         date_deg = y_date * 360) %>%
  do(direction = vector.averaging(.$date_deg, .$avail)$direction,
     distance = vector.averaging(.$date_deg, .$avail)$distance)

temp$direction <- unlist(temp$direction)
temp$distance <- unlist(temp$distance)

# Scale distances to years for overlay plotting
temp$new_dist <- (temp$distance / max(temp$distance)) * 16 + 2000

# Reorder by distance
species <- temp %>%
  arrange(-distance) %>%
  select(SpeciesName)

species <- species$SpeciesName

temp$SpeciesName <- factor(temp$SpeciesName, levels = species)
indices_lo$SpeciesName <- factor(indices_lo$SpeciesName, levels = species)

ggplot(temp, aes(x = direction, xend = direction, y = new_dist, yend = 2000)) +
  geom_segment() +
  coord_polar() +
  scale_x_continuous(limits = c(0, 360)) +
  scale_y_continuous(limits = c(2000, 2016)) +
  facet_wrap(~SpeciesName, ncol = 7) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank()) +
  labs(x = "Month", y = "Year")


ggplot(indices_lo, aes(x = month_of, y = as.numeric(as.character(year_of)), fill = avail)) +
  geom_tile(color = "gray50") +
  scale_fill_gradientn(colours = c("#FFFFFF", brewer.pal(9, "YlOrRd")),
                       trans = scales::sqrt_trans(),
                       limits = c(0, 1),
                       guide = FALSE) +
  scale_y_continuous(limits = c(2000, 2016)) +
  facet_wrap(~SpeciesName, ncol = 7) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.key.width = grid::unit(2.5, "cm")) +
  labs(x = "Month", y = "Year") +
  coord_polar()





pheno_prep_sr2 <- function(pheno = NULL, exclude_species = "", Item = "Fruit", ...){

  # Get SR data only
  pheno <- pheno %>% filter(Project == "SR")

  # Retain only records related to fruit
  pheno <- pheno %>% filter(FoodPart == Item)

  pheno$FoodPart <- "Item"

  # Discard irrelevant columns
  pheno <- pheno %>% select(-PhenologyPercent, -PhenologyCount, -ScientificName,
                            -RecordDate, -ResearcherName, -Comments, -SiteName)

  # New useful columns
  pheno <- pheno %>%
    mutate(year_of = year(PhenologyDate),
           month_of = month(PhenologyDate))

  # Exclude 2006 pheno data because no maturity info
  pheno <- pheno %>% filter(year_of > 2006)

  pheno$month_of <- factor(pheno$month_of, labels = month.abb[1:12])

  # First unite the "FoodPart" and "Measurement" columns
  ph_wide <- pheno %>% unite(FoodPartMeasurement, c(FoodPart, Measurement))

  # Now spread PhenologyScore using FoodPartMeasurement as the key
  ph_wide <- ph_wide %>% spread(FoodPartMeasurement, PhenologyScore)

  # Fix maturity code 5 (change to zero)
  ph_wide[which(ph_wide$Item_Maturity == 5), ]$Item_Maturity <- 0

  ph_wide <- ph_wide %>%
    mutate_each(funs(as.numeric), Item_Cover, Item_Maturity) %>%
    mutate(index_avail = (Item_Cover / 4) * ((4 - Item_Maturity) / 4)) %>%
    filter(!is.na(index_avail))

  pheno <- ph_wide %>% select(-Item_Cover, -Item_Maturity)

  # Remove any species for which there aren't 12 months of data
  remove_species <- pheno %>%
    group_by(SpeciesCode) %>%
    distinct(month_of) %>%
    summarise(n = n()) %>%
    filter(n < 12 | SpeciesCode %in% exclude_species)

  pheno <- pheno %>%
    filter(!(SpeciesCode %in% remove_species$SpeciesCode))

  return(pheno)

}