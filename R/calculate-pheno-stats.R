# ph <- getv_Phenology(paceR_db)

fruit_biomass_sr <- function(ph = NULL, tr = NULL, ...){

  # Get SR data only
  pheno <- pheno %>% filter(Project == "SR")

  # Retain only records related to fruit
  pheno <- pheno %>% filter(FoodPart == "Fruit")

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
  ph_wide[which(ph_wide$Fruit_Maturity == 5), ]$Fruit_Maturity <- 0

  ph_wide <- ph_wide %>%
    mutate_each(funs(as.numeric), Fruit_Cover, Fruit_Maturity) %>%
    mutate(index_avail = (Fruit_Cover / 4) * (Fruit_Maturity / 4)) %>%
    filter(!is.na(index_avail))

  pheno <- ph_wide %>% select(-Fruit_Cover, -Fruit_Maturity)

  # Remove any species for which there aren't 12 months of data
  remove_species <- pheno %>%
    group_by(SpeciesCode) %>%
    distinct(month_of) %>%
    summarise(n = n()) %>%
    filter(n < 12)

  pheno <- pheno %>%
    filter(!(SpeciesCode %in% remove_species$SpeciesCode))

  # Generate list of unique species, for later use
  species <- unique(select(pheno, SpeciesName, SpeciesCode))

  # return(ph_wide)
}


# ggplot(pheno, aes(x = month_of, y = index_avail)) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.25)) +
#   facet_wrap(~SpeciesCode, ncol = 5)

# Create data frame of model results for all species
mods <- pheno %>%
  group_by(SpeciesName) %>%
  do(m = gamm(index_avail ~
            s(as.numeric(month_of),
              bs = "cc"),
          data = .,
          random = list(TreeID = ~1),
          knots = list(month_of = c(1, 13))))


fruit_avail <- matrix(nrow = nrow(mods), ncol = 12)
for (i in 1:nrow(mods)) {
  for (j in 1:12) {

    pred <- predict(mods[i, ]$m[[1]]$gam,
                    newdata = subset(pheno, SpeciesName == mods[i, ]$SpeciesName))

    pred[pred < 0] <- 0

    temp <- data.frame(cbind(subset(pheno,
                                    SpeciesName == mods[i, ]$SpeciesName)$month_of,
                             pred))
    fruit_avail[i, j] <- mean(subset(temp, V1 == j)$pred)
  }
}


fruit_avail <- data.frame(fruit_avail)
names(fruit_avail) <- rep(month.abb, 1)
fruit_avail$SpeciesName <- mods$SpeciesName

