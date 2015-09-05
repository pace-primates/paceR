fruit_biomass_sr <- function(ph = NULL, tr = NULL, ...){

  # Get SR data only
  pheno <- ph %>% filter(Project == "SR")

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

}