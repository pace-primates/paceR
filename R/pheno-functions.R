pheno_fruit_indices_sr <- function(pheno = NULL, type = "raw", ...){

  pheno$year_of <- factor(pheno$year_of)

  # Generate list of unique species, for later use
  species <- unique(select(pheno, SpeciesName, SpeciesCode))

  if (type == "raw") {

    res <- pheno %>%
      group_by(SpeciesName, year_of, month_of) %>%
      summarise(avail = mean(index_avail))

    res$year_of <- factor(res$year_of)

  }
  else if (type == "smoothed") {

    # Years with failed crops
    failed_years <- pheno %>%
      group_by(SpeciesName, year_of, month_of) %>%
      summarise(monthly_sum = sum(index_avail)) %>%
      ungroup() %>%
      group_by(SpeciesName, year_of) %>%
      summarise(yearly_sum = sum(monthly_sum),
                n_months = n()) %>%
      filter(yearly_sum == 0 & n_months == 12)

    # Months with no fruit (require at least 3 years)
    bare_months <- pheno %>%
      group_by(SpeciesName, year_of, month_of) %>%
      summarise(monthly_sum = sum(index_avail)) %>%
      ungroup() %>%
      group_by(SpeciesName, month_of) %>%
      summarise(monthly_sum = sum(monthly_sum),
                n_years = n()) %>%
      filter(monthly_sum == 0 & n_years >= 3)

    mods2 <- pheno %>%
      group_by(SpeciesName) %>%
      do(m = gamm(index_avail ~ s(as.numeric(month_of), bs = "cc", k = 13) +
                    year_of,
                  random = list(TreeID = ~1),
                  knots = list(month_of = c(1, 13)),
                  data = .))

    gam_pred <- list()
    for (i in 1:nrow(mods2)) {
      c_species <- mods2[i, ]$SpeciesName
      c_gam <- mods2[i, ]$m[[1]]$gam
      # c_gam <- mods2[i, ]$m[[1]]
      set <- filter(pheno, SpeciesName == c_species)

      gam_pred[[i]] <- set %>%
        mutate(avail = predict(c_gam, newdata = set))
    }

    gam_pred <- bind_rows(gam_pred)

    gam_pred <- gam_pred %>%
      group_by(SpeciesName, year_of, month_of) %>%
      summarise(avail = mean(avail))

    gam_pred[gam_pred$avail < 0, ]$avail <- 0

    gam_pred <- gam_pred %>%
      left_join(bare_months, by = c("SpeciesName", "month_of")) %>%
      left_join(failed_years, by = c("SpeciesName", "year_of")) %>%
      mutate(avail = ifelse(!is.na(yearly_sum), 0, avail))

    res <- gam_pred
  }

  res <- inner_join(res, species, by = "SpeciesName")

  return(res)

}

pheno_prep_fruit_sr <- function(ph = NULL, exclude_species = "", ...){

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

plot_pheno_indices <- function(df = NULL, fill_col = c("#FFFFFF", brewer.pal(9, "YlGnBu"))){

  p <- ggplot(df, aes(x = month_of, y = year_of, fill = avail)) +
  geom_tile(color = "gray50") +
  scale_fill_gradientn(colours = fill_col,
                       trans = sqrt_trans(),
                       limits = c(0, 1),
                       name = "Availability Index") +
  facet_wrap(~SpeciesName, nrow = 5) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key.width = unit(2.5, "cm")) +
  labs(title = "Raw Fruit Availability Indices")

  return(p)

}

transect_subset_sr <- function(tr = NULL, pheno = NULL, min_dbh = NULL) {

  # Generate list of unique species, for later use
  species <- unique(select(pheno, SpeciesName, SpeciesCode))

  # Get transect trees from target pheno species
  tr_pheno <- tr %>%
    filter(CodeName %in% species$SpeciesCode) %>%
    mutate(DateOf = ymd(DateOf)) %>%
    arrange(TransectID, DateOf, TreeID, StemSeqNum)

  # Fill in fixed DBH for bromeliads, since it is not recorded
  # Using 5 cm per fruiting plant
  # Also ensure that each has a positive n_stems
  tr_pheno %>%
    mutate(Dbh = ifelse(CodeName %in% c("BPLU", "BPIN"), 5, Dbh))

  # Group by TreeID and calculate virtual DBH
  tr_pheno <- tr_pheno %>%
    group_by(DateOf, TransectID, SpeciesName, TreeID, ProportionOfTreeInTransect,
             CodeName) %>%
    mutate(abh = (pi * Dbh ^ 2) / 4,
           n_stems = 1) %>%
    summarise(n_stems = n(),
              abh_total = sum(abh),
              dbh = sqrt(4 * abh_total / pi)) %>%
    ungroup() %>%
    filter(!is.na(dbh))

  # Set "usable" flag to indicate if tree dbh >= threshold
  tr_pheno$usable <- FALSE
  for (i in 1:nrow(tr_pheno)) {
    tr_pheno[i, ]$usable <- tr_pheno[i, ]$dbh >=
      (min_dbh[which(as.character(min_dbh$code_name) ==
                       as.character(tr_pheno[i, ]$CodeName)),]$threshold_dbh)
  }

  return(tr_pheno)

}

fpv_subset_pheno_sr <- function(fpv = NULL, pheno = NULL) {

  # Generate list of unique species, for later use
  species <- unique(select(pheno, SpeciesName, SpeciesCode))

  # Create dbh column
  fpv$dbh <- sqrt((4 * fpv$Area) / pi)

  # Fill in fixed DBH for bromeliads, since it is not recorded
  # Using 5 cm per fruiting plant
  # Also ensure that each bromeliad fpv has a positive NFruiting
  fpv <- fpv %>%
    mutate(dbh = ifelse(Code %in% c("BPLU", "BPIN"), 5, dbh),
           NFruiting = ifelse(Code %in% c("BPLU", "BPIN") & is.na(NFruiting),
                              1, NFruiting),
           dbh = ifelse(Code %in% c("BPLU", "BPIN"), dbh* NFruiting, dbh))

  # Remove NA values and extra columns, because not useful here
  fpv <- fpv %>%
    filter(!is.na(dbh) ) %>%
    select(SpeciesName = Species, code_name = Code, DateOf = Datim,
           n_stems = NumStems, FruitCover, FruitMaturity, NFruiting, dbh)

  # Restrict to pheno species and sort by species & dbh
  fpv <- fpv %>%
    filter(code_name %in% species$SpeciesCode) %>%
    arrange(code_name, dbh)

  return(fpv)

}


plot_fpv_dbh <- function(fpv) {

  # Look at unchecked min dbhs, log tranformed
  p <- ggplot(fpv, aes(x = code_name, y = dbh)) +
    geom_boxplot(width = 0.5, fill = "gray90") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_y_continuous(trans = 'log10',
                       breaks = trans_breaks('log10', function(x) 10 ^ x),
                       labels = trans_format('log10', math_format(10 ^ .x))) +
    coord_flip() +
    labs(x = "Species\n", y = "\nDBH") +
    theme_bw()

  return(p)

}


fpv_get_min_dbh_sr <- function(fpv = NULL) {

  # Store uncorrected min dbhs
  min_dbh <- fpv %>%
    group_by(code_name) %>%
    summarise(threshold_dbh = min(dbh),
              n_trees = n())

  # ---- fpv_fix_dbh_brief --------------------------------------------------

  # Fix species with the most egregious lower outliers:
  # ACOL, AEDU, ARET, BCRA, FCOT, FMOR, KCAL,
  # LSPE, MCHI, MTIN, SOBO
  min_dbh[min_dbh$code_name == "ACOL", ]$threshold_dbh <-
    head(subset(fpv, code_name == "ACOL"))$dbh[4]

  min_dbh[min_dbh$code_name == "AEDU", ]$threshold_dbh <-
    head(subset(fpv, code_name == "AEDU"))$dbh[2]

  min_dbh[min_dbh$code_name == "ARET", ]$threshold_dbh <-
    head(subset(fpv, code_name == "ARET"))$dbh[2]

  min_dbh[min_dbh$code_name == "BCRA", ]$threshold_dbh <-
    head(subset(fpv, code_name == "BCRA"))$dbh[2]

  min_dbh[min_dbh$code_name == "FCOT", ]$threshold_dbh <-
    head(subset(fpv, code_name == "FCOT"))$dbh[2]

  min_dbh[min_dbh$code_name == "FMOR", ]$threshold_dbh <-
    head(subset(fpv, code_name == "FMOR"))$dbh[2]

  min_dbh[min_dbh$code_name == "KCAL", ]$threshold_dbh <-
    head(subset(fpv, code_name == "KCAL"))$dbh[2]

  min_dbh[min_dbh$code_name == "MCHI", ]$threshold_dbh <-
    head(subset(fpv, code_name == "MCHI"))$dbh[2]

  min_dbh[min_dbh$code_name == "MTIN", ]$threshold_dbh <-
    head(subset(fpv, code_name == "MTIN"))$dbh[3]

  min_dbh[min_dbh$code_name == "SOBO", ]$threshold_dbh <-
    head(subset(fpv, code_name == "SOBO"))$dbh[2]


  # ---- fpv_dbh_extra ------------------------------------------------------

  min_dbh[min_dbh$code_name == "TAME", ]$threshold_dbh <-
    rev(sort(subset(tr, SpeciesName == "Trichilia americana")$Dbh))[1]

  min_dbh[min_dbh$code_name == "SEXC", ]$threshold_dbh <-
    rev(sort(subset(tr, SpeciesName == "Sciadodendron excelsum")$Dbh))[1]

  min_dbh[min_dbh$code_name == "SGLN", ]$threshold_dbh <-
    rev(sort(subset(tr, SpeciesName == "Sapium glandulosum")$Dbh))[2]

  return(min_dbh)

}


biomass_max_sr <- function(df = NULL) {

  biomass <- tr_pheno %>%
    group_by(CodeName) %>%
    filter(usable == TRUE) %>%
    summarise(biomass_total_kg = sum(ProportionOfTreeInTransect * 47 * dbh ^ 1.9) / 1000,
              area_total = sum(abh_total))

  # Biomass per hectare and total basal area (151 transects, each 200 m^2)
  biomass <- biomass %>%
    mutate(biomass_max_kg_ha = biomass_total_kg / (151 * 200 / 10000))

  return(biomass)

}

biomass_sr <- function(biomass_max = NULL, indices = NULL) {

  biomass_avail <- indices %>%
    inner_join(biomass_max, by = c("SpeciesCode" = "CodeName")) %>%
    arrange(SpeciesName, year_of, month_of) %>%
    mutate(biomass_monthly_kg = avail * biomass_max_kg_ha)

  return(biomass_avail)

}

plot_biomass_species <- function(df = NULL, fill_col = c("#FFFFFF", brewer.pal(9, "YlGnBu"))) {

  p <- ggplot(df, aes(x = month_of, y = year_of, fill = biomass_monthly_kg)) +
    geom_tile(color = "gray50") +
    scale_fill_gradientn(colours = fill_col,
                         trans = sqrt_trans(),
                         name = "Fruit biomass (kg / ha)") +
    facet_wrap(~SpeciesName, nrow = 5) +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.key.width = unit(2.5, "cm")) +
    labs(title = "Available Fruit Biomass\n", x = "\nMonth", y = "Year\n")

  return(p)

}

plot_biomass_years <- function(df = NULL, fill_col = c("#FFFFFF", brewer.pal(9, "YlGnBu"))) {

  lim <- max(df$total_biomass)

  p <- ggplot(df, aes(x = month_of, y = year_of, fill = total_biomass)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(colours = fill_col,
                         name = "Fruit biomass (kg / ha)",
                         limits = c(0, lim)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.key.width = unit(2.5, "cm")) +
    labs(title = "Available Fruit Biomass by Month and Year\n",
         x = "\nMonth", y = "Year\n")

  return(p)

}

biomass_yearly_summary <- function(df = NULL) {

  biomass_yearly <- df %>%
    ungroup() %>%
    filter(as.numeric(as.character(year_of)) > 2007) %>%
    group_by(year_of, month_of) %>%
    summarise(total_biomass = sum(biomass_monthly_kg))

  biomass_yearly$year_of <- factor(biomass_yearly$year_of)

  return(biomass_yearly)

}