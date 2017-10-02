#' Convert a Santa Rosa phenology table from long to wide format and prepare for fruit biomass calculation.
#'
#' @param pheno The data frame of phenology data from Santa Rosa.
#' @param exclude_species A character vector of species codes to exclude.
#' @param item The food part to focus on. Default is "Fruit".
#' @param maturity Maturity of the food part to focus on. Default is "Mature". Only other valid option is "Immature".
#'
#' @export
#' @examples
#' pheno <- pheno_prep_sr(ph, exclude_species = c("SPAV", "FUNK"))
pheno_prep_sr <- function(pheno = NULL, exclude_species = "", item = "Fruit",
                          maturity = "Mature", ...){

  if (length(item) > 1 | !(item %in% c("Fruit", "Flower", "Leaf"))) {
    stop("Unknown food items. Valid values include 'Fruit', 'Flower', and 'Leaf'.")
  }

  if (length(maturity) > 1 | !(maturity %in% c("Mature", "Immature"))) {
    stop("Unknown maturity value. Valid values are 'Mature' and 'Immature'.")
  }

  # Retain only records related to fruit
  pheno <- pheno %>% filter(TaxonPart == item)

  pheno$TaxonPart <- "Item"

  # Discard irrelevant columns
  pheno <- pheno %>% select(-PhenologyPercent, -PhenologyCount, -ScientificName,
                            -RecordDate, -ResearcherName, -Comments, -SiteName)

  # New useful columns
  pheno <- pheno %>%
    mutate(year_of = year(DateOf),
           month_of = month(DateOf))

  # Exclude 2006 pheno data because no maturity info
  pheno <- pheno %>% filter(year_of > 2006)

  pheno$month_of <- factor(pheno$month_of, labels = month.abb[1:12])

  # First unite the "TaxonPart" and "Measurement" columns
  ph_wide <- pheno %>% unite(TaxonPartMeasurement, c(TaxonPart, Measurement))

  # Now spread PhenologyScore using TaxonPartMeasurement as the key
  ph_wide <- ph_wide %>% spread(TaxonPartMeasurement, PhenologyScore)

  # Fix maturity code 5 (change to zero)
  ph_wide[which(ph_wide$Item_Maturity == 5), ]$Item_Maturity <- 0

  if (maturity == "Mature") {
    ph_wide <- ph_wide %>%
      mutate_at(vars("Item_Cover", "Item_Maturity"), as.numeric)  %>%
      mutate(index_avail = (Item_Cover / 4) * (Item_Maturity / 4)) %>%
      filter(!is.na(index_avail))
  }
  else if (maturity == "Immature") {
    ph_wide <- ph_wide %>%
      mutate_at(vars("Item_Cover", "Item_Maturity"), as.numeric) %>%
      mutate(index_avail = (Item_Cover / 4) * ((4 - Item_Maturity) / 4)) %>%
      filter(!is.na(index_avail))
  }

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


#' Calculate  availability indices for Santa Rosa using different methods.
#'
#' @param pheno The data frame of phenology data from Santa Rosa.
#' @param smooth Either "none", "loess", or "gam. The default is "none".
#'
#' @export
#' @examples
#' indices_lo <- pheno_avail_indices_sr(pheno, smooth = "loess")
pheno_avail_indices_sr <- function(pheno = NULL, smooth = "none", ...){

  pheno$year_of <- factor(pheno$year_of)

  # Generate list of unique species, for later use
  species <- unique(select(pheno, SpeciesName, SpeciesCode))

  if (smooth == "none") {

    res <- pheno %>%
      group_by(SpeciesName, year_of, month_of) %>%
      summarise(avail = mean(index_avail))

    res$year_of <- factor(res$year_of)

  }
  else if (smooth == "gam") {

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
      do(m = mgcv::gamm(index_avail ~ s(as.numeric(month_of), bs = "cc", k = 13) +
                    year_of,
                  random = list(TreeID = ~1),
                  knots = list(month_of = c(1, 13)),
                  data = .))

    gam_pred <- list()
    for (i in 1:nrow(mods2)) {
      c_species <- mods2[i, ]$SpeciesName
      c_gam <- mods2[i, ]$m[[1]]$gam
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
      mutate(avail = ifelse(!is.na(yearly_sum) | !is.na(monthly_sum), 0, avail))

    res <- gam_pred
  }
  else if (smooth == "loess") {

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
      group_by(SpeciesName, year_of) %>%
      do(l = loess(index_avail ~ as.numeric(month_of), data = ., span = 0.5))

    loess_pred <- list()
    for (i in 1:nrow(mods2)) {
      c_species <- mods2[i, ]$SpeciesName
      c_loess <- mods2[i, ]$l[[1]]
      c_year <- mods2[i, ]$year_of
      set <- filter(pheno, SpeciesName == c_species & year_of == c_year)

      # If fewer than 2 months in a given year, set to mean value
      if (nrow(set) > 10 & length(unique(set$month_of)) > 2) {
        loess_pred[[i]] <- set %>%
          mutate(avail = predict(c_loess, newdata = set))
      }
      else {
        loess_pred[[i]] <- set %>%
          mutate(avail = mean(set$index_avail))
      }
    }

    loess_pred <- bind_rows(loess_pred)

    loess_pred <- loess_pred %>%
      group_by(SpeciesName, year_of, month_of) %>%
      summarise(avail = mean(avail))

    loess_pred[loess_pred$avail < 0, ]$avail <- 0

    loess_pred <- loess_pred %>%
      left_join(bare_months, by = c("SpeciesName", "month_of")) %>%
      left_join(failed_years, by = c("SpeciesName", "year_of")) %>%
      mutate(avail = ifelse(!is.na(yearly_sum) | !is.na(monthly_sum), 0, avail))

    res <- loess_pred
  }

  res <- inner_join(res, species, by = "SpeciesName")

  return(res)

}


#' Generate a heatmap of fruit availability indices.
#'
#' @param df The data frame of index data to plot.
#' @param fill_col A color palette for the fill gradient.
#'
#' @export
#' @examples
#' plot_pheno_indices(indices_lo)
plot_pheno_indices <- function(df = NULL, fill_col = c("#FFFFFF", RColorBrewer::brewer.pal(9, "YlGnBu"))){

  p <- ggplot(df, aes(x = month_of, y = year_of, fill = avail)) +
    geom_tile(color = "gray50") +
    scale_fill_gradientn(colours = fill_col,
                         trans = scales::sqrt_trans(),
                         limits = c(0, 1),
                         name = "Availability Index") +
    facet_wrap(~SpeciesName, nrow = 5) +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.key.width = grid::unit(2.5, "cm")) +
    labs(title = "Fruit Availability Indices") +
    coord_equal()


  return(p)

}


#' Get relevant FPV data corresponding to pheno species
#'
#' @param fpv FPV data.
#' @param pheno Phenology data created by pheno_prep_fruit_sr.
#'
#' @export
#' @examples
#' fpv <- fpv_subset_pheno_sr(fpv, pheno)
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
           dbh = ifelse(Code %in% c("BPLU", "BPIN"), dbh * NFruiting, dbh))

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


#' Set minimum DBHs for fruit-producing trees of each species.
#'
#' @param fpv The FPV data.
#'
#' @export
#' @examples
#' min_dbh <- fpv_get_min_dbh_sr(fpv)
fpv_get_min_dbh_sr <- function(fpv = NULL) {

  # Store uncorrected min dbhs
  min_dbh <- fpv %>%
    group_by(code_name) %>%
    summarise(threshold_dbh = min(dbh),
              n_trees = n())

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


  min_dbh[min_dbh$code_name == "TAME", ]$threshold_dbh <-
    rev(sort(subset(tr, SpeciesName == "Trichilia americana")$Dbh))[1]

  min_dbh[min_dbh$code_name == "SEXC", ]$threshold_dbh <-
    rev(sort(subset(tr, SpeciesName == "Sciadodendron excelsum")$Dbh))[1]

  min_dbh[min_dbh$code_name == "SGLN", ]$threshold_dbh <-
    rev(sort(subset(tr, SpeciesName == "Sapium glandulosum")$Dbh))[2]

  return(min_dbh)

}


#' Get relevant transect data corresponding to pheno species.
#' Also exclude individual trees that are too small to produce food based on FPVs.
#'
#' @param tr PACE transect data.
#' @param pheno Phenology data created by pheno_prep_fruit_sr.
#' @param min_dbh FPV minimum DBH data created by fpv_get_min_dbh_sr.
#'
#' @export
#' @examples
#' tr_pheno_fpv <- transect_subset_sr(tr, pheno, min_dbh)
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
  tr_pheno <- tr_pheno %>%
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

  # Join to get min_dbh
  tr_pheno <- left_join(tr_pheno,
                        select(min_dbh, CodeName = code_name, threshold_dbh),
                        by = "CodeName")

  # Set "usable" flag to indicate if tree dbh >= threshold
  tr_pheno <- tr_pheno %>%
    mutate(usable = dbh >= threshold_dbh)

  return(tr_pheno)

}


#' Create boxplots of DBHs for trees in FPV data for each species to find size of fruit-producing trees.
#'
#' @param fpv The FPV data.
#'
#' @export
#' @examples
#' plot_fpv_dbh(fpv)
plot_fpv_dbh <- function(fpv) {

  # Look at unchecked min dbhs, log tranformed
  p <- ggplot(fpv, aes(x = code_name, y = dbh)) +
    geom_boxplot(width = 0.5, fill = "gray90") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_y_continuous(trans = 'log10',
                       breaks = scales::trans_breaks('log10', function(x) 10 ^ x),
                       labels = scales::trans_format('log10', math_format(10 ^ .x))) +
    coord_flip() +
    labs(x = "Species\n", y = "\nDBH") +
    theme_bw()

  return(p)

}


#' Calculate max potential biomass for each species (if availability were 1).
#'
#' @param df Dataframe of data for biomass calculation created by transect_subset_sr
#' @param transect_area Total area in hectares sampled by transects
#'
#' @export
#' @examples
#' biomass_max <- biomass_max_sr(tr_pheno_fpv, transect_area)
biomass_max_sr <- function(df = NULL, transect_area) {

  biomass <- df %>%
    group_by(CodeName) %>%
    filter(usable == TRUE) %>%
    summarise(biomass_total_kg = sum(ProportionOfTreeInTransect * 47 * dbh ^ 1.9) / 1000,
              area_total = sum(abh_total))

  # Biomass per hectare and total basal area
  biomass <- biomass %>%
    mutate(biomass_max_kg_ha = biomass_total_kg / transect_area)

  return(biomass)

}


#' Calcuate available biomass using the indices as weights.
#'
#' @param biomass_max Dataframe of max biomass data created by biomass_max_sr
#' @param indices Index data to use as weights created by pheno_avail_indices_sr
#'
#' @export
#' @examples
#' biomass_avail_lo <- biomass_avail_sr(biomass_max, indices_lo)
biomass_avail_sr <- function(biomass_max = NULL, indices = NULL) {

  biomass_avail <- indices %>%
    inner_join(biomass_max, by = c("SpeciesCode" = "CodeName")) %>%
    arrange(SpeciesName, year_of, month_of) %>%
    mutate(biomass_monthly_kg = avail * biomass_max_kg_ha)

  return(biomass_avail)

}


#' Generate a heatmap of availabile fruit biomass for each species.
#'
#' @param df The data frame of biomass data created by biomass_avail_sr.
#' @param fill_col A color palette for the fill gradient.
#'
#' @export
#' @examples
#' plot_biomass_species(biomass_avail_lo)
plot_biomass_species <- function(df = NULL, fill_col = c("#FFFFFF", RColorBrewer::brewer.pal(9, "YlGnBu"))) {

  p <- ggplot(df, aes(x = month_of, y = year_of, fill = biomass_monthly_kg)) +
    geom_tile(color = "gray50") +
    scale_fill_gradientn(colours = fill_col,
                         trans = scales::sqrt_trans(),
                         name = "Fruit biomass (kg / ha)") +
    facet_wrap(~SpeciesName, nrow = 5) +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.key.width = grid::unit(2.5, "cm")) +
    labs(title = "Available Fruit Biomass\n", x = "\nMonth", y = "Year\n") +
    coord_equal()

  return(p)

}


#' Summarize available biomass data by summing species in each month / year.
#'
#' @param df Dataframe of available biomass data created by biomass_avail_sr
#'
#' @export
#' @examples
#' biomass_summary_lo <- biomass_monthly_summary(biomass_avail_lo)
biomass_monthly_summary <- function(df = NULL) {

  biomass_monthly <- df %>%
    ungroup() %>%
    filter(as.numeric(as.character(year_of)) > 2007) %>%
    group_by(year_of, month_of) %>%
    summarise(total_biomass = sum(biomass_monthly_kg))

  biomass_monthly$year_of <- factor(biomass_monthly$year_of)

  return(biomass_monthly)

}

#' Heatmap plots of total monthly biomass with species combined
#'
#' @param df Dataframe of summarized available biomass data created by biomass_monthly_summary.
#' @param fill_col A color palette for the fill gradient.
#'
#' @export
#' @examples
#' plot_biomass(biomass_summary_lo)
plot_biomass_monthly <- function(df = NULL, fill_col = c("#FFFFFF", RColorBrewer::brewer.pal(9, "YlGnBu"))) {

  lim <- max(df$total_biomass)

  p <- ggplot(df, aes(x = month_of, y = year_of, fill = total_biomass)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(colours = fill_col,
                         name = "Fruit biomass (kg / ha)",
                         limits = c(0, lim)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.key.width = grid::unit(2.5, "cm")) +
    labs(title = "Available Fruit Biomass by Month and Year\n",
         x = "\nMonth", y = "Year\n") +
    coord_equal()

  return(p)

}

#' Calculate available biomass. This function is a wrapper for most of the other phenology functions.
#'
#' @param ph Dataframe of raw PACE phenology data.
#' @param tr Dataframe of raw PACE transect data.
#' @param fpv Dataframe of FPV data (from CSV file).
#' @param exclude_species A character vector of species codes to exclude.
#' @param smooth Either "none", "loess", or "gam. The default is "none".
#'
#' @export
#' @examples
#' exclude_species <- c("SCAP", "SPAV", "CCAN", "BUNG", "HCOU", "ATIB", "GULM", "LCAN", "LSPE", "FUNK")
#' biomass_avail_lo <- get_biomass_sr(ph, tr, fpv, exclude_species, smooth = "loess")
get_biomass_sr <- function(ph = NULL, tr = NULL, fpv = NULL, figs = NULL, exclude_species = "", smooth = "none") {

  # Only work with Santa Rosa data
  pheno <- pheno_prep_sr(ph, exclude_species, "Fruit")

  # Calculate fruit availability indices
  indices <- pheno_avail_indices_sr(pheno, smooth = smooth)

  # Get relevant FPV data corresponding to pheno species
  fpv <- fpv_subset_pheno_sr(fpv, pheno)

  # Fix minimum DBHs (currently done manually, need to verify)
  min_dbh <- fpv_get_min_dbh_sr(fpv)

  # Currently, no min DBH for CGRA due to absence in FPVs
  # Must set manually
  min_dbh <- suppressWarnings(bind_rows(min_dbh,
                       data.frame(code_name = "CGRA",
                                  threshold_dbh = 10,
                                  n_trees = 1)))

  # Get relevant transect data corresponding to pheno species
  # Also exclude individual trees that are too small to produce food based on FPVs
  tr_pheno_fpv <- transect_subset_sr(tr, pheno, min_dbh)

  # Calculate total sampled area
  # Account for change in methodology in 2016 that increased transect area from 200 to 400 m^2
  t_200 <- tr[which(lubridate::year(tr$DateOf) < 2016), ]
  t_400 <- tr[which(lubridate::year(tr$DateOf) >= 2016), ]

  n_200_transects <- length(unique(t_200$TransectID))
  n_400_transects <- length(unique(t_400$TransectID))

  transect_area <- ((n_200_transects * 200) + (n_400_transects * 400)) / 10000

  # Potential peak biomass for each species
  biomass_max <- biomass_max_sr(tr_pheno_fpv, transect_area)

  # Calcuate available biomass using the indices as weights
  biomass_avail <- biomass_avail_sr(biomass_max, indices)

  if (!is.null(figs)) {

    biomass_avail <- biomass_avail %>%
      filter(!str_detect(SpeciesName, "Ficus"))

    # Monthly availability indices for all phenology fig species
    fig_indices <- indices %>%
      filter(str_detect(SpeciesName, "Ficus"))

    # Actual fig data, one row per tree
    # Calculate max biomass per tree
    fig_data <- figs %>%
      filter(!is.na(VirtualFicusCBH)) %>%
      mutate(dbh = VirtualFicusCBH / pi,
             biomass_tree_max_kg = (47 * dbh ^ 1.9) / 1000)

    # Join tree data to indices
    # One row per tree per pheno month
    fig_data <- suppressWarnings(inner_join(fig_indices,
                                            fig_data, by = "SpeciesCode"))

    # Sum of max biomass per species
    # Add up max biomass of each tree
    # HARD CODED VALUE: Fig sampling area 791 ha
    fig_biomass_max <- fig_data %>%
      ungroup() %>%
      group_by(SpeciesName, year_of, month_of, avail, SpeciesCode) %>%
      summarise(biomass_total_kg = sum(biomass_tree_max_kg),
                biomass_max_kg_ha = biomass_total_kg / 791)

    # Available
    fig_biomass_avail <- fig_biomass_max %>%
      mutate(biomass_monthly_kg = biomass_max_kg_ha * avail)

    biomass_avail <- bind_rows(biomass_avail, fig_biomass_avail)
  }

  return(biomass_avail)

}

#' Calculate mean resultant vector from directional data, weighted by magnitudes
#' Based on function in SDMTools, but fixes problem with incorrect quadrants
#'
#' @param direction a vector of directions given in degrees (0 - 360) if
#' \code{deg}==TRUE or in radians if \code{deg}==FALSE
#' @param distance a vector of distances associated with each direction
#' @param deg a boolean object defining if \code{direction} is in degrees
#' (TRUE) or radians (FALSE)
#'
#' @export
#' @examples
#' vector.averaging(c(10,20,70,78,108), distance=10)
#' vector.averaging(c(159,220,258,273,310),distance=runif(5))
vector.averaging <-  function(direction, distance, deg = TRUE) {
  if (deg) direction = direction * pi / 180 #convert to radians
  n <- length(direction) #get the length of direction vector
  if (any(is.na(direction))) { #ensure no NA data
    warning('NAs in data'); pos = which(is.na(direction)); direction = direction[-pos]; distance = distance[-pos]
  } else {
    sinr <- sum(sin(direction))
    cosr <- sum(cos(direction))
    if (sqrt((sinr ^ 2 + cosr ^ 2))/n > .Machine$double.eps) {
      Ve <- sum(distance * sin(direction)) / n
      Vn <- sum(distance * cos(direction)) / n
      UV <- sqrt(Ve ^ 2 + Vn ^ 2)
      AV <- atan2(Ve, Vn)

      AV[AV < 0] <- AV + 2 * pi

      #perform some checks and correct when output in wrong quadrant
      if (is.null(AV)) {
        return(list(distance = NA,direction = NA))
      } else {
        if (deg) AV = AV * 180 / pi #convert back to degrees
        return(list(distance = UV, direction = AV))
      }
    } else {
      return(list(distance = NA, direction = NA))
    }
  }
}

#' Get gps points for vertical transects
#'
#' @param paceR The src_mysql connection to the PACE Database.
#' @param tr_full Dataframe with tblVegetationTransect from Pacelab
#' @param data_dir Define directory in which the file 'v_transfect_2016.gpx' can be found.
#'
#' @export
#' @examples
#' get_vertical_transects <- function(paceR, tr_full_data, data_dir = "data/")
get_vertical_transects <- function(paceR, tr_full, data_dir = "data/") {
  
  # Use gps points from file
  vt <- rgdal::readOGR(dsn = paste0(data_dir, "v_transect_2016.gpx"), layer = "waypoints")
  vt2 <- spTransform(vt, CRSobj = CRS("+init=epsg:32616"))
  
  vt_df <- tbl_df(vt2) %>%
    select(name, x = coords.x1, y = coords.x2)
  
  vt_df <- vt_df %>%
    mutate(endpoint = str_extract(name, "[a-zA-z]+"))
  
  vt_df$name <- str_replace(vt_df$name, "End.", "")
  vt_df$name <- str_replace(vt_df$name, "Start.", "")
  
  vt_df <- vt_df %>%
    group_by(name) %>%
    mutate(endpoint = case_when(y == min(y) ~ "S.",
                                y == max(y) ~ "N."))
  
  vt_df <- unite(vt_df, endpoint, endpoint, name, sep = "")
  
  v_res <- select(tr_full, TransectID, TransectBegin, TransectEnd) %>%
    inner_join(vt_df, by = c("TransectBegin" = "endpoint")) %>%
    rename(start_x = x, start_y = y)
  
  v_res <- v_res %>%
    inner_join(vt_df, by = c("TransectEnd" = "endpoint")) %>%
    rename(end_x = x, end_y = y)
  
  v_res <- v_res %>%
    mutate(transect = str_extract(TransectBegin, "[0-9]+"))
  
  tr_v <- select(v_res, -TransectBegin, -TransectEnd)
  
  tr_v$transect <- paste0("v_", tr_v$transect)
  
  # Set width
  tr_v$radius <- 2
  return(tr_v)
}

#' Get gps points for horizontal transects
#'
#' @param paceR The src_mysql connection to the PACE Database.
#' @param tr_full Dataframe with 'tblVegetationTransect' from Pacelab
#' @param tr_pt Dataframe with 'tblVegetationTransectGridPoint' from Pacelab
#'
#' @export
#' @examples
#' get_horizontal_transects <- function(paceR, tr_full, tr_pt)
get_horizontal_transects <- function(paceR, tr_full, tr_pt) {
  
  tr_begin <- tr_full %>%
    select(ID, matches("Grid")) %>%
    inner_join(select(tr_pt, ID, GpsUtm), by = c("GridPointBeginID" = "ID")) %>%
    separate(GpsUtm, into = c("zone", "char", "x", "y"), sep = " ") %>%
    rename(start_x = x, start_y = y) %>%
    select(-matches("Grid"), -zone, -char)
  
  tr_end <- tr_full %>%
    select(ID, matches("Grid")) %>%
    inner_join(select(tr_pt, ID, GpsUtm), by = c("GridPointEndID" = "ID")) %>%
    separate(GpsUtm, into = c("zone", "char", "x", "y"), sep = " ") %>%
    rename(end_x = x, end_y = y) %>%
    select(-matches("Grid"), -zone, -char)
  
  tr_h <- inner_join(tr_begin, tr_end, by = "ID")
  
  tr_h$transect <- paste0("h_", tr_h$ID)
  
  tr_h <- mutate_at(tr_h, vars(-transect, -ID), as.numeric)
  
  # Set width
  tr_h$radius <- 1
  
  tr_h <- rename(tr_h, TransectID = ID)
  return(tr_h)
}

#' Transforms start- and end-points of transects into polygons (rectangles) with respective width
#'
#' @param all_tr_points Dataframe bind_rows(horizontal_transect, vertical_transects)
#'
#' @export
#' @examples
#' tr_to_polys <- function(all_tr_points)
tr_to_polys <- function(all_tr_points){
  
  # First, define function to transform start- and end-points to lines
  get_line <- function(df) {
    res <- st_linestring(cbind(c(df$start_x, df$end_x), c(df$start_y, df$end_y)))
    return(res)
  }
  
  # Transform points into lines
  temp <- all_tr_points %>%
    group_by(ID, radius) %>%
    nest() %>%
    mutate(lines = purrr::map(data, ~ get_line(.)))
  
  # Use UTM zone 16 as projection, and turn into sf-dataframe
  tran_lines <- temp$lines %>%
    st_sfc(crs = 32616)
  
  tran_lines <- st_as_sf(data.frame(id = temp$ID, radius = temp$radius), tran_lines)
  
  # Use sp package To make rectangles with respective width (capStyle flat not yet inlcuded into sf)
  tran_sl <- as(tran_lines, "Spatial")
  
  tran_poly <- rgeos::gBuffer(tran_sl, byid = TRUE, capStyle = "flat",
                              width = tran_sl$radius)
  
  return(tran_poly)
}