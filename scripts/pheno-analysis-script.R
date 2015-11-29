library(paceR)
Sys.setenv(TZ = 'UTC')

load_pace_packages()

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", dbname = "monkey", password = NULL)
paceR_db <- src_mysql(group = "PACE", user = "camposf", dbname = "paceR", password = NULL)

# Get data from PACE

# Use special phenology query
ph <- getv_Phenology(paceR_db, project = "SR")
# No special transect query, so just get raw table
tr <- get_pace_tbl(paceR_db, "vVegetationTransect")
# Read in FPV file (not currently in PACE!)
fpv <- tbl_df(read.csv("data/AllFPV.csv"))


# Create set of species to exclude
exclude_species <- c("SCAP", "SPAV", "CCAN", "BUNG", "HCOU",
                    "ATIB", "GULM", "LCAN", "LSPE", "FUNK")

# Calcuate available biomass using the indices as weights
biomass_avail_raw <- get_biomass_sr(ph, tr, fpv, exclude_species, smooth = "none")
biomass_avail_gam <- get_biomass_sr(ph, tr, fpv, exclude_species, smooth = "gam")
biomass_avail_lo <- get_biomass_sr(ph, tr, fpv, exclude_species, smooth = "loess")


# Individual species plots of biomass
plot_biomass_species(biomass_avail_raw)
plot_biomass_species(biomass_avail_gam)
plot_biomass_species(biomass_avail_lo)


b_summary_raw <- biomass_monthly_summary(biomass_avail_raw)
b_summary_gam <- biomass_monthly_summary(biomass_avail_gam)
b_summary_lo <- biomass_monthly_summary(biomass_avail_lo)


# Plots of total yearly biomass with species combined
plot_biomass_monthly(b_summary_raw)
plot_biomass_monthly(b_summary_gam)
plot_biomass_monthly(b_summary_lo)


# Side by side plot
b_summary_raw$method <- "none"
b_summary_gam$method <- "gam"
b_summary_lo$method <- "loess"
temp <- bind_rows(b_summary_raw, b_summary_lo, b_summary_gam)
temp$method <- factor(temp$method, levels = c("none", "loess", "gam"))
plot_biomass_monthly(temp) + facet_wrap(~method)




# Longer script

# Only work with Santa Rosa data
pheno <- pheno_prep_sr(ph, exclude_species, "Fruit")

# Calculate fruit availability indices
indices_raw <- pheno_avail_indices_sr(pheno, smooth = "none")
indices_gam <- pheno_avail_indices_sr(pheno, smooth = "gam")
indices_lo <- pheno_avail_indices_sr(pheno, smooth = "loess")

# Plot indices
plot_pheno_indices(indices_raw)
plot_pheno_indices(indices_lo)
plot_pheno_indices(indices_gam)

# Get relevant FPV data corresponding to pheno species
fpv <- fpv_subset_pheno_sr(fpv, pheno)

# Plot FPV DBH data to see outliers
plot_fpv_dbh(fpv)

# Fix minimum DBHs (currently done manually, need to verify)
min_dbh <- fpv_get_min_dbh_sr(fpv)


# Get relevant transect data corresponding to pheno species
# Also exclude individual trees that are too small to produce food based on FPVs
tr_pheno_fpv <- transect_subset_sr(tr, pheno, min_dbh)


# Count number of usable transect trees for each species
tr_pheno %>%
  group_by(CodeName) %>%
  filter(usable == TRUE) %>%
  summarise(num_trees = n())


# Potential peak biomass for each species
biomass_max <- biomass_max_sr(tr_pheno_fpv)

# Calcuate available biomass using the indices as weights
biomass_avail_raw <- biomass_avail_sr(biomass_max, indices_raw)
biomass_avail_gam <- biomass_avail_sr(biomass_max, indices_gam)
biomass_avail_lo <- biomass_avail_sr(biomass_max, indices_lo)
