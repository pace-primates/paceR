rm(list = ls()[!(ls() %in% c("pace_db", "paceR_db"))])

# Problme with R and MacOS 10.13 and timezone, has to be set manually.
Sys.setenv(TZ = "America/Montreal")

### 1. Get all datasets ----
# Phenodata from Pacelab
ph <- getv_Phenology(paceR_db, project = "SR")

# FPV information from csv-table
fpv <- tbl_df(read.csv("./data/AllFPV.csv"))
fpv <- fpv %>% mutate_all(funs(replace(., . == "",NA))) %>%
  droplevels()

# Transect data from Pacelab db
# I used the detailed version, which also contains coordinates of transects start- and end-point
tr <- get_pace_tbl(paceR_db, "vVegetationTransect_detailed")
tr <- as.data.frame(tr) %>%
  filter(Project.NameOf == "Santa Rosa")

# One transect has been named "NA". To avoid problems with NA values, renamed to "NA_tr"
tr[tr == "NA"] <- "NA_tr"
tr[tr == ""] <- NA

# Correct date Transect 217, Meter 0 (all other dates for this transect are "2016-02-04")
tr[tr$TransectID == 217 & tr$Meter == 0,]$DateOf <- "2016-02-04"


### 2. Use Phenology data to calculate pheno-scores (indices) of eaten species ----
# Create set of species to exclude
# Jeremy: "Note: two types of exclusions here, most are wind dispersed or fruits aren't eaten so should ALWAYS be excluded.
# CELA, MARG, QOLE, SGLN are excluded for longer time frames because they were only included from ~2015 on"
# Last line of exclusions (starting from CSYL) were added by Jeremy and Amanda
exclude_species <- c("SCAP", "SPAV", "CCAN", "BUNG", "HCOU","ATIB", "GULM",
                     "LCAN", "LSPE", "FUNK",
                     "CSYL", "CGRA", "LARB","CELA", "MARG", "QOLE", "SGLN")
# SGLN has to be removed from this exclusion list, otherwise fpv_get_min_dbh_sr does not work, which specifically tests for this species
exclude_species <- exclude_species[!exclude_species %in% c("SGLN")]

# Prepare pheno table, only retaining non-excluded species and scores for mature fruits
pheno <- pheno_prep_sr(ph, exclude_species, "Fruit", "Mature")

# Summarizes available fruit indices per year and month
# For the moment, only "smooth = "none"
# Urs: "ungroup()" is necessary for a later step. Maybe we should include ungrouping in all paceR functions
indices <- pheno_avail_indices_sr(pheno, smooth = "none") %>%
  ungroup()

# Clean up
rm(list = c("exclude_species"))


### 3. Use Food Patch Visit (FPV) information to determine the minimum dbh of food tree species ----
# Only keep species that are used in pheno
# Also, the function transforms area into dbh and
# changes dbh and NFruiting values for BPLU and BPIN (species with fruit but no stem?)
fpv <- fpv_subset_pheno_sr(fpv, pheno)

# For some species, the following function determines min_dbh.
# Urs: Fernando, I am not sure how this works, but I guess nothing has to be changed here.
min_dbh <- fpv_get_min_dbh_sr(fpv, tr)
min_dbh <- suppressWarnings(bind_rows(min_dbh, data.frame(code_name = "CGRA", threshold_dbh = 10, n_trees = 1)))

### 4. Prepare Transect Data  ----
# 1. Only retain tree species in transects that are also found in pheno
# 2. Calculate dbh for all trees (fixed dbh for bromeliads)
# 3. Flag trees that meet the min_dbh criterion as 'usable'
tr_pheno_fpv <- transect_subset_sr(tr, pheno, min_dbh)

# Get the coordinates of transect and create polygons for all transects
tr_pt <- get_pace_tbl(pace_db, "tblVegetationTransectGridPoint")
tr_v <- get_vertical_transects(tr_full = distinct(tr, TransectID, .keep_all = TRUE),
                               data_dir = paste0(getwd(), "/data/"))
tr_h <- get_horizontal_transects(tr_full = distinct(tr, TransectID, .keep_all = TRUE), tr_pt)
all_tr <- bind_rows(tr_h, tr_v)
tr_geom <- tr_to_polys(all_tr_points = all_tr) %>%
  st_as_sf() %>%
  left_join(distinct(tr, TransectID), by = "TransectID") %>%
  mutate(tr_area = as.numeric(st_area(geometry)),
         tr_area = round(tr_area, digits = -1)) %>%
  select(-radius) %>%
  arrange(TransectID)

# Urs: Fernando, TransectID 346 is missing from transects (you edited this transect in Pacelab a while ago). Please have a look.
sort(setdiff(unique(tr$TransectID), unique(tr_geom$TransectID)))

# Add geometries of transects to tr_pheno_fpv
tr_pheno_fpv <- tr_pheno_fpv %>%
  left_join(tr_geom, by = "TransectID")

# Transects 251 and 319 are missing from tr_pheno_fpv
# probably because there are no pheno trees on these transects.
# Shouldn't be an issue because transect area is calculated from tr_geom not tr_pheno_fpv, but should be kept in mind
setdiff(tr_geom$TransectID, unique(tr_pheno_fpv$TransectID))

# Clean up
rm(list = c("tr_pt", "tr_v", "tr_h", "all_tr"))

### 5. Calculate biomass for entire study area and the full time period ----
# tr_geom has to be used here because not all transects are included into tr_pheno_fpv_geom
tr_area <- sum((distinct(tr_geom, TransectID, .keep_all = T))$tr_area, na.rm = TRUE)/10000

# Calculate max. and available biomass for entire area:
biomass_max <- biomass_max_sr(tr_pheno_fpv, tr_area)
biomass_avail <- biomass_avail_sr(biomass_max, indices)

### 6. Calculate Homeranges ----
ranging_pts_all <- readr::read_csv(paste0(getwd(), "/data/RangingWaypoints with FAC March 2014.csv"))

# For Mackenzie's data, get the start- and end-date of her study periods from the ranging points
# THIS HAS TO BE MODIFIED FOR OTHER YEARS
study_periods <- ranging_pts_all %>%
  mutate(YearOf = year(timestamp)) %>%
  group_by(YearOf) %>%
  summarize(date_begin = min(as.Date(timestamp)), date_end = max(as.Date(timestamp))) %>%
  select(-YearOf)

# Calculate homeranges.
# I set the min_nb_reloc a little bit lower than the standard, which is opted for full years
# (here, only 3-4 months are included per year)
hr <- get_habitat_use (ranging_waypoints = ranging_pts_all,
                       start_date = NULL, ints_per_year = NULL,
                       hr_periods = study_periods, min_nb_reloc = 200,
                       dir_to_files = paste0(getwd(), "/data"))
# Transform hr into a tidy sf-dataframe (note that not all information are included from the original table, e.g. UD)
hr_sf <- get_sf_from_hu(hr)

# Clean up
rm(list = c("study_periods", "hr"))

### 7. Calculate max. and available biomass for each homerange and time period ----
# Load Fig data, which is required for recalculation of fig biomass
figs <- tbl_df(read.csv(paste0(getwd(), "/data/FicusData_Nov13_2013.csv")))
# Turn UTM coords into sf_points, and df into sf_df
figs_sf <- figs_to_sf(figs)

# Create table with periods for each group from homerange data
hr_periods <- hr_sf %>%
  distinct(GroupCode, start_date, end_date) %>%
  as.data.frame() %>%
  group_by(GroupCode) %>%
  mutate(period_nr = row_number(),
         GroupPeriod = paste0(GroupCode, "_", period_nr))

# Then, calculate max and avail biomass per home range and period
if(exists("biomass_hr_temp")) rm(biomass_hr_temp)
if(exists("biomass_hr")) rm(biomass_hr)
for(i in 1:nrow(hr_periods)) {
  biomass_hr_temp <- biomass_avail_hr(group = hr_periods[i,]$GroupCode,
                                      period_start_date = hr_periods[i,]$start_date,
                                      period_end_date = hr_periods[i,]$end_date,
                                      group_period_label = hr_periods[i,]$GroupPeriod,
                                      hr_type_incl = 95,
                                      hr_sf, tr_geom, tr_pheno_fpv, indices, figs_sf)
  if(!exists("biomass_hr")) {
    biomass_hr <- biomass_hr_temp
  }
  else {
    biomass_hr <- rbind(biomass_hr, biomass_hr_temp)
  }
}
rm(biomass_hr_temp)
