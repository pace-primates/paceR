library(tidyverse)
library(stringr)
library(lubridate)
library(paceR)
library(RColorBrewer)

# write_csv(ph, "data/pheno_2017-05-16.csv")
# write_csv(tr, "data/transect_2017-05-16.csv")

source("~/Dropbox/R/helpers_fc.R")

ph <- read_csv("data/pheno_2017-05-16.csv")
tr <- read_csv("data/transect_2017-05-16.csv", guess_max = Inf)
fpv <- tbl_df(read.csv("data/AllFPV.csv"))

# Read most recent fig file
figs <- tbl_df(read.csv("data/FicusData_Nov13_2013.csv"))

# Create set of species to exclude
exclude_species <- c("SCAP", "SPAV", "CCAN", "BUNG", "HCOU",
                     "ATIB", "GULM", "LCAN", "LSPE", "FUNK", "TRAC")

# Calcuate available biomass using the indices as weights
# biomass_avail_raw <- get_biomass_sr(ph, tr, fpv, figs, exclude_species, smooth = "none")
# biomass_avail_gam <- get_biomass_sr(ph, tr, fpv, figs, exclude_species, smooth = "gam")
biomass_avail_lo <- get_biomass_sr(ph, tr, fpv, figs, exclude_species, smooth = "loess")

# Individual species plots of biomass
plot_biomass_species(biomass_avail_lo)

b_summary_lo <- biomass_monthly_summary(biomass_avail_lo)


# Plots of total yearly biomass with species combined
plot_biomass_monthly(b_summary_lo)


# ---- calculate_fruit ---------------------------------------------------------

biomass <- b_summary_lo

# Create actual dates
biomass$month_num <- as.numeric(biomass$month_of)
biomass$date_of <- ymd(paste(biomass$year_of, biomass$month_num, "1", sep = "-"))
biomass$day_of_year <- yday(biomass$date_of)

# # Change last day_of_year to 366 and rearrange
# biomass[13, ]$day_of_year <- 366
# biomass <- biomass[, c(1, 4, 5, 2)]

# Spline
biomass_daily <- data_frame(date_of = seq.Date(min(biomass$date_of),
                                                   max(biomass$date_of),
                                                   by = "1 day"))
biomass_daily$spline <- spline(x = as.numeric(biomass$date_of),
                               y = biomass$total_biomass,
                               n = nrow(biomass_daily))$y

# Spline plot
plot(spline(x = biomass$date_of,
            y = biomass$total_biomass, n = nrow(biomass_daily)))
points(x = as.numeric(biomass$date_of),
       y = biomass$total_biomass, col = "red", pch = 16)


orkin <- read_csv("data/OrkinSamplingDates-Fernando.csv")
names(orkin)[2] <- "date_of"

sample_biomass <- left_join(orkin, biomass_daily)

names(sample_biomass)[c(2, 3)] <- c("CollectionDate", "Biomass (kg/ha)")

# write_csv(sample_biomass, "~/Desktop/orkin_biomass.csv")

ggplot(sample_biomass, aes(x = CollectionDate, y = `Biomass (kg/ha)`)) + geom_point()



# ---- new_plot -----------------------------------------------------------

biomass_daily$year_of = year(biomass_daily$date_of)
ggplot(filter(biomass_daily, year_of == 2014),
       aes(x = date_of, y = spline)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")



# ---- 2014_only ----------------------------------------------------------

# Get 2014 species only
s_2014 <- levels(factor(ph[year(ph$DateOf) == 2014, ]$SpeciesCode))
s_all <- levels(factor(ph$SpeciesCode))
exclude_species <- s_all[s_all %ni% s_2014]
exclude_species <- unique(c(exclude_species, "SCAP", "SPAV", "CCAN", "BUNG", "HCOU",
                            "ATIB", "GULM", "LCAN", "LSPE", "FUNK", "TRAC"))


biomass_avail_lo_2014 <- get_biomass_sr(ph, tr, fpv, figs, exclude_species, smooth = "loess")
b_summary_lo_2014 <- biomass_monthly_summary(biomass_avail_lo_2014)

biomass_2014 <- b_summary_lo_2014

# Create actual dates
biomass_2014$month_num <- as.numeric(biomass_2014$month_of)
biomass_2014$date_of <- ymd(paste(biomass_2014$year_of, biomass_2014$month_num, "1", sep = "-"))
biomass_2014$day_of_year <- yday(biomass_2014$date_of)

# Spline
biomass_daily_2014 <- data_frame(date_of = seq.Date(min(biomass_2014$date_of),
                                               max(biomass_2014$date_of),
                                               by = "1 day"))
biomass_daily_2014$spline <- spline(x = as.numeric(biomass_2014$date_of),
                               y = biomass_2014$total_biomass,
                               n = nrow(biomass_daily_2014))$y

# Spline plot
plot(spline(x = biomass_2014$date_of,
            y = biomass_2014$total_biomass, n = nrow(biomass_daily_2014)))
points(x = as.numeric(biomass_2014$date_of),
       y = biomass_2014$total_biomass, col = "red", pch = 16)

sb <- data_frame(bout = c("SB1", "SB2", "SB3", "SB4"),
                 start = c(ymd("2014-04-29"),
                           ymd("2014-06-03"),
                           ymd("2014-07-29"),
                           ymd("2014-11-25")),
                 end = c(ymd("2014-05-07"),
                         ymd("2014-06-25"),
                         ymd("2014-09-03"),
                         ymd("2014-12-10")))



# ---- plot_2014_biomass --------------------------------------------------

biomass_daily_2014$year_of = year(biomass_daily_2014$date_of)
biomass_daily_2014 <- filter(biomass_daily_2014, year_of == 2014)
biomass_2014 <- filter(biomass_2014, year_of == 2014)

p1 <- ggplot() +
  geom_col(data = biomass_daily_2014, aes(x = date_of, y = spline),
           width = 1, fill = "gray70", color = "gray70") +
  geom_col(data = biomass_daily_2014, aes(x = date_of, y = spline),
           width = 1, fill = "gray70", color = "gray70") +
  geom_rect(data = sb, aes(xmin = start, xmax = end, ymin = 0, ymax = 180),
            color = NA, fill = "red", alpha = 0.1) +
  geom_text(data = sb, aes(x = ((end - start) / 2) + start, y = 190, label = bout),
            color = "red") +
  geom_point(data = biomass_2014, aes(x = date_of, y = total_biomass),
           shape = 21, fill = "white", color = "black") +
  scale_x_date(breaks = seq.Date(from = ymd("2014-01-01"), to = ymd("2014-12-01"),
                                      by = "1 months"), date_labels = "%b") +
  theme_journal() +
  labs(x = "Date in 2014", y = "Available Fruit Biomass (kg/ha)")

rain <- get_pace_tbl(pace_db, "tblWeather")

rain <- make_date_cols(rain, DateOf)
rain$DateOf <- ymd(rain$DateOf)
rain_2014 <- filter(rain, year_of == 2014)

p2 <- ggplot() +
  geom_col(data = rain_2014, aes(x = DateOf, y = Rainfall),
           width = 1, fill = "gray70", color = "gray70") +
  scale_x_date(breaks = seq.Date(from = ymd("2014-01-01"), to = ymd("2014-12-01"),
                                 by = "1 months"), date_labels = "%b") +
  geom_rect(data = sb, aes(xmin = start, xmax = end, ymin = 0, ymax = 60),
            color = NA, fill = "red", alpha = 0.1) +
  geom_text(data = sb, aes(x = ((end - start) / 2) + start, y = 65, label = bout),
            color = "red") +
  theme_journal() +
  labs(x = "Date in 2014", y = "Daily Rainfall (mm)")

cowplot::plot_grid(p1, p2, align = "hv", ncol = 1,
                   labels = c("A", "B"))



# ---- n_trees_2014 -------------------------------------------------------

pheno <- pheno_prep_sr(ph, exclude_species, "Fruit")

# Get relevant FPV data corresponding to pheno species
fpv2 <- fpv_subset_pheno_sr(fpv, pheno)

# Plot FPV DBH data to see outliers
plot_fpv_dbh(fpv2)

# Fix minimum DBHs (currently done manually, need to verify)
min_dbh <- fpv_get_min_dbh_sr(fpv2)


# Set CGRA manually
min_dbh <- bind_rows(min_dbh,
                     data.frame(code_name = "CGRA",
                                threshold_dbh = 10,
                                n_trees = 1))

# Get relevant transect data corresponding to pheno species
# Also exclude individual trees that are too small to produce food based on FPVs
tr_pheno_fpv <- transect_subset_sr(tr, pheno, min_dbh)


# Count number of usable transect trees for each species
# Missing FGOL and FOBT because none in transects
table_s1 <- tr_pheno_fpv %>%
  group_by(CodeName) %>%
  filter(usable == TRUE) %>%
  summarise(ntrees_for_biomass = n())



# Count number of trees in FPVs that were used to determine min_dbh
# Missing CGRA because none in transects
table_s1 <- min_dbh %>%
  left_join(table_s1, by = c("code_name" = "CodeName"))

# Count total in transects
temp <- tr_pheno_fpv %>%
  group_by(CodeName, SpeciesName) %>%
  summarise(ntrees_in_transects = n())

table_s1 <- table_s1 %>%
  left_join(temp, by = c("code_name" = "CodeName"))

write_csv(table_s1, "~/Desktop/table_s1.csv")
