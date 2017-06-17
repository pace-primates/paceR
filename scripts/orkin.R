library(tidyverse)

write_csv(ph, "data/pheno_2017-05-16.csv")
write_csv(tr, "data/transect_2017-05-16.csv")

ph <- read_csv("data/pheno_2017-05-16.csv")
tr <- read_csv("data/transect_2017-05-16.csv", guess_max = Inf)
fpv <- tbl_df(read.csv("data/AllFPV.csv"))

# Read most recent fig file
figs <- tbl_df(read.csv("data/FicusData_Nov13_2013.csv"))


# Create set of species to exclude
exclude_species <- c("SCAP", "SPAV", "CCAN", "BUNG", "HCOU",
                     "ATIB", "GULM", "LCAN", "LSPE", "FUNK",
                     "TRAC")

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


orkin <- read_csv("~/Desktop/OrkinSamplingDates-Fernando.csv")
names(orkin)[2] <- "date_of"

sample_biomass <- left_join(orkin, biomass_daily)

names(sample_biomass)[c(2, 3)] <- c("CollectionDate", "Biomass (kg/ha)")

write_csv(sample_biomass, "~/Desktop/orkin_biomass.csv")


ggplot(sample_biomass, aes(x = CollectionDate, y = `Biomass (kg/ha)`)) + geom_point()

