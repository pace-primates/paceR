library(paceR)
library(tidyverse)
library(lubridate)
library(stringr)
library(RColorBrewer)
library(scales)

source("~/Dropbox/R/themes_fc.R")

# write_csv(ph, "data/pheno_2017-06-09.csv")

ph <- read_csv("data/pheno_2017-06-09.csv")
tr <- read_csv("data/transect_2017-05-16.csv", guess_max = Inf)
fpv <- tbl_df(read.csv("data/AllFPV.csv"))

# Read most recent fig file
figs <- tbl_df(read.csv("data/FicusData_Nov13_2013.csv"))


# Create set of species to exclude
exclude_species <- c("SCAP", "SPAV", "CCAN", "BUNG", "HCOU",
                     "ATIB", "GULM", "LCAN", "LSPE", "FUNK",
                     "TRAC")

exclude_species <- c("SCAP", "SPAV", "CCAN", "BUNG", "HCOU",
                     "ATIB", "GULM", "LCAN", "LSPE", "FUNK")

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

ggplot() +
  geom_line(data = biomass, aes(x = date_of, y = total_biomass),
             color = "gray40", lty = 3, size = 0.5) +
  geom_line(data = biomass_daily, aes(x = date_of, y = spline),
            size = 0.5) +
  geom_point(data = biomass, aes(x = date_of, y = total_biomass),
             shape = 21, fill = "black", color = "white", size = 2.5) +
  theme_journal_x2() +
  coord_cartesian(xlim = c(ymd("2015-01-01", ymd("2016-01-01")))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs (x = "", y = "Fruit Biomass (kg/ha)",
        title = "Fruit biomass during 2015",
        subtitle = "Spline vs linear interpolation")


biomass_webb <- biomass_daily %>%
  mutate(year_of = year(date_of)) %>%
  filter(year_of %in% seq(2014, 2016)) %>%
  select(-year_of) %>%
  rename(estimated_biomass = spline)

write_csv(biomass_webb, "~/Desktop/webb_biomass.csv")

