library(paceR)
library(mgcv)
library(viridis)
library(grid)
library(broom)
library(lme4)
library(scales)
library(ggplot2)

load_pace_packages()

source('~/Github/paceR/R/calculate-pheno-stats.R')
system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", dbname = "monkey", password = NULL)
paceR_db <- src_mysql(group = "PACE", user = "camposf", dbname = "paceR", password = NULL)

ph <- getv_Phenology(paceR_db)

pheno <- fruit_biomass_sr(ph)

# ggplot(pheno, aes(x = month_of, y = index_avail)) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.25)) +
#   facet_wrap(~SpeciesCode, ncol = 5)

# Remove any species for which there aren't 12 months of data
remove_species <- pheno %>%
  group_by(SpeciesCode) %>%
  distinct(month_of) %>%
  summarise(n = n()) %>%
  filter(n < 12 | SpeciesCode %in% c("SCAP", "SPAV", "CCAN", "BUNG", "HCOU",
                                     "ATIB", "GULM", "LCAN", "LSPE", "FUNK"))

pheno <- pheno %>%
  filter(!(SpeciesCode %in% remove_species$SpeciesCode))

fr <- pheno %>%
  group_by(SpeciesName, year_of, month_of) %>%
  summarise(avail = mean(index_avail))

# Generate list of unique species, for later use
species <- unique(select(pheno, SpeciesName, SpeciesCode))

pheno$year_of <- factor(pheno$year_of)



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

# mods <- pheno %>%
#   group_by(SpeciesName) %>%
#   do(m = gamm(index_avail ~
#                 s(as.numeric(month_of),
#                   bs = "cc"),
#               data = .,
#               random = list(TreeID = ~1),
#               knots = list(month_of = c(1, 13))))

mods2 <- pheno %>%
  group_by(SpeciesName) %>%
  do(m = gamm(index_avail ~ s(as.numeric(month_of, bs = "cc")) +
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
  left_join(bare_months) %>%
  left_join(failed_years) %>%
  # mutate(avail_fixed = ifelse(!is.na(monthly_sum) | !is.na(yearly_sum), 0, avail))
  mutate(avail_fixed = ifelse(!is.na(yearly_sum), 0, avail))

ggplot(gam_pred, aes(x = month_of, y = year_of, fill = avail_fixed)) +
  geom_raster() +
  scale_fill_gradientn(colours = viridis(256),
                       trans = sqrt_trans(),
                       limits = c(0, 1),
                       name = "Availability Index") +
  facet_wrap(~SpeciesName, nrow = 5) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key.width = unit(2.5, "cm")) +
  labs(title = "GAMM predictions")



mods1 <- pheno %>%
  group_by(SpeciesName) %>%
  do(augment(lmer(index_avail ~ year_of + month_of + (1 | TreeID),
                  data = .)))

fruit_avail <- mods1 %>%
  group_by(SpeciesName, year_of, month_of) %>%
  summarise(avail = mean(.fitted))

fruit_avail[fruit_avail$avail < 0, ]$avail <- 0

ggplot(fruit_avail, aes(x = month_of, y = year_of, fill = avail)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_gradientn(colours = viridis(256),
                       trans = sqrt_trans(),
                       name = "Availability Index",
                       limits = c(0, 1)) +
  facet_wrap(~SpeciesName, nrow = 5) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key.width = unit(2.5, "cm")) +
  labs(title = "Fixed month and year, Random Tree")

ggplot(fr, aes(x = month_of, y = year_of, fill = avail)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_gradientn(colours = viridis(256),
                       trans = sqrt_trans(),
                       name = "Availability Index",
                       limits = c(0, 1)) +
  facet_wrap(~SpeciesName, nrow = 5) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key.width = unit(2.5, "cm")) +
  labs(title = "Raw (not modeled)")



yrs <- 2008:year(Sys.Date())

full_species <- pheno %>%
  select(SpeciesCode, year_of) %>%
  filter(as.numeric(as.character(year_of)) >= 2008) %>%
  group_by(SpeciesCode) %>%
  distinct(year_of) %>%
  arrange(SpeciesCode, year_of) %>%
  summarise(n = n()) %>%
  filter(n == length(yrs))

full_species <- full_species$SpeciesCode



# ---- transects ----------------------------------------------------------

tr <- get_pace_tbl(paceR_db, "vVegetationTransect")

# Get transect trees from target pheno species
tr_pheno <- tr %>%
  # filter(CodeName %in% species$SpeciesCode) %>%
  mutate(DateOf = ymd(DateOf)) %>%
  arrange(TransectID, DateOf, TreeID, StemSeqNum)

# Fill in fixed DBH for bromeliads, since it is not recorded
# Using 5 cm per fruiting plant
# Also ensure that each has a positive n_stems
tr_pheno %>%
  mutate(dbh = ifelse(CodeName %in% c("BPLU", "BPIN"), 5, Dbh),
         Dbh = ifelse(CodeName %in% c("BPLU", "BPIN"), 5, Dbh))

# Group by TreeID and calculate virtual DBH
tr_pheno <- tr_pheno %>%
  group_by(DateOf, TransectID, SpeciesName, TreeID, ProportionOfTreeInTransect,
           CodeName) %>%
  mutate(abh = (pi * Dbh ^ 2) / 4,
         n_stems = 1) %>%
  summarise(n_stems = n(),
            abh_total = sum(abh),
            dbh = sqrt(4 * abh_total / pi) * n_stems) %>%
  ungroup()














