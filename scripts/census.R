library(paceR)
Sys.setenv(TZ = 'UTC')

source("~/Dropbox/R/themes_fc.R")
source("~/Dropbox/R/helpers_fc.R")
source("~/Dropbox/R/plotting-functions.R")
library(tidyverse)
library(forcats)
library(lubridate)
library(RColorBrewer)

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", dbname = "monkey", password = NULL)
paceR_db <- src_mysql(group = "PACE", user = "camposf", dbname = "paceR", password = NULL)

census <- get_pace_tbl(paceR_db, "vCensusAnnual")

census <- census %>%
  mutate_each(funs(as.Date), DateStart, DateEnd, DateOf)

census <- filter(census, Project == "Santa Rosa")

census_group <- get_pace_tbl(pace_db, "tblGroup") %>%
  select(GroupID = ID, PrimateSpeciesID, NameLong)

species <- get_pace_tbl(pace_db, "tblPrimateSpecies") %>%
  select(PrimateSpeciesID = ID, Species = NameShort)

census <- census %>%
  inner_join(census_group) %>%
  inner_join(species)

`%ni%` = Negate(`%in%`)
bad_years <- c(1989, 1991, 1993:1998)

t1 <- census %>%
  filter(UseForTotalEstimate == 1) %>%
  group_by(Species, YearOfCensus) %>%
  summarise(total_n = sum(N)) %>%
  filter(total_n > 100)

# Add additional points manually
m <- tibble(Species = c("Mantled Howlers", "Capuchins"),
            YearOfCensus = c(1972, 1972),
            total_n = c(85, 297))
t1 <- bind_rows(t1, m)

# h_color <- brewer.pal(3, "Set1")[1]
h_color <- "firebrick"
c_color <- brewer.pal(3, "Set1")[2]

# Total population (howlers)
ggplot(filter(t1, Species == "Mantled Howlers" & YearOfCensus >= 1983),
       aes(x = YearOfCensus, y = total_n)) +
  # annotate(geom = "rect", xmin = 2007, xmax = 2013, ymin = -Inf, ymax = Inf,
  #          fill = "gray90") +
  geom_line(color = h_color) +
  geom_point(shape = 21, color = "white", fill = h_color, size = 3) +
  annotate(geom = "text", x = 1990, y = 300, label = "Mantled Howlers",
           hjust = 0, color = h_color) +
  coord_cartesian(y = c(0, 700)) +
  labs(x = "Year", y = "Number of Animals",
       title = "Total Population Size") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme_journal_x2() +
  scale_color_manual(values = h_color,
                     name = "", guide = FALSE)

# Total population (both)
ggplot(filter(t1, YearOfCensus >= 1983),
       aes(x = YearOfCensus, y = total_n, color = Species, fill = Species)) +
  # annotate(geom = "rect", xmin = 2007, xmax = 2013, ymin = -Inf, ymax = Inf,
  #          fill = "gray90") +
  geom_line() +
  geom_point(shape = 21, color = "white", size = 3) +
  annotate(geom = "text", x = 1990, y = 300, label = "Mantled Howlers",
           hjust = 0, color = h_color) +
  annotate(geom = "text", x = 1983, y = 550, label = "Capuchins",
           hjust = 0, color = c_color) +
  coord_cartesian(y = c(0, 700)) +
  labs(x = "Year", y = "Number of Animals",
       title = "Total Population Size") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  theme_journal_x2() +
  scale_color_manual(values = c(c_color, h_color),
                     name = "", guide = FALSE) +
  scale_fill_manual(values = c(c_color, h_color),
                    name = "", guide = FALSE) +
  geom_smooth(method = "loess", color = NA, span = 1)



t1 %>%
  spread(Species, total_n) %>%
  View()

census %>%
  filter(UseForTotalEstimate == 1) %>%
  group_by(Species, YearOfCensus, GroupID) %>%
  summarise(group_n = sum(N)) %>%
  ungroup() %>%
  group_by(Species, YearOfCensus) %>%
  summarise(total_n = sum(group_n),
            smallest = min(group_n),
            largest = max(group_n),
            mean = mean(group_n, na.rm = TRUE),
            n_groups = n()) %>%
  View()

d_alo <- census %>%
  filter(UseForDemography == 1 & Species == "Mantled Howlers") %>%
  unite(AgeSexClass, AgeClass, Sex)

d_alo$AgeSexClass <- fct_collapse(d_alo$AgeSexClass,
                                  "AdultFemale" = "A_F",
                                  "AdultMale" = "A_M",
                                  "Juvenile" = c("J_U", "LIM_U", "SA_M", "SIM_U"),
                                  "Infant" = c("I_U", "ID_U", "II_U"),
                                  "Unknown" = "U_U")
d_alo_summary <- d_alo %>%
  filter(YearOfCensus %ni% bad_years) %>%
  group_by(YearOfCensus, AgeSexClass) %>%
  summarise(n = sum(N)) %>%
  spread(AgeSexClass, n) %>%
  mutate(af_inf = AdultFemale / Infant,
         af_imm = AdultFemale / (Infant + Juvenile))

outl <- filter(d_alo_summary, YearOfCensus == 2011)

# AF-Infant ratio
ggplot(d_alo_summary, aes(x = YearOfCensus, y = af_inf)) +
  annotate(geom = "rect", xmin = 2007, xmax = 2013, ymin = -Inf, ymax = Inf,
           fill = "gray90") +
  geom_line() +
  annotate(geom = "point", x = outl$YearOfCensus, y = outl$af_inf,
           size = 4, color = h_color) +
  geom_point(shape = 21, fill = "black", color = "white", size = 2) +
  geom_smooth(method = "loess", color = h_color, fill = h_color) +
  expand_limits(y = 0.5) +
  geom_hline(yintercept = 1, lty = 2, color = "gray50") +
  theme_journal_x2() +
  labs(x = "Year", y = "# Adult Females / # Infants",
       title = "Adult Female-to-Infant Ratio") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5))

g_alo <- d_alo %>%
  filter(YearOfCensus %ni% bad_years) %>%
  group_by(YearOfCensus, GroupID) %>%
  summarise(group_n = sum(N)) %>%
  summarise(group_mean = mean(group_n),
            group_med = median(group_n),
            group_max = max(group_n),
            group_min = min(group_n))

temp <- gather(g_alo, var, value, -YearOfCensus)

# Group Size
ggplot(filter(g_alo, !YearOfCensus %in% c(1996, 1997, 1998)),
       aes(x = YearOfCensus)) +
  annotate(geom = "rect", xmin = 2007, xmax = 2013, ymin = -Inf, ymax = Inf,
           fill = "gray90") +
  geom_line(aes(y = group_med), color = h_color) +
  geom_line(aes(y = group_max), color = h_color, alpha = 0.7, size = 0.3) +
  geom_line(aes(y = group_min), color = h_color, alpha = 0.7, size = 0.3) +
  geom_point(aes(y = group_med), shape = 21, size = 3,
             color = "white", fill = h_color) +
  geom_point(aes(y = group_max), shape = 21, size = 1.5,
             color = "white", fill = h_color) +
  geom_point(aes(y = group_min), shape = 21, size = 1.5,
             color = "white", fill = h_color) +
  geom_ribbon(aes(ymin = group_min, ymax = group_max),
              fill = h_color, alpha = 0.3) +
  annotate(geom = "text", x = 1995, y = 43, label = "Max", color = h_color) +
  annotate(geom = "text", x = 1995, y = 0, label = "Min", color = h_color) +
  annotate(geom = "text", x = 1995, y = 17, label = "Median", color = h_color) +
  theme_journal_x2() +
  labs(x = "Year", y = "# Animals",
       title = "Group Size") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5))




temp <- d_alo %>%
  group_by(YearOfCensus) %>%
  summarise(total_n = sum(N))

d_alo$AgeSexClass <- factor(d_alo$AgeSexClass,
                            levels = c("AdultFemale", "AdultMale", "Juvenile", "Infant"))

d_alo_as <- d_alo %>%
  filter(YearOfCensus %ni% bad_years & AgeSexClass != "Unknown") %>%
  group_by(YearOfCensus, AgeSexClass) %>%
  summarise(n = sum(N)) %>%
  inner_join(temp) %>%
  mutate(as_prop = n / total_n)

d_alo_as_med <- d_alo_as %>%
  ungroup() %>%
  group_by(AgeSexClass) %>%
  summarise(class_med = median(as_prop))

ggplot() +
  annotate(geom = "rect", xmin = 2007, xmax = 2013, ymin = -Inf, ymax = Inf,
           fill = "gray90") +
  geom_hline(data = d_alo_as_med, aes(yintercept = class_med),
             color = "gray80") +
  geom_line(data = d_alo_as,
            aes(x = YearOfCensus, y = as_prop),
            color = h_color) +
  geom_point(data = d_alo_as,
             aes(x = YearOfCensus, y = as_prop),
             shape = 21, fill = h_color, color = "white", size = 3) +
  facet_wrap(~AgeSexClass, ncol = 1) +
  theme_journal_x2() +
  labs(x = "Year", y = "Proportion of Population",
       title = "Population Age/Sex Composition")

# Diseases
dis <- read_csv("~/Desktop/AAPA/diseases.csv")

ggplot(dis, aes(x = YearOf, y = Cases)) +
  annotate(geom = "rect", xmin = 2007, xmax = 2013, ymin = -Inf, ymax = Inf,
           fill = "gray90") +
  annotate(geom = "text", x = 2000, y = 18000, label = "Dengue",
           color = brewer.pal(3, "Set1")[2]) +
  annotate(geom = "text", x = 2011, y = 3000, label = "Chikungunya",
           color = brewer.pal(3, "Set1")[1]) +
  annotate(geom = "text", x = 2016, y = 500, label = "Zika",
           color = brewer.pal(3, "Set1")[3]) +
  geom_line(aes(color = Disease)) +
  geom_point(aes(fill = Disease),
             shape = 21, color = "white", size = 3) +
  theme_journal_x2() +
  scale_y_continuous(trans = sqrt_sign_trans()) +
  expand_limits(y = -1) +
  scale_color_brewer(palette = "Set1", guide = FALSE) +
  scale_fill_brewer(palette = "Set1", guide = FALSE) +
  theme(plot.caption = element_text(size = 7)) +
  labs(x = "Year", y = "# Cases",
       title = substitute('Cases of'~italic("Flavivirus")~'diseases'),
       subtitle = "Costa Rica",
       caption = "Data: PAHO / WHO")



# Rain
weather <- get_pace_tbl(pace_db, "tblWeather", collect = FALSE) %>%
  filter(ProjectID == 1) %>%
  select(DateOf, TemperatureMax, TemperatureMin, Rainfall) %>%
  collect()

rain <- weather %>%
  mutate(DateOf = ymd(DateOf),
         MonthOf = month(DateOf),
         YearOf = year(DateOf),
         DayOfYear = yday(DateOf)) %>%
  filter(!is.na(DateOf)) %>%
  select(-contains("Temperature"))

rain$MonthOf <- factor(rain$MonthOf, labels = month.abb)

rain_wide <- rain %>%
  group_by(YearOf, MonthOf) %>%
  summarise(Rainfall = sum(Rainfall)) %>%
  spread(YearOf, Rainfall) %>%
  as.data.frame()

rownames(rain_wide) <- rain_wide$MonthOf
rain_wide <- select(rain_wide, -MonthOf)

rain_monthly <- rain %>%
  group_by(YearOf, MonthOf) %>%
  summarise(RainMonthly = sum(Rainfall)) %>%
  ungroup() %>%
  group_by(MonthOf) %>%
  summarise(AvgRainMonthly = mean(RainMonthly, na.rm = TRUE))

rain_yearly <- rain %>%
  group_by(YearOf) %>%
  summarise(RainYearly = sum(Rainfall, na.rm = TRUE))

my_years <- c(rep("Growth", times = length(which(rain_yearly$YearOf <= 1992))),
              rep("Stable", times = length(which(rain_yearly$YearOf > 1992 & rain_yearly$YearOf < 2007))),
              rep("Decline", times = length(which(rain_yearly$YearOf >= 2007 & rain_yearly$YearOf < 2015))),
              rep("Recovery", times = length(which(rain_yearly$YearOf >= 2015))))

my_years <- factor(my_years, levels = c("Growth", "Stable", "Decline", "Recovery"))

library(superheat)


# ---- rainfall_heatmap ---------------------------------------------------

png("~/Desktop/AAPA/superheat2.png", width = 1024 * 1.5, height = 768 * 1.5, res = 150)
superheat(rain_wide,
          scale = F,

          # Aesthetics
          title = "Monthly and Annual Rainfall (mm)",
          title.size = 8,
          heat.pal = brewer.pal(9, "Blues"),
          legend.breaks = seq(0, 1000, by = 250),
          left.label.col = "white",
          bottom.label.col = "white",
          left.label.text.size = 4,
          bottom.label.text.size = 4,

          # bottom.label.text.angle = 90,
          # grid.hline.col = "white",
          # grid.vline.col = "white",
          # grid.vline.size = 0.25,
          # grid.hline.size = 0.25,

          membership.cols = my_years,
          grid.hline.col = "white",
          grid.vline.col = "white",
          grid.vline.size = 2,
          grid.hline.size = 0.25,

          # Right marginal plot
          yr = rain_monthly$AvgRainMonthly,
          yr.axis.name = "Mean Monthly Rainfall",
          yr.plot.type = "bar",
          yr.bar.col = "black",
          yr.obs.col = rep("beige", nrow(rain_monthly)),

          # Top marginal plot
          yt = rain_yearly$RainYearly,
          yt.axis.name = "Total Annual Rainfall",
          yt.plot.type = "bar",
          yt.bar.col = "black",
          yt.obs.col = rep("beige", nrow(rain_yearly)),
          yt.plot.size = 0.6)
dev.off()


# ---- next ---------------------------------------------------------------

rain_monthly2 <- rain %>%
  group_by(YearOf, MonthOf) %>%
  summarise(RainMonthly = sum(Rainfall)) %>%
  inner_join(rain_monthly) %>%
  mutate(RainAnom = RainMonthly - AvgRainMonthly)

rain_monthly2$season <- fct_collapse(rain_monthly2$MonthOf,
                                     EarlyDry = c("Dec", "Jan", "Feb"),
                                     LateDry = c("Mar", "Apr", "May"),
                                     EarlyWet = c("Jun", "Jul", "Aug"),
                                     LateWet = c("Sep", "Oct", "Nov"))

rain_season <- rain_monthly2 %>%
  group_by(YearOf, season) %>%
  summarise(RainSeason = sum(RainMonthly)) %>%
  ungroup() %>%
  group_by(season) %>%
  summarise(AvgRainSeason = mean(RainSeason, na.rm = TRUE))

rain_monthly3 <- rain_monthly2 %>%
  group_by(YearOf, season) %>%
  summarise(RainSeason = sum(RainMonthly)) %>%
  inner_join(rain_season) %>%
  mutate(RainAnom = RainSeason - AvgRainSeason)


lim <- max(abs(rain_monthly2$RainAnom), na.rm = TRUE)

ggplot(filter(rain_monthly2, MonthOf == "Oct"),
       aes(x = YearOf, y = RainAnom)) +
  annotate(geom = "rect", xmin = 2007, xmax = 2013, ymin = -Inf, ymax = Inf,
           fill = "gray90") +
  geom_line() +
  geom_point(aes(fill = RainAnom), shape = 21, color = "black", size = 3) +
  facet_wrap(~MonthOf) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_journal_x2() +
  scale_y_continuous(trans = sqrt_sign_trans()) +
  labs(x = "Year", y = "Rain Anomaly (mm)",
       title = "October Rainfall Anomalies (mm)") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       limits = c(-lim, lim), name = "") +
  theme(legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(1.5, "cm"))


lim <- max(abs(rain_monthly3$RainAnom), na.rm = TRUE)

ggplot(rain_monthly3, aes(x = YearOf, y = RainAnom)) +
  annotate(geom = "rect", xmin = 2007, xmax = 2013, ymin = -Inf, ymax = Inf,
           fill = "gray90") +
  geom_line() +
  geom_point(aes(fill = RainAnom), shape = 21, color = "black", size = 3) +
  facet_wrap(~season) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_journal_x2() +
  scale_y_continuous(trans = sqrt_sign_trans()) +
  labs(x = "Year", y = "Rain Anomaly (mm)",
       title = "Seasonal Rainfall Anomalies (mm)") +
  scale_x_continuous(breaks = seq(1980, 2015, by = 5)) +
  scale_fill_gradientn(colours = brewer.pal(11, "BrBG"),
                       limits = c(-lim, lim), name = "") +
  theme(legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(1.5, "cm"))

