library(paceR)
Sys.setenv(TZ = 'UTC')

# load_pace_packages()
library(tidyverse)
library(forcats)
library(lubridate)
library(superheat)
library(RColorBrewer)

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", dbname = "monkey", password = NULL)
paceR_db <- src_mysql(group = "PACE", user = "camposf", dbname = "paceR", password = NULL)

ph <- getv_Phenology(paceR_db, project = "SR")

exclude_species <- c("AEDU", "AOCC", "BPLU", "BPIN", "BUNG", "CGUA", "CPAN",
                     "FUNK", "JPUN", "MARB", "MARG", "MCAL", "PGUA", "RMON",
                     "RTHU", "SPAV", "ACOL")

# For fruits
exclude_species <- c("SCAP", "SPAV", "CCAN", "BUNG", "HCOU",
                     "ATIB", "GULM", "LCAN", "LSPE", "FUNK",
                     "TRAC")


# ---- mature_leaves ------------------------------------------------------

pheno <- pheno_prep_sr(ph, exclude_species, item = "Leaf", maturity = "Immature")

# Freeze at April 2016
pheno <- filter(pheno, DateOf <= ymd("2017-01-01"))

indices_lo <- pheno_avail_indices_sr(pheno, smooth = "none")
indices_lo[which(indices_lo$avail > 1), ]$avail <- 1


# ggplot(indices_lo, aes(x = month_of, y = as.numeric(as.character(year_of)), fill = avail)) +
#   geom_tile(color = "gray50") +
#   scale_fill_gradientn(colours = c("#FFFFFF", brewer.pal(9, "YlOrRd")),
#                        trans = scales::sqrt_trans(),
#                        limits = c(0, 1),
#                        name = "Availability Index") +
#   scale_y_continuous(limits = c(2000, 2016)) +
#   facet_wrap(~SpeciesName, ncol = 7) +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         strip.background = element_blank(),
#         panel.grid = element_blank(),
#         # axis.text.x = element_text(angle = 90, vjust = 0.5),
#         legend.key.width = grid::unit(2.5, "cm")) +
#   labs(x = "Month", y = "Year") +
#   coord_polar()

temp <- indices_lo %>%
  group_by(year_of, month_of) %>%
  summarise(mean_avail = mean(avail)) %>%
  spread(year_of, mean_avail) %>%
  as.data.frame()

rownames(temp) <- temp$month_of
temp <- select(temp, -month_of)

avail_monthly <- indices_lo %>%
  group_by(month_of) %>%
  summarise(avg_avail_monthly = mean(avail, na.rm = TRUE))

avail_yearly <- indices_lo %>%
  group_by(year_of) %>%
  summarise(avail_yearly = mean(avail, na.rm = TRUE))


# ---- leaves_plot --------------------------------------------------------

png("~/Desktop/new_leaves.png", width = 1024 * 1.5, height = 768 * 1.5, res = 150)
superheat(temp,
          scale = F,

          # Aesthetics
          title = "Proportion New Leaf Cover",
          title.size = 8,
          grid.hline.col = "white",
          grid.vline.col = "white",
          bottom.label.text.angle = 90,
          heat.pal = brewer.pal(9, "YlOrRd"),
          # legend.breaks = seq(0, 1000, by = 250),
          left.label.col = "white",
          bottom.label.col = "white",
          left.label.text.size = 4,
          bottom.label.text.size = 4,

          # Right marginal plot
          yr = avail_monthly$avg_avail_monthly,
          yr.axis.name = "Monthly",
          yr.plot.type = "bar",
          yr.bar.col = "black",
          yr.obs.col = rep("gray90", nrow(avail_monthly)),
          yr.plot.size = 0.3,

          # Top marginal plot
          yt = avail_yearly$avail_yearly,
          yt.axis.name = "Annual",
          yt.plot.type = "bar",
          yt.bar.col = "black",
          yt.obs.col = rep("gray90", nrow(avail_yearly)),
          yt.plot.size = 0.4
          )
dev.off()



# ---- next ---------------------------------------------------------------


# Vectors
indices_lo$SpeciesName <- mapvalues(indices_lo$SpeciesName,
                                    from = "Ficus morazaniana",
                                    to = "Ficus bullenei*")

temp <- indices_lo %>%
  group_by(SpeciesName) %>%
  mutate(date_of = parse_date_time(paste(year_of, month_of, "01", sep = "-"),
                                   orders = "%y-%b-%d"),
         y_date = decimal_date(date_of) - year(date_of),
         date_deg = y_date * 360) %>%
  do(direction = vector.averaging(.$date_deg, .$avail)$direction,
     distance = vector.averaging(.$date_deg, .$avail)$distance)

temp$direction <- unlist(temp$direction)
temp$distance <- unlist(temp$distance)

# Scale distances to years for overlay plotting
temp$new_dist <- (temp$distance / max(temp$distance)) * 16 + 2000

# Reorder by distance
species <- temp %>%
  arrange(-distance) %>%
  select(SpeciesName)

species <- species$SpeciesName

temp$SpeciesName <- factor(temp$SpeciesName, levels = species)
indices_lo$SpeciesName <- factor(indices_lo$SpeciesName, levels = species)

ggplot(temp, aes(x = direction, xend = direction, y = new_dist, yend = 2000)) +
  geom_segment() +
  coord_polar() +
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 30)) +
  scale_y_continuous(limits = c(2000, 2016)) +
  facet_wrap(~SpeciesName, ncol = 7) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x = "Month", y = "Year")


ggplot(indices_lo, aes(x = month_of, y = as.numeric(as.character(year_of)), fill = avail)) +
  geom_tile(color = "gray50") +
  scale_fill_gradientn(colours = c("#FFFFFF", brewer.pal(9, "YlOrRd")),
                       trans = scales::sqrt_trans(),
                       limits = c(0, 1),
                       guide = FALSE) +
  scale_y_continuous(limits = c(2000, 2016)) +
  facet_wrap(~SpeciesName, ncol = 7) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.key.width = grid::unit(2.5, "cm")) +
  labs(x = "Month", y = "Year") +
  coord_polar()

old_leaf_seas <- temp
old_leaf_seas$maturity <- "Mature Leaves"


# ----  new_leaves --------------------------------------------------------

pheno <- pheno_prep_sr(ph, exclude_species, item = "Leaf", maturity = "Immature")
indices_lo <- pheno_avail_indices_sr(pheno, smooth = "loess")
indices_lo[which(indices_lo$avail > 1), ]$avail <- 1

# Vectors
indices_lo$SpeciesName <- mapvalues(indices_lo$SpeciesName,
                                    from = "Ficus morazaniana",
                                    to = "Ficus bullenei*")

temp <- indices_lo %>%
  group_by(SpeciesName) %>%
  mutate(date_of = parse_date_time(paste(year_of, month_of, "01", sep = "-"),
                                   orders = "%y-%b-%d"),
         y_date = decimal_date(date_of) - year(date_of),
         date_deg = y_date * 360) %>%
  do(direction = vector.averaging(.$date_deg, .$avail)$direction,
     distance = vector.averaging(.$date_deg, .$avail)$distance)

temp$direction <- unlist(temp$direction)
temp$distance <- unlist(temp$distance)

# Scale distances to years for overlay plotting
temp$new_dist <- (temp$distance / max(temp$distance)) * 16 + 2000

# Reorder by distance
species <- temp %>%
  arrange(-distance) %>%
  select(SpeciesName)

species <- species$SpeciesName

temp$SpeciesName <- factor(temp$SpeciesName, levels = species)
indices_lo$SpeciesName <- factor(indices_lo$SpeciesName, levels = species)

new_leaf_seas <- temp
new_leaf_seas$maturity <- "New Leaves"


# ---- combined_plots -----------------------------------------------------

leaf_seas <- rbind(new_leaf_seas, old_leaf_seas)
leaf_seas$mo <- cut(leaf_seas$direction, breaks = seq(0, 360, by = 30),
                    labels = month.abb)

ggplot(leaf_seas, aes(x = direction, xend = direction, y = distance, yend = 0,
                      fill = maturity, color = maturity)) +
  geom_segment(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 360), breaks = seq(15, 345, by = 30),
                     minor_breaks = seq(0, 330, by = 30),
                     labels = c(month.abb)) +
  scale_color_manual(values = c("#31a354",  "#fec44f"), name = "") +
  scale_fill_manual(values = c("#31a354", "#fec44f"), name = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_line(color = "gray95"),
        panel.grid.major.x = element_blank()) +
  labs(y = "Seasonality Index\n", x = "") +
  coord_polar()

# "#31a354"
c1 <- "springgreen4"
c2 <- "#fec44f"

pal <- c(c1, c2)
deu <- dichromat(pal, type = "d")
pro <- dichromat(pal, type = "p")
tri <- dichromat(pal, type = "t")

pal <- pal

ggplot(leaf_seas, aes(x = mo, fill = maturity, color = maturity, alpha = maturity)) +
  geom_vline(xintercept = seq(0.5, 11.5, by = 1), color = "gray95") +
  # geom_bar(alpha = 0.5, position = "identity") +
  geom_bar(position = "identity") +
  scale_x_discrete(month.abb, drop = FALSE) +
  scale_color_manual(values = pal, name = "", labels = c("Mature Leaves  ", "New Leaves")) +
  scale_fill_manual(values = pal, name = "", labels = c("Mature Leaves  ", "New Leaves")) +
  scale_alpha_manual(values = c(0.75, 0.5), name = "", labels = c("Mature Leaves  ", "New Leaves")) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank()) +
  labs(x = "", y = "Number of Species in Peak Phenophase\n") +
  # labs(title = "Tritanopia") +
  coord_polar()

ggplot(leaf_seas, aes(x = distance, fill = maturity, color = maturity)) +
  geom_density(alpha = 0.5) +
  geom_rug() +
  scale_color_manual(values = c("#31a354",  "#fec44f"), name = "") +
  scale_fill_manual(values = c("#31a354", "#fec44f"), name = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "\nSeasonality Index", y = "Density\n")

ggplot(indices_lo, aes(x = month_of, y = as.numeric(as.character(year_of)), fill = avail)) +
  geom_tile(color = "gray50") +
  scale_fill_gradientn(colours = c("#FFFFFF", brewer.pal(9, "YlGn")),
                       # trans = scales::sqrt_trans(),
                       limits = c(0, 1),
                       guide = FALSE) +
  scale_y_continuous(limits = c(2000, 2016)) +
  facet_wrap(~SpeciesName, ncol = 7) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.key.width = grid::unit(2.5, "cm")) +
  labs(x = "Month", y = "Year") +
  coord_polar()


# ---- summaries ----------------------------------------------------------

pheno %>%
  group_by(SpeciesCode, PhenologyDate) %>%
  summarise(n = n_distinct(TreeID)) %>%
  ungroup() %>%
  summary()

pheno %>%
  group_by(PhenologyDate) %>%
  summarise(n = n_distinct(SpeciesCode)) %>%
  ungroup() %>%
  summary()

n_distinct(pheno$PhenologyDate)
