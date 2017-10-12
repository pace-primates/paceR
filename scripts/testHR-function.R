rm(list = ls())
source("./R/get-homerange.R")

load_hr_biomass_packages()

ranging_pts_all <- readr::read_csv("./data/RangingWaypoints with FAC March 2014.csv")

# Test the function with year-intervals
# min_nb_reloc set to 200 because none of the years has enough location points to meet the minimum criterium as specified in the function
hr <- get_homerange (ranging_waypoints = ranging_pts_all, start_date = NULL, intervals_per_year = 1,
                     hr_periods = NULL, min_nb_reloc = 200, output = "hr")

ud <- get_homerange (ranging_waypoints = ranging_pts_all, start_date = NULL, intervals_per_year = 1,
                     spec_hr_periods = NULL, min_nb_reloc = 200, output = "ud")

ob <- get_homerange (ranging_waypoints = ranging_pts_all, start_date = NULL, intervals_per_year = 1,
                     spec_hr_periods = NULL, min_nb_reloc = 200, output = "ob")

# Now, try with more specific intervals
hr_periods <- ranging_pts_all %>%
  mutate(YearOf = year(timestamp)) %>%
  group_by(YearOf) %>%
  summarize(date_begin = min(as.Date(timestamp)), date_end = max(as.Date(timestamp))) %>%
  select(-YearOf)


hr_2 <- get_homerange (ranging_waypoints = ranging_pts_all, start_date = NULL, intervals_per_year = 1,
                     hr_periods = hr_periods, min_nb_reloc = 200, output = "hr")
