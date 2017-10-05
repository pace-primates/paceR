rm(list = ls())

library(paceR)
load_hr_packages()

ranging_pts_all <- readr::read_csv("./data/RangingWaypoints with FAC March 2014.csv")

# Test the function with year-intervals
# min_nb_reloc set to 200 because none of the years has enough location points to meet the minimum criterium
# as specified in the function (becuase only a few months included)
hr <- get_habitat_use (ranging_waypoints = ranging_pts_all, start_date = NULL, ints_per_year = 1,
                     hr_periods = NULL, min_nb_reloc = 200)

# Then get a simple feature table with the polygons for each homerange
hr_sf <- get_sf_from_hu(hr)

# Now, try with more specific intervals
hr_periods <- ranging_pts_all %>%
  mutate(YearOf = year(timestamp)) %>%
  group_by(YearOf) %>%
  summarize(date_begin = min(as.Date(timestamp)), date_end = max(as.Date(timestamp))) %>%
  select(-YearOf)

hr_2 <- get_habitat_use (ranging_waypoints = ranging_pts_all, start_date = NULL, ints_per_year = 1,
                     hr_periods = hr_periods, min_nb_reloc = 200)
hr_sf <- get_sf_from_hu(hr_2)
