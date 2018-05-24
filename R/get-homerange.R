#' Loads various packages required to estimate biomass per homerange
#'
#' No description
#'
#' @export
#' @examples
#' load_pace_packages()
load_hr_biomass_packages <- function(...)
{
  packages <- c('adehabitatHR', 'adehabitatLT', 'plyr', 'lubridate', 'scales', 'tidyverse', 'stringr',
                'RColorBrewer', 'rgdal', 'gridExtra', 'rgeos', 'colorspace',
                'stringr', 'tidyr', 'dplyr', 'ggplot2', 'readr', 'sf')
  for (package in packages) {
    if (!isTRUE(require(package, character.only = TRUE))) {
      install.packages(package)
      require(package, character.only = TRUE, ...)
    } else
      require(package, character.only = TRUE, ...)
  }
}



#' Calculate home ranges for time intervals
#' 
#' (Use intervals that start on first of month when used with phenology)
#'
#' @param ranging_waypoint Table with ranging waypoint - MORE SPECIFICS
#' @param start_date Begin of first time interval. By default (NULL), Jan 1st of the earliest timestamp in ranging dataset - NOT YET IMPLEMENTED
#' @param ints_per_year Default is 1 (i.e. yearly homeranges). Set to NULL if not needed. Currently implemented:
#'  12 (1 month-intervals)
#'  8 (1.5 months-intervals)
#'  4 (3 months-intervals)
#'  2 (6 months-intervals)
#'  1 (1 year-intervals)
#' @param hr_periods If custom periods for home ranges are required (e.g. 05-02-2012 - 20-07-2012). Default is NULL.
#'  Create object of type: data.frame(date_begin = start_vec, date_end = end_vec)
#'  With start_vec including all start_dates and end_vec all end_dates
#'    - Overwrites ints_per_year if both are specified
#' @param min_nb_reloc Minimum number of location points per interval. Intervals for which the data are too sparse are removed.
#'   Default is NULL, which uses Fernando's criteria
#    - 96 pts (~8 full days) for monthly home range
#    - 192 pts (~16 full days) for quarterly home range
#    - 384 pts (~32 full days) for half-yearly home range
#    - 768 pts (~64 full days) for annual home range
#' @param output What type of object should be returned? ud, ob, or hr (sf-dataframe with homerange polygons)
#'
#' @export
#' @examples
#'
get_homerange <- function (ranging_waypoints, start_date = NULL, ints_per_year = 1,
                           hr_periods = NULL, min_nb_reloc = NULL, output = c("ud", "hr", "ob")){
  # Are all arguments are correcly set?
  # If hr_periods are defined, set ints_per_year to NULL and check if min_nb_reloc is defined
  if(!is.null(hr_periods)){
    if(!is.null(ints_per_year)){
      warning ("Arguments ints_per_year and hr_periods are both defined. Custom periods as specified in hr_periods are used")
      ints_per_year <- NULL
    }
    if(is.null(min_nb_reloc)){
      warning("hr_periods are defined but no value for min_nb_reloc. Minimum number of location points per interval set to 100, please check if appropriate")
      min_nb_reloc <- 100
    }}
  # If hr_periods are not defined, check if ints_per_year are set to one of the implemented values
  if(!is.null(ints_per_year)){
    if(!(ints_per_year %in% c(12, 8, 4, 2, 1))) stop("Number of intervals not implemented. Use 12, 8, 4, 2, or 1 for ints_per_year")
  }
  # Check if output type is correctly specified
  if(length(output) >1 | !(output[1] %in% c("ud", "hr", "ob"))) stop("Incorrect output type specified. Use 'ud', 'hr', or 'ob'")

  # Set time zone to UTC, otherwise problems with some interval calculations (see below)
  # But save original timezone before to set timezone back to original value
  timezone.backup <- Sys.timezone()
  Sys.setenv(TZ = 'UTC')

  # Only use the points flagged for calculating home ranges
  pts_included <- filter(ranging_waypoints, use_for_hr == TRUE) %>%
    mutate(group = as.factor(group))

  # Convert each group's total path to an ltraj trajectory
  ran <-adehabitatLT::as.ltraj(xy = pts_included[, c("x","y")],
                               date = pts_included$timestamp,
                               id = pts_included$group,
                               infolocs = as.data.frame(pts_included[, c(3,6:12)]))
  # Convert to data frame
  ran_df <- as.tibble(adehabitatLT::ld(ran)) %>%
    rename(date_of = date)

  # URS: Could also use ran_df$date_of here instead of pts_included$timestamp
  date_begin <- floor_date(min(pts_included$timestamp), unit = "year")
  date_end <- ceiling_date(max(pts_included$timestamp), unit = "year") - days(1)

  ### CALCULATE TIME INTERVALS
  # Intervals can also be provided as an argument (using hr_periods).
  # This if helpful if each year only has several month of data (as, for example, with Mackenzie's data)
  if(!is.null(ints_per_year)){
    if (ints_per_year == 12) { # monthly
      start_vec <- seq(date_begin, date_end, "1 months")
      end_vec <- start_vec[-1] - days(1)
      end_vec <- c(end_vec, date_end) # This only works when Sys.setenv(TZ='UTC'), otherwise timezone and time is changed (why?)

      ints <- data.frame(block_type = "month",
                         date_begin = start_vec,
                         date_end = end_vec)

    } else if (ints_per_year == 8) { # eighth
      start_vec1 <- seq(date_begin, date_end, "3 months")
      start_vec2 <- seq((date_begin + months(1) + days(15)),
                        date_end, "3 months")
      start_vec <- sort(c(start_vec1, start_vec2))

      end_vec1 <- start_vec1[-1] - days(1)
      end_vec2 <- start_vec2 - days(1)
      end_vec <- sort(c(end_vec1, end_vec2, date_end))

      ints <- data.frame(block_type = "eighth",
                         date_begin = start_vec,
                         date_end = end_vec)

    } else if (ints_per_year == 4) { # quarter
      start_vec <- seq(date_begin - months(2) + days(15),
                       date_end, "3 months")
      end_vec <- start_vec[-1] - days(1)
      end_vec <- c(end_vec, date_end)

      ints <- data.frame(block_type = "quarter",
                         date_begin = start_vec,
                         date_end = end_vec)

    } else if (ints_per_year == 2) { # half
      start_vec <- seq(date_begin - months(2) + days(15),
                       date_end, "6 months")
      end_vec <- start_vec[-c(1)] - days(1)
      start_vec <- start_vec[-length(start_vec)]

      ints <- data.frame(block_type = "half",
                         date_begin = start_vec,
                         date_end = end_vec)

    } else if (ints_per_year == 1) { # year
      start_vec <- seq(date_begin, date_end + days(1), "1 year")
      end_vec <- end_vec <- start_vec[-c(1)] - days(1)
      start_vec <- start_vec[-length(start_vec)]

      ints <- data.frame(block_type = "year",
                         date_begin = start_vec,
                         date_end = end_vec)
    }} else if (!is.null(hr_periods)) { # Custom periods
      ints <- hr_periods %>%
        mutate(block_type = paste0("custom_", row_number())) %>%
        select(block_type, date_begin, date_end)
    }

  # Create scale entry for each group for the time interval from
  # start of study to end of study
  ob_all <- NULL
  for(i in 1:length(levels(ran_df$id))) {
    temp <- cbind(ints, rep(levels(ran_df$id)[i], times = nrow(ints)))
    names(temp)[4] <- "id"
    ob_all <- rbind(ob_all, temp)
  }
  # rearrange
  ob_all <- as.tibble(ob_all) %>%
    mutate(nb_reloc = 0) %>%
    select(block_type, id, nb_reloc, date_begin, date_end)

  # How many location points actually lie in each time interval
  # for each group? We do this by joining to ran_df
  temp <- left_join(ob_all, ran_df, by = "id")

  # Create date interval for filtering
  temp$date_interval <- interval(temp$date_begin, temp$date_end)

  # Count number of points for each group in each date interval
  ob_all <- temp %>%
    filter(date_of %within% date_interval) %>%
    group_by(id, block_type, date_begin, date_end) %>%
    summarise(nb_reloc = n()) %>%
    ungroup

  # Remove intervals for which the data are too sparse
  # If min_nb_relos is NULL, use Fernando's criteria
  if(is.null(min_nb_reloc)){
    ob <- ob_all %>%
      filter((block_type == "month" & nb_reloc >= 96) |
               (block_type == "quarter" & nb_reloc >= 192) |
               (block_type == "half" & nb_reloc >= 384) |
               (block_type == "year" & nb_reloc >= 768))
  }else{
    ob <- ob_all %>% filter(nb_reloc >= min_nb_reloc)
  }

  ob$ints <- interval(ob$date_begin, ob$date_end)
  ob <- select(ob, -date_begin, -date_end)

  # Habitat MAPS
  lc <- rgdal::readGDAL(fname = "./data/LC-2011-03-06.tif")
  sp::fullgrid(lc) <- FALSE
  names(lc) <- "habitat"

  ndvi <- rgdal::readGDAL(fname = "./data/NDVI-2011-03-06.tif")
  fullgrid(ndvi) <- FALSE
  names(ndvi) <- "ndvi"

  # Create boundary rectangle and clip habitat map and ndvi
  bounds <- ran_df %>%
    summarise(xmin = min(x) - 100, xmax = max(x) + 100,
              ymin = min(y) - 100, ymax = max(y) + 100) %>%
    mutate(clip_box = st_polygon(list(rbind(c(xmin, ymin), c(xmin, ymax), c(xmax, ymax),
                                            c(xmax, ymin),c(xmin, ymin)))))

  clip_rect <- st_sfc(geom = st_sfc(bounds$clip_box)) %>%
    st_set_crs("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    as("Spatial")

  hab <- lc[clip_rect, drop = TRUE]
  age <- ndvi[clip_rect, drop = TRUE]

  # Create HCL color palette for land cover maps
  lc_grad <- colorRampPalette(colors = rev(heat_hcl(4, h = c(130, 70),
                                                    c = c(80, 30),
                                                    l = c(45, 95),
                                                    power = c(1/5, 2))))(4)

  # Calculate Home ranges
  ob$area_core <- 0
  ob$area_primary <- 0
  ob$area_total <- 0
  ob$ndvi_core <- 0
  ob$ndvi_primary <- 0
  ob$ndvi_total <- 0
  ob$ndvi_high <- 0
  ob$ndvi_medium <- 0
  ob$ndvi_low <- 0

  ud <- list() # Using ud <- vector("list", nrow(ob)) might be faster
  vv <- list()

  # Calculate diffusion values
  for(i in 1:length(levels(ran_df$id))){
    vv_id <- levels(ran_df$id)[i]
    vv[[i]] <- BRB.D(ran[id = vv_id],
                     Tmax = 90*60,
                     Lmin = 5,
                     habitat = hab,
                     activity = NULL)
    names(vv[[i]]) <- levels(ran_df$id)[i]
  }

  # Warning: Might be slow!
  # Can repeat for recursion / intensity distributions (t = "RD" or t = "ID")

  for(i in 1:nrow(ob)){
    cat(i, "/", nrow(ob), " homeranges calculated", '\r')
    temp <- suppressWarnings(area.BRB(
      x = ran[id = ob[i, ]$id],
      start.date = int_start(ob[i, ]$ints),
      end.date = int_end(ob[i, ]$ints) + days(1),
      hab = hab,
      ndvi = ndvi,
      iso = c(50, 70, 95),
      t = "UD",
      vv = vv[[ob[i, ]$id]]
    ))
    # Add values to ob
    ob[i, ]$area_core <- temp$hr$hr50$area
    ob[i, ]$area_primary <- temp$hr$hr70$area
    ob[i, ]$area_total <- temp$hr$hr95$area

    ob[i, ]$ndvi_core <- temp$ndvi$ndvi50
    ob[i, ]$ndvi_primary <- temp$ndvi$ndvi70
    ob[i, ]$ndvi_total <- temp$ndvi$ndvi95

    ob[i, ]$ndvi_high <- temp$ndvi$ndvi50
    ob[i, ]$ndvi_medium <- mean(ndvi[gDifference(temp$hr$hr70,
                                                 temp$hr$hr50), ]$ndvi,
                                na.rm = TRUE)
    ob[i, ]$ndvi_low <- mean(ndvi[gDifference(temp$hr$hr95,
                                              temp$hr$hr70), ]$ndvi,
                             na.rm = TRUE)
    # Add values to ud
    ud[[i]] <- temp$ud

    # Create sf-dataframe with homerange polygons
    hr_50_temp <- st_as_sf(temp$hr$hr50) %>%
      mutate(hr_type = "hr_50")
    hr_70_temp <- st_as_sf(temp$hr$hr70) %>%
      mutate(hr_type = "hr_70")
    hr_95_temp <- st_as_sf(temp$hr$hr95) %>%
      mutate(hr_type = "hr_95")

    hr_temp <- rbind(hr_50_temp, hr_70_temp, hr_95_temp) %>%
        mutate(id = ob[i,]$id, block_type = ob[i,]$block_type, nb_reloc = ob[i,]$nb_reloc,
             ints = rep(ob[i,]$ints, 3)) %>%
      select(-area)

    if(!exists("hr_full")){
      hr_full <- hr_temp
    } else {
      hr_full <- rbind(hr_full, hr_temp)
    }
  }
  # Set the timezone back to original value
  Sys.setenv(TZ = timezone.backup)

  # Return the specified object
  if(output == "ud"){
    return(ud)
  } else if(output == "ob"){
    ob$start <- int_start(ob$ints)
    ob$end <- int_end(ob$ints)
    ob$rowid <- as.numeric(rownames(ob))
    return(ob)
  } else if(output == "hr"){
    return(hr_full)
  }
}

#' Updated BRB code
#'
#' @param x definition
#' @param start.date definition
#' @param end.date definition
#' @param hab definition
#' @param iso definition
#' @param plot.it definition
#' @param t definition
#' @param vv definition
#'
#' @export
#' @examples
#'
area.BRB <- function(x = NULL, start.date = NULL, end.date = NULL, hab = NULL, ndvi = NULL, iso = 95, plot.it = FALSE, t = "UD", vv = NULL) {

  # Subset - Urs: ARE THE TWO FOLLOWING LINES NECESSARY?
  start.date <- as.POSIXct(start.date)
  end.date <- as.POSIXct(end.date)

  x.sub <- gdltraj(x, min = start.date, max = end.date,type = "POSIXct")

  # Calculate home range
  ud.BRB <- BRB(x.sub, D = vv, Tmax = 90*60,
                Lmin = 5, hmin = 50, type = t, filtershort = FALSE,
                habitat = hab,
                grid = hab,
                b = FALSE, extent = 1,
                maxt = 90*60, radius = 150)

  if(plot.it == TRUE){
    image(ud.BRB, col = topo.colors(100))
  }

  po <- list()
  n <- list()

  for(i in 1:length(iso)){
    # polygons
    po[[paste("hr", iso[i], sep = "")]] <- getverticeshr(ud.BRB,
                                                         percent = iso[i])
    proj4string(po[[i]]) <- CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

    # ndvi
    n[[paste("ndvi", iso[i], sep = "")]] <- mean(ndvi[po[[i]], drop = TRUE]$ndvi,
                                                 na.rm=TRUE)
  }

  return(list(hr = po, ndvi = n, ud = ud.BRB))
}
