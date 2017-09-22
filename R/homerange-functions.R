#' Calculate habitat utilization from ranging data and topography (?) map
#'
#' Creates dataframe that contains various information on habitat use for each group and time interval\cr
#' spdf_50, spdf_70, spdf_95 contain objects of class 'SpatialPolygonDataFrame' (see package 'sp' for futher information)\cr
#' ud contains object of class 'estUD' (see 'adehabitatHR' for further information)\cr
#' Note: use intervals that start on first of each month when using with fruit biomass calculation (because of monthly phenology data)
#'
#' @param ranging_waypoint Table with ranging waypoint - MORE SPECIFICS
#' @param start_date Begin of first time interval. By default (NULL), Jan 1st of the earliest timestamp in ranging dataset - NOT YET IMPLEMENTED
#' @param ints_per_year Default is 1 (i.e. yearly homeranges). Set to NULL if not needed. Currently implemented:
#' \itemize{
#'  \item 12 (1 month-intervals)
#'  \item 8 (1.5 months-intervals)
#'  \item 4 (3 months-intervals)
#'  \item 2 (6 months-intervals)
#'  \item 1 (1 year-intervals)
#'  }
#' @param hr_periods If custom periods for home ranges are required (e.g. 05-02-2012 - 20-07-2012). Default is NULL.\cr
#'  Create object of type: data.frame(date_begin = start_vec, date_end = end_vec)\cr
#'  With start_vec including all start_dates and end_vec all end_dates\cr
#'  Note: 'ints_per_year' is ignored if both are specified
#' @param min_nb_reloc Minimum number of location points per interval. Intervals for which the data are too sparse are removed.
#'   Default (NULL) uses Fernando's criteria:
#'   \itemize{
#'    \item 96 pts (~8 full days) for monthly home range
#'    \item 192 pts (~16 full days) for quarterly home range
#'    \item 384 pts (~32 full days) for half-yearly home range
#'    \item 768 pts (~64 full days) for annual home range
#' }
#' @param dir_to_files Directory in which the necessary datafiles are included, e.g. 'paste0(getwd(), "/data")'
#' @export
#' @examples
#'
get_habitat_use <- function (ranging_waypoints, start_date = NULL, ints_per_year = 1,
                             hr_periods = NULL, min_nb_reloc = NULL,
                             dir_to_files = NULL){
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
    dplyr::select(block_type, id, nb_reloc, date_begin, date_end)
  
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
  # If min_nb_reloc is NULL, use Fernando's criteria
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
  lc <- rgdal::readGDAL(fname = paste0(dir_to_files, "/LC-2011-03-06.tif"))
  sp::fullgrid(lc) <- FALSE
  names(lc) <- "habitat"
  
  ndvi <- rgdal::readGDAL(fname = paste0(dir_to_files, "/NDVI-2011-03-06.tif"))
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
  ob <- ob %>%
    mutate(area_50 = NA,  # labelled as 'core' in original query
           area_70 = NA,  # labelled as 'primary' in original query
           area_95 = NA,  # labelled as 'total' in original query
           ndvi_50 = NA, ndvi_70 = NA, ndvi_95 = NA, ndvi_high = NA, ndvi_medium = NA, ndvi_low = NA,
           spdf_50 = NA, spdf_70 = NA, spdf_95 = NA,
           ud = NA)
  
  for(i in 1:nrow(ob)){
    cat("Calculating homerange ", i, "/", nrow(ob), '\r')
    temp <- suppressWarnings(area.BRB(
      x = ran[id = ob[i, ]$id],
      start.date = int_start(ob[i, ]$ints),
      end.date = int_end(ob[i, ]$ints) + days(1),
      hab = hab,
      ndvi = ndvi,
      iso = c(50, 70, 95),
      t = "UD",
      vv = vv[[ob[i, ]$id]]))
    
    ob[i,]$area_50 <- temp$hr$hr50$area
    ob[i,]$area_70 <- temp$hr$hr70$area
    ob[i,]$area_95 <- temp$hr$hr95$area
    ob[i,]$ndvi_50 <- temp$ndvi$ndvi50
    ob[i,]$ndvi_70 <- temp$ndvi$ndvi70
    ob[i,]$ndvi_95 <- temp$ndvi$ndvi95
    ob[i, ]$ndvi_high <- temp$ndvi$ndvi50
    ob[i, ]$ndvi_medium <- mean(ndvi[gDifference(temp$hr$hr70, temp$hr$hr50), ]$ndvi, na.rm = TRUE)
    ob[i, ]$ndvi_low <- mean(ndvi[gDifference(temp$hr$hr95, temp$hr$hr70), ]$ndvi, na.rm = TRUE)
    ob[i, ]$spdf_50 <- list(temp$hr$hr50)
    ob[i, ]$spdf_70 <- list(temp$hr$hr70)
    ob[i, ]$spdf_95 <- list(temp$hr$hr95)
    ob[i,]$ud <- list(temp$ud)
  }
  
  # Turn intervals into start and end value because intervals sometime cause problems (e.g. with gather)
  ob <- ob %>%
    mutate(start_date = int_start(ints),
           end_date = int_end(ints)) %>%
    select(id, block_type, start_date, end_date, nb_reloc, area_50, area_70, area_95, ndvi_50, ndvi_70, ndvi_95,
           ndvi_high, ndvi_medium, ndvi_low, spdf_50, spdf_70, spdf_95, ud)
  
  # Set the timezone back to original value
  Sys.setenv(TZ = timezone.backup)
  
  return(ob)
}

#' Updated BRB code
#'
#' For the original function, see adehabitatHR::BRB
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


#' Creates a 'simple feature' dataframe from the habitat utilization dataframe
#'
#' The returned object is a tidy (i.e. long) dataframe:\cr
#' One row per group, interval, and homerange-type (50, 70, and 95 percent)
#'
#' @param habitat_use_df The dataframe returned from get_habitat_use()
#'
#' @export
#' @examples
#'
get_sf_from_hu <- function(habitat_use_df){
  
  temp_df <- habitat_use_df %>%
    select(id, block_type, nb_reloc, start_date, end_date, spdf_50, spdf_70, spdf_95)
  
  temp_df_long <- temp_df %>%
    gather(hr_type, spdf, spdf_50, spdf_70, spdf_95) %>%
    mutate(hr_type = sub("spdf_", "", hr_type))
  
  for(i in 1:nrow(temp_df_long)){
    geom_i <- st_as_sf(temp_df_long[i,]$spdf[[1]]) %>%
      select(geometry)
    if(!exists("geoms")){
      geoms <- geom_i
    } else {
      geoms <- rbind(geoms, geom_i)
    }}
  
  temp_df_sf <- geoms %>%
    bind_cols(temp_df_long, .) %>%
    select(-spdf) %>%
    st_as_sf()
  
  return(temp_df_sf)
}
