##### This file estimates how far phones are from their home tract in each hourly window (Step 8) ---------
# Uses the same hourly windows as Step 6: for each window, the average distance (km) between each
# phone's ping and the center of its home tract, with pings farther from the window center counting less.
# Input:  data/mpt_home_by_cz/mpt_home_by_cz_<cz>.csv.gz, data/shape_files/tracts_2020
# Output: data/displacements/displacement_data_<cz>.csv.gz (one row per hourly window)
# Functions --------------------
# Upload packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Gaussian weight
gaus_weight <- function(x, mu = 0, sigma = 1) {
  return(exp(-((x - mu)^2 / (2 * sigma^2))))
}

# Upload packages --------------------
ipak(c("dplyr",
       "fs",
       "here",
       "sf",
       "stringr",
       "tidyr",
       "vroom",
       "Rcpp"))
rm(ipak)

here::i_am("code/08_displacement_distributions.R")

# C++ functions --------------------
sourceCpp(here("code",
               "haversine2.cpp")) # great-circle distance in km

# Home tract centroids --------------------
ct_shape <- st_read(here("data",
                         "shape_files",
                         "tracts_2020"),
                    layer = "US_tract_2020",
                    quiet = TRUE)
ct_centroid <- st_geometry(ct_shape) |>
  st_centroid() |>
  st_transform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") |>
  st_coordinates()
ct_centroid <- tibble(GISJOIN = ct_shape$GISJOIN,
                      hlon = ct_centroid[, 1],
                      hlat = ct_centroid[, 2])
rm(ct_shape)

# Settings --------------------
# CZs (from the file names)
czs <- fs::dir_ls(here("data",
                       "mpt_home_by_cz"),
                  glob = "*.csv.gz") |>
  basename() |>
  str_extract("[0-9]+")

# Same 428 hourly windows as Step 6
time_vec <- c(3:384,
              507:552)

# Data processing function by CZ ----------
process_function <- function(CZ) {

  # Upload data: keep pings under 24 km/h (6.67 m/s) with good vertical accuracy
  dat <- vroom(here("data",
                    "mpt_home_by_cz",
                    paste0("mpt_home_by_cz_",
                           CZ,
                           ".csv.gz")),
               delim = ",") |>
    filter(speed >= 0 & speed <= 6.67,
           vertical_accuracy < 20) |>
    select(device_id,
           home,
           day,
           hour,
           minute,
           second,
           latitude,
           longitude,
           cz2000,
           metro_name) |>
    tidyr::drop_na()

  # Distance from each ping to the phone's home tract center
  dat <- dat |>
    left_join(ct_centroid,
              by = join_by(home == GISJOIN))
  dat$dist <- haversine_distance(dat$latitude,
                                 dat$longitude,
                                 dat$hlat,
                                 dat$hlon)

  # Hours since the first ping
  dat <- dat |>
    mutate(time = (day * 24) + (hour) + (minute / 60) + (second / 3600))
  min_time <- min(dat$time)
  dat <- dat |>
    mutate(diff_min_time = time - min_time)

  # Calculate displacement for each time window
  res <- vector("list", length(time_vec))
  for (i in 1:length(time_vec)) {

    # Pings within 3 hours of the window center; keep each phone's ping closest to the center
    x <- dat |>
      filter((diff_min_time <= time_vec[i] + 3) & (diff_min_time >= time_vec[i] - 3)) |>
      mutate(time_from_window = abs(diff_min_time - time_vec[i])) |>
      group_by(device_id) |>
      arrange(time_from_window) |>
      slice(1) |>
      ungroup() |>
      arrange(diff_min_time)

    if (nrow(x) > 0) {
      # Label the window by the day and hour of its center
      center_time <- min_time + time_vec[i]
      center_day <- as.integer(floor(center_time / 24))
      center_hour <- as.integer(floor(center_time - (center_day * 24)))

      # Average distance from home, weighted by time from the window center
      res[[i]] <- tibble(displacement = weighted.mean(x$dist, gaus_weight(x$time_from_window)),
                         hour = center_hour,
                         cz2000 = x$cz2000[1],
                         metro_name = x$metro_name[1],
                         day = center_day)
    }
  }

  # Save results
  vroom_write(bind_rows(res),
              here("data",
                   "displacements",
                   paste0("displacement_data_",
                          CZ,
                          ".csv.gz")),
              delim = ",")
  message(paste0("----- Completed processing CZ ",
                 CZ,
                 " -----"))
}

# Run function --------------------
for (cz in czs) {
  process_function(cz)
}
