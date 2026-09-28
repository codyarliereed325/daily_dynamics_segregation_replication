##### This file estimates segregation for each hourly time window (Step 6) ---------
# For each hour, every phone seen within three hours is compared with every other phone. Phones
# that are closer in space and time count more (Gaussian weights with a 1 km and 1 hour scale).
# Each phone carries its home tract's population by race, so every phone gets a local racial
# mix. The Theil index (H) then measures how much these local mixes differ from the CZ as a
# whole: 0 = every local mix looks like the CZ, 1 = groups never share space.
# Input:  data/mpt_home_by_cz/mpt_home_by_cz_<cz>.csv.gz
# Output: data/seg_results_data/seg_data_<cz>.csv.gz (one row per hourly window)
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
ipak(c("fs",
       "here",
       "stringr",
       "tidytable",
       "vroom",
       "Rcpp",
       "RcppArmadillo",
       "RcppParallel"))
rm(ipak)

here::i_am("code/06_calculate_segregation.R")

# C++ functions --------------------
sourceCpp(here("code",
               "main_function.cpp")) # space-time weighting of all other phones
sourceCpp(here("code",
               "theil_index.cpp"))   # Theil index for each group versus everyone else
sourceCpp(here("code",
               "theil_multi.cpp"))   # multigroup Theil index

# Settings --------------------
# CZs (from the file names)
czs <- fs::dir_ls(here("data",
                       "mpt_home_by_cz"),
                  glob = "*.csv.gz") |>
  basename() |>
  str_extract("[0-9]+")

# Window centers in hours after the first ping. The data skip January 20-24,
# so there are two blocks of windows: 428 hourly windows per CZ.
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
           day,
           hour,
           minute,
           second,
           latitude,
           longitude,
           altitude,
           cz2000,
           white,
           black,
           hisp,
           asian,
           oth,
           freq_tract,
           adjusted_freq_tract) |>
    drop_na()

  # Weight observations: each phone carries its home tract's population by group, divided by the
  # adjusted number of phones from that tract. Tracts with few phones are down-weighted
  # (1 phone: 5%, 2: 39%, 3: 75%, 5 or more: about 100%).
  dat <- dat |>
    mutate(across(white:oth, ~ (.x / (adjusted_freq_tract)) * (1 - gaus_weight(freq_tract, mu = 0.5, 1.5))))

  # Hours since the first ping
  dat <- dat |>
    mutate(time = (day * 24) + (hour) + (minute / 60) + (second / 3600))
  min_time <- min(dat$time)
  dat <- dat |>
    mutate(diff_min_time = time - min_time)

  # Calculate segregation for each time window
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
      # Local racial mix around each phone: all other phones, weighted by distance (1 km) and time (1 hour)
      mat <- adjustRacialFrequencies(x$latitude,
                                     x$longitude,
                                     x$altitude,
                                     x$diff_min_time,
                                     as.matrix(x[, c("white",
                                                     "black",
                                                     "hisp",
                                                     "asian",
                                                     "oth")]),
                                     distSigma = 1,
                                     timeSigma = 1)

      # Phones seen farther from the window center count less
      mat <- mat * gaus_weight(x$time_from_window)

      # Label the window by the day and hour of its center
      center_time <- min_time + time_vec[i]
      center_day <- as.integer(floor(center_time / 24))
      center_hour <- as.integer(floor(center_time - (center_day * 24)))

      # Theil index for each group (versus everyone else) and for all groups together
      seg <- theil_index(mat)
      res[[i]] <- data.frame(Hwhite = seg[1],
                             Hblack = seg[2],
                             Hhisp = seg[3],
                             Hasian = seg[4],
                             Hoth = seg[5],
                             Htotal = theil_index_multi(mat),
                             day = center_day,
                             hour = center_hour,
                             cz2000 = x$cz2000[1],
                             obs = nrow(x))

      # Free memory (the weight matrix has one row and column per phone)
      rm(x,
         mat,
         seg)
      gc()
    }
    if (i %% 50 == 0) {
      message(paste0("Completed window ",
                     i,
                     " of ",
                     length(time_vec),
                     " for CZ ",
                     CZ))
    }
  }

  # Save results
  vroom_write(bind_rows(res),
              here("data",
                   "seg_results_data",
                   paste0("seg_data_",
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
