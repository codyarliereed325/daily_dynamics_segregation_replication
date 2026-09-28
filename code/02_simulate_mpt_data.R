##### This file simulates cleaned mobile phone data for the two example CZs (Step 2) ---------
# Every device and every ping is SIMULATED. No real phone data are used.
# Devices live in tracts drawn from the simulated population and follow simple daily routines:
# home at night, work on weekdays, and outings in the evening and on weekends. Each device pings
# a few times a day. The output has the structure of the paper's cleaned data: one row per ping,
# each device's home tract already assigned, and only the columns the analysis uses.
# Input:  data/shape_files/tracts_2020, data/census_sim/census_2020_tract_sim.csv.gz, data/cz_names.csv.gz
# Output: data/czdata_homect/cz_home_ct_cznames.csv.gz, figures/simulated_pings_*.png
# Functions --------------------
# Upload packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Pick destination tracts: tracts that are closer and more attractive are more likely
choose_destination <- function(origin, tracts, attraction, scale_km) {
  idx <- match(origin, tracts$GISJOIN)
  destination <- character(length(origin))
  for (o in unique(idx)) {
    rows <- which(idx == o)
    dist <- sqrt((tracts$x - tracts$x[o])^2 + (tracts$y - tracts$y[o])^2)
    destination[rows] <- sample(tracts$GISJOIN,
                                length(rows),
                                replace = TRUE,
                                prob = attraction * exp(-dist / scale_km))
  }
  destination
}

# One random point inside each requested tract (returns longitude and latitude)
# Draws points in each tract's bounding box and keeps the ones that fall inside the tract
random_points <- function(gisjoin, tract_shape) {
  idx <- match(gisjoin, tract_shape$GISJOIN)
  bbox <- do.call(rbind, lapply(st_geometry(tract_shape), st_bbox))
  xy <- matrix(NA, nrow = length(gisjoin), ncol = 2)
  todo <- seq_along(gisjoin)
  while (length(todo) > 0) {
    b <- bbox[idx[todo], , drop = FALSE]
    candidate <- cbind(runif(length(todo), b[, "xmin"], b[, "xmax"]),
                       runif(length(todo), b[, "ymin"], b[, "ymax"]))
    hits <- st_intersects(st_as_sf(as.data.frame(candidate),
                                   coords = c("V1", "V2"),
                                   crs = st_crs(tract_shape)),
                          tract_shape)
    inside <- mapply(function(hit, i) i %in% hit, hits, idx[todo])
    xy[todo[inside], ] <- candidate[inside, ]
    todo <- todo[!inside]
  }
  st_as_sf(as.data.frame(xy),
           coords = c("V1", "V2"),
           crs = st_crs(tract_shape)) |>
    st_transform(4326) |>
    st_coordinates()
}

# Upload packages --------------------
ipak(c("dplyr",
       "ggplot2",
       "here",
       "sf",
       "tidyr",
       "vroom"))
rm(ipak)

here::i_am("code/02_simulate_mpt_data.R")

# Settings (all made up) --------------------
set.seed(20200104)

# CZs: number of devices, UTC offset in January, downtown location, and share of places above ground
cz_settings <- tibble(cz2000 = c(58, 134),
                      n_devices = c(3000, 4000),
                      utc_offset = c(-6, -5),
                      downtown_lon = c(-87.6298, -73.9855), # the Loop; Midtown Manhattan
                      downtown_lat = c(41.8781, 40.7580),
                      high_rise = c(0.20, 0.35)) |>
  left_join(vroom(here("data",
                       "cz_names.csv.gz"),
                  delim = ","),
            by = join_by(cz2000))

# Local days in the paper's calendar (the data cover UTC days January 4-19 and 25-26, 2020)
days <- c(3:19, 24:26)

# Relative ping frequency by hour of day (0-23)
hour_weights <- c(0.45, 0.35, 0.30, 0.30, 0.30, 0.40, 0.65, 0.90, 1.00, 1.05, 1.10, 1.15,
                  1.20, 1.20, 1.15, 1.15, 1.20, 1.25, 1.25, 1.20, 1.10, 0.95, 0.80, 0.60)

# Phones per resident fall with the percent of Black and Hispanic residents in a tract
# (the pattern the paper's negative binomial weighting corrects)
coverage_beta <- c(black = -0.006,
                   hisp = -0.004,
                   asian = 0.002)

# Upload data --------------------
# Tract shapes
tract_shape <- st_read(here("data",
                            "shape_files",
                            "tracts_2020"),
                       layer = "US_tract_2020",
                       quiet = TRUE)

# Simulated census data: population and percent of each group
cen_dat <- vroom(here("data",
                      "census_sim",
                      "census_2020_tract_sim.csv.gz"),
                 delim = ",") |>
  transmute(GISJOIN,
            total = U7S001,
            black = (U7L004 / U7S001) * 100,
            hisp = (U7L010 / U7S001) * 100,
            asian = (U7L006 / U7S001) * 100)

# Tract centroids in km
centroids <- st_coordinates(st_centroid(st_geometry(tract_shape))) / 1000
tract_dat <- tibble(GISJOIN = tract_shape$GISJOIN,
                    cz2000 = tract_shape$cz2000,
                    x = centroids[, 1],
                    y = centroids[, 2]) |>
  left_join(cen_dat,
            by = join_by(GISJOIN))
rm(centroids,
   cen_dat)

# Downtown locations in km
downtown <- st_as_sf(cz_settings,
                     coords = c("downtown_lon", "downtown_lat"),
                     crs = 4326) |>
  st_transform(5070) |>
  st_coordinates()
cz_settings$downtown_x <- downtown[, 1] / 1000
cz_settings$downtown_y <- downtown[, 2] / 1000
rm(downtown)

# Simulation function by CZ ----------
simulate_cz <- function(k) {
  cz <- cz_settings[k, ]
  tracts <- tract_dat |>
    filter(cz2000 == cz$cz2000)

  # Jobs concentrate downtown; outings are more spread out
  downtown_dist <- sqrt((tracts$x - cz$downtown_x)^2 + (tracts$y - cz$downtown_y)^2)
  work_attraction <- tracts$total * (1 + 15 * exp(-downtown_dist^2 / (2 * 2.5^2)))
  leisure_attraction <- tracts$total * (1 + 5 * exp(-downtown_dist^2 / (2 * 4^2)))

  # Devices --------------------
  # Home tracts follow population, times a coverage factor that varies from tract to tract
  coverage <- exp(coverage_beta["black"] * tracts$black +
                    coverage_beta["hisp"] * tracts$hisp +
                    coverage_beta["asian"] * tracts$asian) *
    rgamma(nrow(tracts), shape = 0.5, rate = 0.5)
  device_dat <- tibble(device_n = sum(cz_settings$n_devices[seq_len(k - 1)]) + seq_len(cz$n_devices),
                       home = sample(tracts$GISJOIN,
                                     cz$n_devices,
                                     replace = TRUE,
                                     prob = tracts$total * coverage),
                       worker = runif(cz$n_devices) < 0.6,
                       activity = rgamma(cz$n_devices, shape = 2, rate = 2)) # some devices ping more
  device_dat$work <- choose_destination(device_dat$home,
                                        tracts,
                                        work_attraction,
                                        scale_km = 10)

  # Daily schedules (hours after midnight) --------------------
  day_dat <- device_dat |>
    tidyr::crossing(day = days) |>
    mutate(day_of_week = weekdays(as.Date(paste0("2020-01-", day))),

           # Devices are seen on about two-thirds of days
           active = runif(n()) < 0.65,

           # Workers work on weekdays and some Saturdays
           at_work = worker & (!(day_of_week %in% c("Saturday", "Sunday")) |
                                 (day_of_week == "Saturday" & runif(n()) < 0.2)),
           work_start = pmin(pmax(rnorm(n(), 8, 1), 5), 11),
           work_end = pmin(pmax(rnorm(n(), 17.5, 1), work_start + 4), 22),

           # Daytime trips (errands, visits), mostly on weekends and for non-workers
           day_trip = runif(n()) < case_when(day_of_week == "Saturday" ~ 0.60,
                                              day_of_week == "Sunday" ~ 0.45,
                                              !at_work ~ 0.50,
                                              TRUE ~ 0),
           day_trip_start = runif(n(), 10, 15),
           day_trip_end = day_trip_start + runif(n(), 1, 3),

           # Evening outings, most common on Friday and Saturday
           evening = runif(n()) < case_when(day_of_week == "Friday" ~ 0.35,
                                             day_of_week == "Saturday" ~ 0.40,
                                             day_of_week == "Sunday" ~ 0.10,
                                             TRUE ~ 0.12),
           evening_start = runif(n(), 18.5, 21),
           evening_end = pmin(evening_start + runif(n(), 1.5, 4), 24)) |>
    filter(active) |>
    mutate(row_id = row_number())

  # Where the day trips and evening outings go
  day_dat$day_trip_tract <- NA_character_
  day_dat$day_trip_tract[day_dat$day_trip] <- choose_destination(day_dat$home[day_dat$day_trip],
                                                                 tracts,
                                                                 leisure_attraction,
                                                                 scale_km = 4)
  day_dat$evening_tract <- NA_character_
  day_dat$evening_tract[day_dat$evening] <- choose_destination(day_dat$home[day_dat$evening],
                                                               tracts,
                                                               leisure_attraction,
                                                               scale_km = 7)

  # Places: one random point for each home, workplace, day trip, and evening outing --------------------
  places <- bind_rows(tibble(key = paste0("home_", device_dat$device_n),
                             GISJOIN = device_dat$home),
                      tibble(key = paste0("work_", device_dat$device_n[device_dat$worker]),
                             GISJOIN = device_dat$work[device_dat$worker]),
                      tibble(key = paste0("trip_", day_dat$row_id[day_dat$day_trip]),
                             GISJOIN = day_dat$day_trip_tract[day_dat$day_trip]),
                      tibble(key = paste0("evening_", day_dat$row_id[day_dat$evening]),
                             GISJOIN = day_dat$evening_tract[day_dat$evening]))
  lonlat <- random_points(places$GISJOIN,
                          tract_shape)
  places <- places |>
    mutate(place_lon = lonlat[, 1],
           place_lat = lonlat[, 2],
           place_alt = ifelse(runif(n()) < cz$high_rise,
                              (1 + rgeom(n(), 1 / 6)) * 3.2, # floors above ground (meters)
                              runif(n(), 0, 12)))

  # Pings --------------------
  ping_dat <- day_dat |>

    # A few pings per active day, at random times (more often during the day)
    mutate(n_pings = rpois(n(), 3.6 * activity)) |>
    tidyr::uncount(n_pings) |>
    mutate(hour = sample(0:23, n(), replace = TRUE, prob = hour_weights),
           minute = sample(0:59, n(), replace = TRUE),
           second = sample(0:59, n(), replace = TRUE),
           time = hour + minute / 60 + second / 3600,

           # Where the device is at that time
           key = case_when(evening & time >= evening_start & time <= evening_end ~ paste0("evening_", row_id),
                           day_trip & time >= day_trip_start & time <= day_trip_end ~ paste0("trip_", row_id),
                           at_work & time >= work_start & time <= work_end ~ paste0("work_", device_n),
                           TRUE ~ paste0("home_", device_n)),

           # Keep pings inside the data's UTC coverage (January 4-19 and 25-26)
           utc_hours = day * 24 + time - cz$utc_offset) |>
    filter((utc_hours >= 4 * 24 & utc_hours < 20 * 24) |
             (utc_hours >= 25 * 24 & utc_hours < 27 * 24)) |>
    left_join(places,
              by = join_by(key)) |>

    # GPS noise, speed, and vertical accuracy (a few pings fail the paper's filters)
    mutate(moving = runif(n()) < 0.05,
           noise_m = ifelse(moving, 300, 15),
           latitude = round(place_lat + rnorm(n(), sd = noise_m / 111320), 6),
           longitude = round(place_lon + rnorm(n(), sd = noise_m / (111320 * cos(place_lat * pi / 180))), 6),
           altitude = round(place_alt + rnorm(n(), sd = 3), 1),
           speed_bin = sample(1:5, n(), replace = TRUE, prob = c(0.83, 0.09, 0.03, 0.02, 0.03)),
           speed = ifelse(moving,
                          runif(n(), 7, 25),
                          (c(0, 1, 5, 10, 15)[speed_bin] + runif(n()) * c(1, 4, 5, 5, 9)[speed_bin]) / 3.6),
           speed = round(speed, 2),
           vertical_accuracy = round(ifelse(runif(n()) < 0.03,
                                            runif(n(), 20, 80),
                                            abs(rnorm(n(), 0, 4)) + 1), 1),
           night_at_home = key == paste0("home_", device_n) & (hour >= 20 | hour <= 5)) |>

    # Keep devices with at least 3 nighttime pings at home, as the paper's home detection requires
    group_by(device_n) |>
    filter(sum(night_at_home) >= 3) |>
    ungroup() |>
    transmute(device_id = sprintf("SYN-%06d", device_n),
              home,
              altitude,
              vertical_accuracy,
              speed,
              day,
              hour,
              minute,
              second,
              latitude,
              longitude,
              GISJOIN,
              cz2000 = cz$cz2000,
              metro_name = cz$metro_name)

  message(paste0("----- Simulated CZ ",
                 cz$metro_name,
                 ": ",
                 nrow(ping_dat),
                 " pings from ",
                 n_distinct(ping_dat$device_id),
                 " devices -----"))
  ping_dat
}

# Run function --------------------
mpt_dat <- lapply(seq_len(nrow(cz_settings)),
                  simulate_cz) |>
  bind_rows() |>
  arrange(cz2000,
          device_id,
          day,
          hour,
          minute,
          second)

# check for errors
if(all(mpt_dat$GISJOIN %in% tract_dat$GISJOIN) && all(mpt_dat$home %in% tract_dat$GISJOIN) &&
   !anyNA(mpt_dat)) {
  message("Looks good to me!")
} else message("You made an error :(")

# Save data --------------------
vroom_write(mpt_dat,
            here("data",
                 "czdata_homect",
                 "cz_home_ct_cznames.csv.gz"),
            delim = ",")

# Quick look at the simulated data --------------------
# Map: weekday pings at 3 AM and at noon in Chicago
map_dat <- mpt_dat |>
  filter(cz2000 == 58,
         hour %in% c(3, 12),
         day %in% c(6:9, 13:16)) |>
  mutate(time = factor(ifelse(hour == 3, "3 AM", "12 PM"),
                       levels = c("3 AM", "12 PM")))

map_plot <- ggplot() +
  geom_sf(data = st_transform(filter(tract_shape, cz2000 == 58), 4326),
          fill = NA,
          color = "grey85",
          linewidth = 0.1) +
  geom_point(data = map_dat,
             aes(x = longitude, y = latitude),
             size = 0.3,
             alpha = 0.4) +
  facet_wrap(~ time) +
  theme_bw(base_size = 12) +
  theme(text = element_text(family = "Times"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank()) +
  labs(x = NULL,
       y = NULL,
       caption = "Simulated data")

ggsave(here("figures",
            "simulated_pings_map.png"),
       plot = map_plot,
       width = 8,
       height = 5,
       units = "in",
       dpi = 300)

# Pings by hour of day (average over the days observed at that hour)
hour_plot <- mpt_dat |>
  mutate(day_type = ifelse(weekdays(as.Date(paste0("2020-01-", day))) %in% c("Saturday", "Sunday"),
                           "Weekend",
                           "Weekday")) |>
  count(metro_name, day_type, day, hour) |>
  group_by(metro_name, day_type, hour) |>
  summarise(n = mean(n),
            .groups = "drop") |>
  ggplot(aes(x = hour, y = n, color = day_type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ metro_name) +
  theme_bw(base_size = 12) +
  theme(text = element_text(family = "Times"),
        legend.position = "bottom",
        strip.background = element_blank()) +
  labs(x = "Hour of Day",
       y = "Average Pings per Hour",
       color = NULL,
       caption = "Simulated data")

ggsave(here("figures",
            "simulated_pings_by_hour.png"),
       plot = hour_plot,
       width = 8,
       height = 4,
       units = "in",
       dpi = 300)
