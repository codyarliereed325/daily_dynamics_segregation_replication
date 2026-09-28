##### This file simulates tract demographics for the two example CZs (Step 1) ---------
# Every number here is SIMULATED from made-up parameters. None of it is real Census or ACS data.
# The columns use the NHGIS codes of the tables used in the paper, so the analysis code reads
# these files exactly as it read the real extract:
#   2020 Census:          U7L = table P5 (race and ethnicity), U7S = table P12 (sex by age)
#   ACS 2016-2020 5-year: AMRZ = table B15003 (education, 25+), AMR7 = table B19001 (household income)
# Input:  data/shape_files/tracts_2020
# Output: data/census_sim/census_2020_tract_sim.csv.gz, data/census_sim/acs_20205_tract_sim.csv.gz
# Functions --------------------
# Upload packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Smooth random surface over tracts: bell-shaped bumps centered on a few random tracts
random_surface <- function(x, y, n_seeds, scale_km) {
  seeds <- sample(length(x), n_seeds)
  rowSums(sapply(seeds, function(s) exp(-((x - x[s])^2 + (y - y[s])^2) / (2 * scale_km^2))))
}

# Split each tract's total across categories, using that tract's row of shares
draw_counts <- function(size, shares) {
  counts <- t(sapply(seq_along(size), function(i) rmultinom(1, size[i], shares[i, ])))
  colnames(counts) <- colnames(shares)
  counts
}

# The same shares for every tract
share_matrix <- function(shares, n) {
  matrix(shares / sum(shares), nrow = n, ncol = length(shares), byrow = TRUE)
}

# Upload packages --------------------
ipak(c("dplyr",
       "here",
       "sf",
       "vroom"))
rm(ipak)

here::i_am("code/01_simulate_census_data.R")

# Settings --------------------
set.seed(20200104)

# Tract centroids --------------------
tract_shape <- st_read(here("data",
                            "shape_files",
                            "tracts_2020"),
                       layer = "US_tract_2020",
                       quiet = TRUE)
centroids <- st_coordinates(st_centroid(st_geometry(tract_shape))) / 1000 # km
tract_dat <- tibble(GISJOIN = tract_shape$GISJOIN,
                    cz2000 = tract_shape$cz2000,
                    x = centroids[, 1],
                    y = centroids[, 2])
n_tracts <- nrow(tract_dat)
rm(tract_shape,
   centroids)

# Population --------------------
tract_dat$total <- pmax(round(rlnorm(n_tracts, meanlog = log(4000), sdlog = 0.35)),
                        200)

# Race and ethnicity (table P5) --------------------
# Each group clusters around a few random neighborhoods in each CZ, so the CZs are segregated
race_share <- matrix(NA,
                     nrow = n_tracts,
                     ncol = 5,
                     dimnames = list(NULL, c("white", "black", "hisp", "asian", "oth")))
for (cz in unique(tract_dat$cz2000)) {
  idx <- which(tract_dat$cz2000 == cz)
  x <- tract_dat$x[idx]
  y <- tract_dat$y[idx]
  eta <- cbind(white = 0,
               black = -2.0 + 4.0 * random_surface(x, y, n_seeds = 5, scale_km = 4),
               hisp = -1.5 + 3.5 * random_surface(x, y, n_seeds = 5, scale_km = 4),
               asian = -2.5 + 3.0 * random_surface(x, y, n_seeds = 4, scale_km = 3),
               oth = -3.0) +
    matrix(rnorm(length(idx) * 5, sd = 0.3), ncol = 5)
  race_share[idx, ] <- exp(eta) / rowSums(exp(eta))
}
race_counts <- draw_counts(tract_dat$total, race_share)

# The paper's "other" group combines four P5 categories
oth_counts <- draw_counts(race_counts[, "oth"],
                          share_matrix(c(0.08, 0.02, 0.15, 0.75), n_tracts))

census_dat <- tibble(GISJOIN = tract_dat$GISJOIN,
                     U7S001 = tract_dat$total,        # Total population
                     U7L003 = race_counts[, "white"], # Not Hispanic: White alone
                     U7L004 = race_counts[, "black"], # Not Hispanic: Black alone
                     U7L005 = oth_counts[, 1],        # Not Hispanic: American Indian and Alaska Native alone
                     U7L006 = race_counts[, "asian"], # Not Hispanic: Asian alone
                     U7L007 = oth_counts[, 2],        # Not Hispanic: Native Hawaiian and Pacific Islander alone
                     U7L008 = oth_counts[, 3],        # Not Hispanic: Some Other Race alone
                     U7L009 = oth_counts[, 4],        # Not Hispanic: Two or More Races
                     U7L010 = race_counts[, "hisp"])  # Hispanic or Latino

# Sex by age (table P12) --------------------
# 23 age groups (under 5, 5-9, ..., 85 and over); shares vary a little from tract to tract
age_shares <- c(0.057, 0.060, 0.064, 0.039, 0.027, 0.014, 0.014, 0.040, 0.069, 0.069, 0.066, 0.062,
                0.061, 0.063, 0.066, 0.026, 0.038, 0.023, 0.032, 0.045, 0.030, 0.019, 0.020)
age_share <- matrix(rgamma(n_tracts * 23,
                           shape = rep(200 * age_shares / sum(age_shares), each = n_tracts)),
                    nrow = n_tracts)
age_share <- age_share / rowSums(age_share)

female <- rbinom(n_tracts, tract_dat$total, 0.51)
male_age <- draw_counts(tract_dat$total - female, age_share)
female_age <- draw_counts(female, age_share)
colnames(male_age) <- paste0("U7S", sprintf("%03d", 3:25))   # Male: under 5 to 85 and over
colnames(female_age) <- paste0("U7S", sprintf("%03d", 27:49)) # Female: under 5 to 85 and over

census_dat <- bind_cols(census_dat,
                        as_tibble(male_age),
                        U7S026 = female,                      # Female total
                        as_tibble(female_age))

# Socioeconomic status --------------------
# A separate random surface, independent of race, that shifts education and income
ses <- numeric(n_tracts)
for (cz in unique(tract_dat$cz2000)) {
  idx <- which(tract_dat$cz2000 == cz)
  x <- tract_dat$x[idx]
  y <- tract_dat$y[idx]
  ses[idx] <- scale(random_surface(x, y, n_seeds = 6, scale_km = 5) -
                      random_surface(x, y, n_seeds = 6, scale_km = 5) +
                      rnorm(length(idx), sd = 0.3))
}

# Education, population 25 and over (table B15003) --------------------
pop_25plus <- rowSums(male_age[, 9:23]) + rowSums(female_age[, 9:23])
eta <- cbind(hs = 0,
             some_college = 0.1 + 0.2 * ses,
             assoc = -0.9 + 0.2 * ses,
             bach = -0.3 + 0.8 * ses,
             advanced = -1.0 + 1.0 * ses)
educ_counts <- draw_counts(pop_25plus, exp(eta) / rowSums(exp(eta)))

# Detailed categories within each group
hs_counts <- draw_counts(educ_counts[, "hs"],                     # no schooling through GED (17 categories)
                         share_matrix(c(0.030, 0.001, 0.001, 0.002, 0.003, 0.004, 0.004, 0.006, 0.012,
                                        0.006, 0.015, 0.020, 0.025, 0.030, 0.030, 0.700, 0.111), n_tracts))
some_college_counts <- draw_counts(educ_counts[, "some_college"], # less than 1 year, 1 year or more
                                   share_matrix(c(0.35, 0.65), n_tracts))
advanced_counts <- draw_counts(educ_counts[, "advanced"],         # master's, professional, doctorate
                               share_matrix(c(0.70, 0.15, 0.15), n_tracts))

acs_dat <- tibble(GISJOIN = tract_dat$GISJOIN,
                  AMRZE001 = pop_25plus)
acs_dat[paste0("AMRZE", sprintf("%03d", 2:18))] <- as.data.frame(hs_counts)
acs_dat[c("AMRZE019", "AMRZE020")] <- as.data.frame(some_college_counts)
acs_dat$AMRZE021 <- educ_counts[, "assoc"]
acs_dat$AMRZE022 <- educ_counts[, "bach"]
acs_dat[c("AMRZE023", "AMRZE024", "AMRZE025")] <- as.data.frame(advanced_counts)

# Household income (table B19001) --------------------
# 16 income brackets; incomes are log-normal around a tract median that rises with SES
households <- round(tract_dat$total / 2.6)
edges <- c(0, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 75, 100, 125, 150, 200, Inf) * 1000
inc_share <- t(sapply(70000 * exp(0.45 * ses),
                      function(m) diff(plnorm(edges, meanlog = log(m), sdlog = 0.85))))
inc_counts <- draw_counts(households, inc_share)

acs_dat$AMR7E001 <- households
acs_dat[paste0("AMR7E", sprintf("%03d", 2:17))] <- as.data.frame(inc_counts)

# check for errors: categories add up to their totals
if(all(rowSums(select(census_dat, U7L003:U7L010)) == census_dat$U7S001) &&
   all(rowSums(select(census_dat, U7S003:U7S049, -U7S026)) == census_dat$U7S001) &&
   all(rowSums(select(acs_dat, AMRZE002:AMRZE025)) == acs_dat$AMRZE001) &&
   all(rowSums(select(acs_dat, AMR7E002:AMR7E017)) == acs_dat$AMR7E001)) {
  message("Looks good to me!")
} else message("You made an error :(")

# Save data --------------------
vroom_write(census_dat,
            here("data",
                 "census_sim",
                 "census_2020_tract_sim.csv.gz"),
            delim = ",")
vroom_write(acs_dat,
            here("data",
                 "census_sim",
                 "acs_20205_tract_sim.csv.gz"),
            delim = ",")
