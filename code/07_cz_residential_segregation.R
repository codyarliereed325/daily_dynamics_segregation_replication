##### This file estimates residential segregation for each CZ (Step 7) ---------
# The baseline for the segregation ratio (Step 10): the same Theil index computed on census tracts,
# where each tract's local mix is its own population plus nearby tracts' populations
# (Gaussian weights with a 1 km scale on tract centroids).
# Input:  data/czdata_homect/cz_home_ct_cznames.csv.gz (tracts with pings), data/census_sim/census_2020_tract_sim.csv.gz,
#         data/shape_files/tracts_2020, data/cz_names.csv.gz
# Output: data/residential_spat_seg.csv.gz
# Functions --------------------
# Upload packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Upload packages --------------------
ipak(c("dplyr",
       "here",
       "sf",
       "vroom",
       "Rcpp",
       "RcppArmadillo"))
rm(ipak)

here::i_am("code/07_cz_residential_segregation.R")

# C++ functions --------------------
sourceCpp(here("code",
               "main_function_tracts.cpp")) # spatial weighting of tracts (each tract counts itself fully)
sourceCpp(here("code",
               "theil_index.cpp"))
sourceCpp(here("code",
               "theil_multi.cpp"))

# Upload data --------------------
# Tracts with at least one ping in each CZ
data <- vroom(here("data",
                   "czdata_homect",
                   "cz_home_ct_cznames.csv.gz"),
              delim = ",") |>
  distinct(GISJOIN,
           cz2000) |>
  left_join(vroom(here("data",
                       "cz_names.csv.gz"),
                  delim = ","),
            by = join_by(cz2000))

# Tract population by race and ethnicity
data <- vroom(here("data",
                   "census_sim",
                   "census_2020_tract_sim.csv.gz"),
              delim = ",") |>
  mutate(white = U7L003,
         black = U7L004,
         hisp = U7L010,
         asian = U7L006,
         oth = U7L009 + U7L008 + U7L007 + U7L005) |>
  select(GISJOIN,
         white:oth) |>
  right_join(data,
             by = join_by(GISJOIN))

# Tract centroids (computed in a projected coordinate system, as in the paper)
tract_shape <- st_read(here("data",
                            "shape_files",
                            "tracts_2020"),
                       layer = "US_tract_2020",
                       quiet = TRUE) |>
  filter(GISJOIN %in% data$GISJOIN)
centroids <- st_geometry(tract_shape) |>
  st_transform("+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0") |>
  st_centroid() |>
  st_transform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") |>
  st_coordinates()
data <- data |>
  left_join(tibble(GISJOIN = tract_shape$GISJOIN,
                   latitude = centroids[, 2],
                   longitude = centroids[, 1]),
            by = join_by(GISJOIN))
rm(tract_shape,
   centroids)

# Residential segregation --------------------
res <- list()
for (cz in unique(data$metro_name)) {
  x <- data |>
    filter(metro_name == cz)

  # Local racial mix around each tract (tracts have no altitude or time, so both are 0)
  mat <- adjustRacialFrequenciesTracts(x$latitude,
                                       x$longitude,
                                       rep(0, nrow(x)),
                                       rep(0, nrow(x)),
                                       as.matrix(x[, c("white",
                                                       "black",
                                                       "hisp",
                                                       "asian",
                                                       "oth")]),
                                       distSigma = 1,
                                       timeSigma = 1)

  # Theil index for each group and for all groups together
  seg <- round(theil_index(mat), 4)
  res[[cz]] <- tibble(white = seg[1],
                      black = seg[2],
                      hisp = seg[3],
                      asian = seg[4],
                      oth = seg[5],
                      multiH = round(theil_index_multi(mat), 4),
                      cz_name = cz)
}

# Save results --------------------
vroom_write(bind_rows(res),
            here("data",
                 "residential_spat_seg.csv.gz"),
            delim = ",")
