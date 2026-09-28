##### This file prepares public tract shapes for the two example CZs (Step 0) ---------
# Already run: the output is in data/shape_files/tracts_2020. Rerunning it needs internet.
# Source: U.S. Census Bureau, 2020 cartographic boundary files for census tracts (public domain).
# Input:  data/fips_commutingzone_crosswalk.csv.gz (counties in each CZ)
# Output: data/shape_files/tracts_2020/US_tract_2020.shp
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
       "vroom"))
rm(ipak)

here::i_am("code/00_prepare_tract_shapes.R")

# Counties in the two CZs (USDA ERS 2000 commuting zones) --------------------
cz_crosswalk <- vroom(here("data",
                           "fips_commutingzone_crosswalk.csv.gz"),
                      delim = ",",
                      col_types = "cn")

# Download tract shapes --------------------
# One file for each state with a county in the CZs
states <- unique(substr(cz_crosswalk$fips, 1, 2))
tract_shape <- lapply(states, function(state) {
  file_name <- paste0("cb_2020_", state, "_tract_500k")
  download.file(paste0("https://www2.census.gov/geo/tiger/GENZ2020/shp/", file_name, ".zip"),
                file.path(tempdir(), paste0(file_name, ".zip")),
                mode = "wb")
  unzip(file.path(tempdir(), paste0(file_name, ".zip")),
        exdir = tempdir())
  st_read(file.path(tempdir(), paste0(file_name, ".shp")),
          quiet = TRUE)
}) |>
  bind_rows()

# Keep tracts in the CZs --------------------
# GISJOIN is the NHGIS tract ID (G + state + 0 + county + 0 + tract) used to join all tract data
tract_shape <- tract_shape |>
  mutate(fips = paste0(STATEFP, COUNTYFP),
         GISJOIN = paste0("G", STATEFP, "0", COUNTYFP, "0", TRACTCE)) |>
  inner_join(cz_crosswalk,
             by = join_by(fips)) |>
  select(GISJOIN,
         cz2000) |>
  st_transform(5070) # USA Contiguous Albers, the projection of the NHGIS shapes used in the paper

# check for errors
if(!any(duplicated(tract_shape$GISJOIN))) {
  message("Looks good to me!")
} else message("You made an error :(")

# Save shapefile --------------------
st_write(tract_shape,
         here("data",
              "shape_files",
              "tracts_2020",
              "US_tract_2020.shp"),
         delete_layer = TRUE)
