##### This file attaches home-tract race counts to the phone data and saves one file per CZ (Step 5) ---------
# Each phone is given the population counts of its home tract by race and ethnicity, and the
# number of phones living in that tract. Step 6 turns these into the weights each phone carries.
# Input:  data/czdata_homect/cz_home_ct_cznames_adjusted_obs.csv.gz, data/census_sim/census_2020_tract_sim.csv.gz
# Output: data/mpt_home_by_cz/mpt_home_by_cz_<cz>.csv.gz
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
       "vroom"))
rm(ipak)

here::i_am("code/05_attaching_census_data_to_mpt_data.R")

# Upload data --------------------
# Mobile phone data with adjusted phone counts (Step 4)
mpt_dat <- vroom(here("data",
                      "czdata_homect",
                      "cz_home_ct_cznames_adjusted_obs.csv.gz"),
                 delim = ",") |>
  filter(!is.na(home))

# Census data: population by race and ethnicity
cen_dat <- vroom(here("data",
                      "census_sim",
                      "census_2020_tract_sim.csv.gz"),
                 delim = ",") |>
  mutate(total = U7S001,
         white = U7L003,
         black = U7L004,
         hisp = U7L010,
         asian = U7L006,
         oth = U7L009 + U7L008 + U7L007 + U7L005) |>
  select(GISJOIN,
         total,
         white:oth)

# check for errors
if(all(with(cen_dat, white + black + hisp + asian + oth == total))) {
  message("Looks good to me!")
} else message("You made an error :(")

# Save one file per CZ --------------------
for (cz in unique(mpt_dat$cz2000)) {
  cz_dat <- mpt_dat |>
    filter(cz2000 == cz)

  # Number of phones (unique devices) living in each home tract of this CZ
  freq_tract_dat <- cz_dat |>
    distinct(device_id,
             home) |>
    count(home,
          name = "freq_tract")

  # Attach the home tract's phone count and population by race to every ping
  cz_dat <- cz_dat |>
    left_join(freq_tract_dat,
              by = join_by(home)) |>
    left_join(select(cen_dat, home = GISJOIN, white:oth),
              by = join_by(home))

  # Save data
  vroom_write(cz_dat,
              here("data",
                   "mpt_home_by_cz",
                   paste0("mpt_home_by_cz_",
                          cz,
                          ".csv.gz")),
              delim = ",")
  message(paste0("----- Completed CZ ",
                 cz,
                 " -----"))
}
