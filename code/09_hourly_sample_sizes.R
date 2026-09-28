##### This file summarizes the observations used in the hourly estimates (Step 9) ---------
# obs is the number of phones used for each hourly estimate, after all analysis filters and
# keeping one observation per phone (paper Table 4).
# Input:  data/seg_results_data/seg_data_<cz>.csv.gz, data/cz_names.csv.gz
# Output: data/hourly_sample_sizes.csv.gz
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
       "fs",
       "here",
       "vroom"))
rm(ipak)

here::i_am("code/09_hourly_sample_sizes.R")

# Upload data --------------------
seg_results <- vroom(fs::dir_ls(here("data",
                                     "seg_results_data"),
                                glob = "*.csv.gz"),
                     delim = ",") |>
  left_join(vroom(here("data",
                       "cz_names.csv.gz"),
                  delim = ","),
            by = join_by(cz2000))

# Check for errors: 428 distinct hourly estimates per CZ, with positive whole-number counts
stopifnot(all(table(seg_results$cz2000) == 428),
          !anyNA(seg_results),
          !anyDuplicated(seg_results[c("cz2000", "day", "hour")]),
          all(seg_results$obs > 0),
          all(seg_results$obs == floor(seg_results$obs)))

# Observations per hourly estimate --------------------
# N is the number of hourly estimates; mean, minimum, maximum, and SD describe the number of
# observations per estimate (sd() uses N - 1 in the denominator)
hourly_sample_sizes <- seg_results |>
  group_by(cz2000,
           metro_name) |>
  summarise(N = n(),
            mean = mean(obs),
            minimum = min(obs),
            maximum = max(obs),
            sd = sd(obs),
            .groups = "drop") |>
  arrange(metro_name)

# Pooled row: all hourly estimates, including variation across CZs
pooled_sample_sizes <- seg_results |>
  summarise(cz2000 = NA_real_,
            metro_name = "All commuting zones",
            N = n(),
            mean = mean(obs),
            minimum = min(obs),
            maximum = max(obs),
            sd = sd(obs))

# Save data --------------------
# Keep full precision; means and SDs are rounded only in the paper
vroom_write(bind_rows(hourly_sample_sizes,
                      pooled_sample_sizes),
            here("data",
                 "hourly_sample_sizes.csv.gz"),
            delim = ",")
