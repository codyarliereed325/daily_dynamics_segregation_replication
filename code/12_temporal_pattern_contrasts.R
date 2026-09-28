##### This file compares temporal patterns of racial segregation (Step 12) ---------
# Connects the three theoretical perspectives to changes in racial segregation over the day and
# week (paper Table 5). All comparisons come from one hour-by-day interaction model:
#   weekday daytime vs. overnight, weekend daytime vs. overnight, the difference between the two,
#   and Friday evening vs. Monday-Thursday evenings.
# Input:  data/seg_results_data/seg_data_<cz>.csv.gz, data/cz_names.csv.gz
# Output: data/temporal_pattern_contrasts.csv.gz, data/friday_evening_comparisons.csv.gz,
#         data/daytime_overnight_comparisons.csv.gz, objects/temporal_pattern_model.RDS
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
       "lfe",
       "tidyr",
       "vroom"))
rm(ipak)

here::i_am("code/12_temporal_pattern_contrasts.R")

# Upload data --------------------
day_names <- c("Monday",
               "Tuesday",
               "Wednesday",
               "Thursday",
               "Friday",
               "Saturday",
               "Sunday")

# Dates and hours are local. %u gives Monday = 1 through Sunday = 7 in any language.
# Multiply H by 100 to match the paper.
seg_results <- vroom(fs::dir_ls(here("data",
                                     "seg_results_data"),
                                glob = "*.csv.gz"),
                     delim = ",") |>
  tidyr::drop_na() |>
  left_join(vroom(here("data",
                       "cz_names.csv.gz"),
                  delim = ","),
            by = join_by(cz2000)) |>
  mutate(date = as.Date(paste0("2020-01-", day)),
         day_of_week = factor(day_names[as.integer(format(date, "%u"))],
                              levels = day_names),
         hour = factor(hour,
                       levels = 0:23),
         Htotal = Htotal * 100,
         metro_name = factor(metro_name))

# Check for errors: every CZ has the same 428 hourly estimates, with no missing or repeated CZ-hours
stopifnot(all(table(seg_results$cz2000) == 428),
          !anyNA(seg_results),
          all(is.finite(seg_results$Htotal)),
          !anyDuplicated(seg_results[c("cz2000", "day", "hour")]))

# By hour and day of week --------------------
# The hourly pattern can differ by day, controlling for each CZ's overall level of racial
# segregation. Standard errors are clustered by CZ because overlapping hourly windows in the same
# CZ need not have independent errors.
hour_day_res <- felm(Htotal ~ hour * day_of_week | metro_name | 0 | metro_name,
                     data = seg_results)

model_coef <- coef(hour_day_res)
model_vcov <- vcov(hour_day_res,
                   type = "cluster")[names(model_coef),
                                      names(model_coef),
                                      drop = FALSE]

# 95% confidence intervals: estimate +/- 1.96 * SE, as in the paper
critical_value <- 1.96

# Average model estimate for a period --------------------
# Every included day-hour combination gets equal weight, so uneven calendar coverage does not
# decide how much each day contributes. The result is a vector of weights on the model
# coefficients; subtracting two such vectors gives the weights for their difference.
average_period <- function(days, hours){
  period_data <- expand_grid(day_of_week = factor(days,
                                                 levels = day_names),
                             hour = factor(hours,
                                            levels = 0:23))
  period_design <- model.matrix(~ hour * day_of_week,
                                data = period_data)
  stopifnot(all(names(model_coef) %in% colnames(period_design)))
  colMeans(period_design[, names(model_coef), drop = FALSE])
}

# Estimate a difference and its uncertainty --------------------
# For coefficient weights c and covariance matrix V: difference = c'b, SE = sqrt(c'Vc).
# This includes the covariance between the two periods; treating them as independent is incorrect.
# A negative estimate means lower racial segregation in the first named period.
estimate_difference <- function(contrast, comparison){
  contrast <- contrast[names(model_coef)]
  estimate <- sum(contrast * model_coef)
  standard_error <- sqrt(as.numeric(t(contrast) %*% model_vcov %*% contrast))
  tibble(comparison = comparison,
         estimate = estimate,
         standard_error = standard_error,
         ci_lower = estimate - critical_value * standard_error,
         ci_upper = estimate + critical_value * standard_error,
         n_observations = nobs(hour_day_res))
}

# Comparisons --------------------
# Daytime (8 AM-5 PM) compared with overnight (midnight-5 AM)
weekday_difference <- average_period(day_names[1:5], 8:17) -
  average_period(day_names[1:5], 0:5)
weekend_difference <- average_period(day_names[6:7], 8:17) -
  average_period(day_names[6:7], 0:5)

# Friday evening (6-11 PM) compared with Monday-Thursday evenings
friday_difference <- average_period("Friday", 18:23) -
  average_period(day_names[1:4], 18:23)

# The four comparisons in Table 5
# (a positive weekend-minus-weekday difference means the weekend decline is smaller)
temporal_contrasts <- bind_rows(
  estimate_difference(weekday_difference,
                      "Weekday daytime minus weekday overnight"),
  estimate_difference(weekend_difference,
                      "Weekend daytime minus weekend overnight"),
  estimate_difference(weekend_difference - weekday_difference,
                      "Weekend day-night difference minus weekday day-night difference"),
  estimate_difference(friday_difference,
                      "Friday evening minus Monday-Thursday evening"))

# Friday evening compared with each other day's evening
friday_comparisons <- lapply(day_names[day_names != "Friday"],
                             function(day){
                               estimate_difference(average_period("Friday", 18:23) -
                                                     average_period(day, 18:23),
                                                   paste("Friday evening minus", day, "evening"))
                             }) |>
  bind_rows()

# Daytime compared with overnight for each day
daytime_comparisons <- lapply(day_names,
                              function(day){
                                estimate_difference(average_period(day, 8:17) -
                                                      average_period(day, 0:5),
                                                    paste(day, "daytime minus", day, "overnight"))
                              }) |>
  bind_rows()

# Save results --------------------
vroom_write(temporal_contrasts,
            here("data",
                 "temporal_pattern_contrasts.csv.gz"),
            delim = ",")
vroom_write(friday_comparisons,
            here("data",
                 "friday_evening_comparisons.csv.gz"),
            delim = ",")
vroom_write(daytime_comparisons,
            here("data",
                 "daytime_overnight_comparisons.csv.gz"),
            delim = ",")
saveRDS(hour_day_res,
        here("objects",
             "temporal_pattern_model.RDS"))

print(temporal_contrasts,
      width = Inf)
