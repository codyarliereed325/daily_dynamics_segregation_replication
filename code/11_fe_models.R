##### This file estimates the fixed-effects models and plots the results (Step 11) ---------
# Outcome: multigroup racial segregation (Htotal, Theil H x 100) for each CZ and hourly window.
#   Hourly model:      hour of day (vs. 12 PM), with day-of-week and CZ fixed effects
#   Daily model:       day of week (vs. Wednesday), with hour and CZ fixed effects
#   Interaction model: each day vs. Wednesday at each hour, with CZ fixed effects
# Standard errors are clustered by CZ (and by day of week in the hourly model), as in the paper.
# Input:  data/seg_results_data/seg_data_<cz>.csv.gz, data/cz_names.csv.gz
# Output: figures/day_hour_facet_fig.png (paper Figure 4), figures/hour_day_interaction_fig.png (paper Figure 5)
# Functions --------------------
# Upload packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Upload packages --------------------
ipak(c("fs",
       "here",
       "vroom",
       "tidyverse",
       "lfe"))
rm(ipak)

here::i_am("code/11_fe_models.R")

# Upload data --------------------
seg_results <- vroom(fs::dir_ls(here("data",
                                     "seg_results_data"),
                                glob = "*.csv.gz"),
                     delim = ",") |>
  tidyr::drop_na() |>
  left_join(vroom(here("data",
                       "cz_names.csv.gz"),
                  delim = ","),
            by = join_by(cz2000)) |>

  # Days of the week in January 2020
  mutate(day_of_week = case_when(day == 3 | day == 10 | day == 17 | day == 24 ~ "Friday",
                                 day == 4 | day == 11 | day == 18 | day == 25 ~ "Saturday",
                                 day == 5 | day == 12 | day == 19 | day == 26 ~ "Sunday",
                                 day == 6 | day == 13 ~ "Monday",
                                 day == 7 | day == 14 ~ "Tuesday",
                                 day == 8 | day == 15 ~ "Wednesday",
                                 day == 9 | day == 16 ~ "Thursday"),
         day_of_week = factor(day_of_week, levels = c("Monday",
                                                      "Tuesday",
                                                      "Wednesday",
                                                      "Thursday",
                                                      "Friday",
                                                      "Saturday",
                                                      "Sunday")),

         # convert to 0-100 scale
         across(Hwhite:Htotal, ~ .x * 100),
         metro_name = factor(metro_name),
         hour = factor(hour))

# Dummy variables (Wednesday and 12 PM are the reference categories)
day_of_week_dummies <- model.matrix(~ day_of_week - 1, data = seg_results)[, -3]
hour_dummies <- model.matrix(~ hour - 1, data = seg_results)[, -13]

# Models --------------------
# By hour segregation
hour_res <- felm(Htotal ~ hour_dummies | day_of_week + metro_name | 0 | day_of_week + metro_name,
                 data = seg_results)

# By day segregation
day_res <- felm(Htotal ~ day_of_week_dummies | hour + metro_name | 0 | metro_name,
                data = seg_results)

# Plot hourly and daily results --------------------
# Coefficients and clustered standard errors (the reference categories are 0)
hr_plot_dat <- tibble(Type = as_factor(0:23),
                      Coefficient = c(hour_res$coefficients[1:12], NA, hour_res$coefficients[13:23]),
                      Error = c(hour_res$cse[1:12], NA, hour_res$cse[13:23]),
                      Model = "Hourly")
day_plot_dat <- tibble(Type = as_factor(levels(seg_results$day_of_week)),
                       Coefficient = c(day_res$coefficients[1:2], NA, day_res$coefficients[3:6]),
                       Error = c(day_res$cse[1:2], NA, day_res$cse[3:6]),
                       Model = "Daily")
combined_plot_dat <- bind_rows(hr_plot_dat,
                               day_plot_dat) |>
  mutate(Type = fct_rev(Type),
         IsReference = (Model == "Daily" & Type == "Wednesday") | (Model == "Hourly" & Type == "12"))

day_hour_plot <- ggplot(combined_plot_dat, aes(x = Coefficient, y = Type)) +
  geom_point(size = 2.5, shape = 16, na.rm = TRUE) +                    # estimates
  geom_point(data = filter(combined_plot_dat, IsReference),
             aes(x = 0), size = 2.5, shape = 1, stroke = 1) +           # reference categories
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_errorbar(aes(xmin = Coefficient - 1.96 * Error, xmax = Coefficient + 1.96 * Error),
                width = 0.3, linewidth = 0.5, na.rm = TRUE) +           # 95% confidence intervals
  facet_grid(Model ~ ., scales = "free_y", space = "free") +
  theme_bw(base_size = 14) +
  theme(text = element_text(family = "Times"),
        strip.background = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90"),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(1.5, "lines")) +
  labs(x = "Coefficient Estimate") +
  scale_x_continuous(labels = scales::comma)

ggsave(here("figures",
            "day_hour_facet_fig.png"),
       plot = day_hour_plot,
       width = 4.5,
       height = 7.5,
       units = "in",
       dpi = 300)

# Hour by day of week results --------------------
# Refit with each hour as the reference. The six day coefficients (24-29, after the 23 hour terms)
# then compare Monday, Tuesday, Thursday, Friday, Saturday, and Sunday with Wednesday at that hour.
list_res <- list()
for (i in 1:24) {
  hour_dummies <- model.matrix(~ hour - 1, data = seg_results)[, -i]
  hour_day_res <- felm(Htotal ~ hour_dummies * day_of_week_dummies | metro_name | 0 | metro_name,
                       data = seg_results)
  list_res[[i]] <- tibble(coef_res = hour_day_res$coefficients[24:29],
                          se_res = hour_day_res$cse[24:29],
                          Hour = i - 1,
                          Day = c("Monday",
                                  "Tuesday",
                                  "Thursday",
                                  "Friday",
                                  "Saturday",
                                  "Sunday"))
}
hour_day_res_data <- bind_rows(list_res) |>
  mutate(Day = as_factor(Day),
         CI_lower = coef_res - 1.96 * se_res,
         CI_upper = coef_res + 1.96 * se_res)

# Other weekdays in grey for context; confidence bands only for Friday, Saturday, and Sunday
weekend <- c("Friday",
             "Saturday",
             "Sunday")

hour_day_plot <- ggplot(hour_day_res_data, aes(x = Hour, y = coef_res, group = Day, color = Day)) +
  geom_ribbon(data = filter(hour_day_res_data, Day %in% weekend),
              aes(ymin = CI_lower, ymax = CI_upper, fill = Day),
              color = NA, alpha = 0.15, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_line(data = filter(hour_day_res_data, !Day %in% weekend),
            aes(color = "Other weekdays"), linewidth = 0.6) +
  geom_line(data = filter(hour_day_res_data, Day %in% weekend),
            linewidth = 1.2) +
  annotate("text", x = 12, y = 0, label = "Wednesday (reference)",
           vjust = 1.6, color = "grey40", size = 3.5, family = "Times") +
  scale_color_manual(values = c("Friday" = "#377eb8", "Saturday" = "#4daf4a", "Sunday" = "#984ea3",
                                "Other weekdays" = "grey65"),
                     breaks = c("Friday", "Saturday", "Sunday", "Other weekdays"),
                     labels = c("Friday", "Saturday", "Sunday", "Other weekdays: Monday, Tuesday, Thursday")) +
  scale_fill_manual(values = c("Friday" = "#377eb8", "Saturday" = "#4daf4a", "Sunday" = "#984ea3"),
                    guide = "none") +
  scale_x_continuous(breaks = c(seq(0, 21, 3), 23)) +
  theme_minimal(base_size = 16) +
  theme(text = element_text(family = "Times"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        panel.grid.major = element_line(color = "grey93"),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(1.5, "lines"),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  labs(y = "Difference in racial segregation from Wednesday\n(Theil H × 100)",
       x = "Hours From Midnight")

ggsave(here("figures",
            "hour_day_interaction_fig.png"),
       plot = hour_day_plot,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)
