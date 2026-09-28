##### This file plots average distance from home by hour and day of the week (Step 13) ---------
# Each CZ counts equally: average within CZ first, then across CZs (95% confidence intervals across CZs).
# Input:  data/displacements/displacement_data_<cz>.csv.gz
# Output: figures/displacement_hourly.png
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
       "ggplot2",
       "here",
       "scales",
       "tidyr",
       "vroom"))
rm(ipak)

here::i_am("code/13_disp_results_plots.R")

# Upload data --------------------
displacement_results <- vroom(fs::dir_ls(here("data",
                                              "displacements"),
                                         glob = "*.csv.gz"),
                              delim = ",") |>
  tidyr::drop_na() |>

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
                                                      "Sunday"))) |>
  filter(!is.na(day_of_week))

# CZ-equal summary statistics --------------------
disp_summary <- displacement_results |>
  group_by(cz2000,
           day_of_week,
           hour) |>
  summarise(cz_mean_disp = mean(displacement),
            .groups = "drop") |>
  group_by(day_of_week,
           hour) |>
  summarise(mean_disp = mean(cz_mean_disp),
            sd_disp = sd(cz_mean_disp),
            n_cz = n(),
            se_disp = sd_disp / sqrt(n_cz),
            ci_low = mean_disp - 1.96 * se_disp,
            ci_high = mean_disp + 1.96 * se_disp,
            .groups = "drop")

# check for errors: every day and hour is present, with at least 2 CZs in each
if(nrow(disp_summary) == 168 && !anyNA(disp_summary)) {
  message("Looks good to me!")
} else message("You made an error :(")

# Plot --------------------
day_colors <- c("Monday" = "#9A9A9A",
                "Tuesday" = "#7F7F7F",
                "Wednesday" = "#4D4D4D",
                "Thursday" = "#B0B0B0",
                "Friday" = "#1B9E77",
                "Saturday" = "#D95F02",
                "Sunday" = "#7570B3")
weekend <- c("Friday",
             "Saturday",
             "Sunday")

disp_plot <- ggplot(disp_summary,
                    aes(x = hour,
                        y = mean_disp,
                        color = day_of_week,
                        group = day_of_week)) +
  geom_ribbon(data = filter(disp_summary, day_of_week %in% weekend),
              aes(ymin = ci_low,
                  ymax = ci_high,
                  fill = day_of_week),
              alpha = 0.18,
              color = NA,
              show.legend = FALSE) +
  geom_line(data = filter(disp_summary, !day_of_week %in% weekend),
            linewidth = 1.0) +
  geom_line(data = filter(disp_summary, day_of_week %in% weekend),
            linewidth = 1.4) +
  scale_color_manual(values = day_colors) +
  scale_fill_manual(values = day_colors) +
  scale_x_continuous(breaks = seq(0, 23, 2),
                     limits = c(0, 23),
                     expand = c(0, 0)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Hour of Day",
       y = "Average Displacement from Home Tract (km)",
       color = "Day of Week") +
  theme_bw(base_size = 14) +
  theme(text = element_text(family = "Times"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey90"),
        panel.grid.major.y = element_line(color = "grey92"),
        axis.text = element_text(size = 12),
        legend.position = "right")

ggsave(here("figures",
            "displacement_hourly.png"),
       plot = disp_plot,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)
