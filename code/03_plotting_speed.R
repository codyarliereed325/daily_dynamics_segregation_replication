##### This file describes recorded speeds in the phone data (Step 3) ---------
# Share of observations in five speed categories, after the paper's speed (0-24 km/h) and
# vertical-accuracy filters (paper Figure 1). In the paper this uses all cleaned records, including
# phones without a home tract; the simulated data only include phones with a home tract.
# Input:  data/czdata_homect/cz_home_ct_cznames.csv.gz
# Output: data/speed_distribution.csv.gz, figures/speed_distribution.png
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
       "ggplot2",
       "here",
       "vroom"))
rm(ipak)

here::i_am("code/03_plotting_speed.R")

# Upload data --------------------
# Speed is recorded in meters per second
dat <- vroom(here("data",
                  "czdata_homect",
                  "cz_home_ct_cznames.csv.gz"),
             delim = ",",
             col_types = cols_only(speed = col_double(),
                                   vertical_accuracy = col_double())) |>
  filter(speed >= 0 & speed <= 6.67,
         vertical_accuracy < 20) |>
  transmute(speed_km = speed * 3.6)

# Share of observations in each speed category --------------------
labels <- c("0-1", "1-5", "5-10", "10-15", "15+")
speed_dat <- dat |>
  mutate(bin = cut(speed_km,
                   breaks = c(0, 1, 5, 10, 15, Inf),
                   labels = labels,
                   right = FALSE,
                   include.lowest = TRUE)) |>
  count(bin,
        name = "observations",
        .drop = FALSE) |>
  mutate(proportion = observations / sum(observations))
rm(dat)

# Save data
vroom_write(speed_dat,
            here("data",
                 "speed_distribution.csv.gz"),
            delim = ",")

# Figure --------------------
# The five categories, with percentages labeled directly
speed_plot <- speed_dat |>
  mutate(bin = factor(bin, levels = rev(labels))) |>
  ggplot(aes(x = 100 * proportion, y = bin)) +
  geom_col(fill = "grey40", width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", 100 * proportion)),
            hjust = -0.15, family = "Times", size = 4) +
  scale_x_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20),
                     expand = c(0, 0)) +
  labs(x = "Observations (%)",
       y = "Recorded Speed (km/h)") +
  theme_bw(base_size = 13) +
  theme(text = element_text(family = "Times"),
        axis.text = element_text(size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90"),
        plot.margin = margin(8, 8, 8, 8))

ggsave(here("figures",
            "speed_distribution.png"),
       plot = speed_plot,
       width = 6,
       height = 3.2,
       units = "in",
       dpi = 300)
