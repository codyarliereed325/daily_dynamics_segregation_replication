##### This file compares hourly and residential segregation for each CZ (Step 10) ---------
# The racial segregation ratio divides each hourly estimate by the CZ's residential segregation:
# below 1 = less segregated than where people live, above 1 = more segregated (paper Figure 3).
# Input:  data/seg_results_data/seg_data_<cz>.csv.gz, data/residential_spat_seg.csv.gz, data/cz_names.csv.gz
# Output: figures/seg_ratio_boxplot.png
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
       "vroom"))
rm(ipak)

here::i_am("code/10_segregation_tables.R")

# Upload data --------------------
# Residential segregation (Step 7)
resident_seg <- vroom(here("data",
                           "residential_spat_seg.csv.gz"),
                      delim = ",") |>
  select(metro_name = cz_name,
         multiH)

# Hourly segregation (Step 6) and its ratio to residential segregation
ratios_dat <- vroom(fs::dir_ls(here("data",
                                    "seg_results_data"),
                               glob = "*.csv.gz"),
                    delim = ",") |>
  left_join(vroom(here("data",
                       "cz_names.csv.gz"),
                  delim = ","),
            by = join_by(cz2000)) |>
  left_join(resident_seg,
            by = join_by(metro_name)) |>
  mutate(seg_ratio = round(round(Htotal, 4) / multiH, 4))

# Figure --------------------
# All hourly ratios in each CZ, with CZs ordered by residential segregation
ratio_boxplot <- ggplot(ratios_dat, aes(y = reorder(metro_name, multiH), x = seg_ratio)) +
  geom_vline(xintercept = 1, linewidth = 0.5, linetype = "dashed", color = "black") +
  geom_point(color = "grey30", alpha = 0.2, size = 0.5,
             position = position_jitter(height = 0.2, seed = 1)) +
  geom_boxplot(outliers = FALSE) +
  labs(x = "Racial Segregation Ratio") +
  scale_x_continuous(limits = c(0, NA)) +
  theme_bw(base_size = 14) +
  theme(text = element_text(family = "Times"),
        axis.text = element_text(size = 12),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90"),
        panel.grid.major.y = element_blank())

ggsave(here("figures",
            "seg_ratio_boxplot.png"),
       plot = ratio_boxplot,
       width = 5.25,
       height = 6,
       units = "in",
       dpi = 300)
