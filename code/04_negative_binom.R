##### This file runs the negative binomial model used to weight tracts (Step 4) ---------
# Phones are not spread evenly across tracts: some tracts have fewer phones per resident.
# The model predicts the number of phones living in each tract from tract demographics (paper Table 3).
# The race part of that prediction is then used to adjust each tract's phone count
# (adjusted_freq_tract), which weights the phones in the segregation estimates (Step 6).
# Input:  data/czdata_homect/cz_home_ct_cznames.csv.gz, data/census_sim/*.csv.gz
# Output: data/czdata_homect/cz_home_ct_cznames_adjusted_obs.csv.gz,
#         objects/neg_binomial_model.RDS, figures/home_tract_density.png
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
       "forcats",
       "ggplot2",
       "here",
       "vroom"))
rm(ipak)

here::i_am("code/04_negative_binom.R")

# Upload data --------------------
# Mobile phone data: one row per ping; each phone's home tract is already assigned
mpt_dat <- vroom(here("data",
                      "czdata_homect",
                      "cz_home_ct_cznames.csv.gz"),
                 delim = ",",
                 col_types = "ccnnnnnnnnncnc") |>
  filter(!is.na(home))

# Census data: population by race and ethnicity, age, and sex
cen_dat <- vroom(here("data",
                      "census_sim",
                      "census_2020_tract_sim.csv.gz"),
                 delim = ",") |>
  mutate(total = U7S001,
         white = U7L003,
         black = U7L004,
         hisp = U7L010,
         asian = U7L006,
         oth = U7L009 + U7L008 + U7L007 + U7L005,
         age_under_5 = U7S003 + U7S027,
         age_5_14 = U7S004 + U7S005 + U7S028 + U7S029,
         age_15_19 = U7S006 + U7S007 + U7S030 + U7S031,
         age_20_64 = U7S008 + U7S009 + U7S010 + U7S011 + U7S012 + U7S013 + U7S014
         + U7S015 + U7S016 + U7S017 + U7S018 + U7S019 + U7S032 + U7S033 + U7S034
         + U7S035 + U7S036 + U7S037 + U7S038 + U7S039 + U7S040 + U7S041 + U7S042
         + U7S043,
         age_65_plus = U7S020 + U7S021 + U7S022 + U7S023 + U7S024 + U7S025 + U7S044
         + U7S045 + U7S046 + U7S047 + U7S048 + U7S049,
         female = U7S026) |>
  select(GISJOIN,
         total:female)

# ACS data: education (population 25+) and household income
acs_dat <- vroom(here("data",
                      "census_sim",
                      "acs_20205_tract_sim.csv.gz"),
                 delim = ",") |>
  mutate(total_pop_acs = AMRZE001,
         educ_hs = AMRZE002 + AMRZE003 + AMRZE004 + AMRZE005 + AMRZE006 + AMRZE007 + AMRZE008
         + AMRZE009 + AMRZE010 + AMRZE011 + AMRZE012 + AMRZE013 + AMRZE014 + AMRZE015 + AMRZE016
         + AMRZE017 + AMRZE018,
         educ_college_nodegree = AMRZE019 + AMRZE020,
         educ_assoc = AMRZE021,
         educ_bach = AMRZE022,
         educ_advanced = AMRZE023 + AMRZE024 + AMRZE025,
         total_hh = AMR7E001,
         inc_under_20k = AMR7E002 + AMR7E003 + AMR7E004,
         inc_20k_35k = AMR7E005 + AMR7E006 + AMR7E007,
         inc_35k_50k = AMR7E008 + AMR7E009 + AMR7E010,
         inc_50k_75k = AMR7E011 + AMR7E012,
         inc_75k_125k = AMR7E013 + AMR7E014,
         inc_125k_200k = AMR7E015 + AMR7E016,
         inc_over_200k = AMR7E017) |>
  select(GISJOIN,
         total_pop_acs:inc_over_200k)

# check for errors: groups add up to totals, and every tract with pings has census and ACS data
if(all(with(cen_dat, white + black + hisp + asian + oth == total)) &&
   all(with(cen_dat, age_under_5 + age_5_14 + age_15_19 + age_20_64 + age_65_plus == total)) &&
   all(with(acs_dat, educ_hs + educ_college_nodegree + educ_assoc + educ_bach + educ_advanced == total_pop_acs)) &&
   all(with(acs_dat, inc_under_20k + inc_20k_35k + inc_35k_50k + inc_50k_75k + inc_75k_125k
            + inc_125k_200k + inc_over_200k == total_hh)) &&
   all(mpt_dat$GISJOIN %in% cen_dat$GISJOIN) &&
   all(mpt_dat$GISJOIN %in% acs_dat$GISJOIN)) {
  message("Looks good to me!")
} else message("You made an error :(")

# Creating analysis data -----------
# Number of phones (unique devices) living in each home tract
freq_tract_dat <- mpt_dat |>
  distinct(device_id,
           home) |>
  count(home,
        name = "freq_tract")

# Attach the tract's CZ, census, and ACS data
freq_tract_dat <- freq_tract_dat |>
  left_join(distinct(mpt_dat, home = GISJOIN, cz2000),
            by = join_by(home)) |>
  left_join(rename(cen_dat, home = GISJOIN),
            by = join_by(home)) |>
  left_join(rename(acs_dat, home = GISJOIN),
            by = join_by(home))

# Plot home frequencies (paper Figure 2) --------------------
# Use a light grey fill and black curve so the distribution is clear in print.
density_plot <- ggplot(freq_tract_dat,
                       aes(x = freq_tract)) +
  geom_density(fill = "grey90",
               color = "black",
               linewidth = 0.6) +
  theme_bw(base_size = 13) +
  labs(x = "Mobile Phones per Home Tract",
       y = "Density") +
  theme(text = element_text(family = "Times"),
        axis.text = element_text(size = 11, color = "black"),
        axis.title.x = element_text(margin = margin(t = 8)),
        axis.title.y = element_text(margin = margin(r = 8)),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        panel.border = element_rect(color = "grey40", linewidth = 0.4),
        plot.margin = margin(8, 10, 8, 8)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(xlim = c(0, max(freq_tract_dat$freq_tract)))

ggsave(here("figures",
            "home_tract_density.png"),
       plot = density_plot,
       width = 6,
       height = 3.5,
       units = "in",
       dpi = 600)

# Model variables as percentages of the tract population (as in the paper)
mod_data <- freq_tract_dat |>
  mutate(across(white:age_65_plus, ~ if_else(total > 0, (.x / total) * 100, 0)),
         across(starts_with("educ"), ~ if_else(total_pop_acs > 0, (.x / total_pop_acs) * 100, 0)),
         across(starts_with("inc_"), ~ if_else(total_hh > 0, (.x / total_hh) * 100, 0)),
         female = (female / total) * 100,
         cz2000 = fct_relevel(as_factor(cz2000), "134")) |>
  tidyr::drop_na()

# Negative binomial model --------------------
# Reference categories: white, ages 20-64, bachelor's degree, $50-75k income, and CZ 134 (NYC)
binom <- MASS::glm.nb(freq_tract ~ cz2000 +
                        educ_hs + educ_college_nodegree + educ_assoc + educ_advanced +
                        inc_under_20k + inc_20k_35k + inc_35k_50k + inc_75k_125k + inc_125k_200k + inc_over_200k +
                        total + black + hisp + asian + oth +
                        age_under_5 + age_5_14 + age_15_19 + age_65_plus + female,
                      data = mod_data)

# Save model
saveRDS(binom,
        here("objects",
             "neg_binomial_model.RDS"))

# Race-adjusted phone counts --------------------
# Predicted phones per tract with and without the race terms. Their ratio is the part of a tract's
# phone count that is explained by its racial composition, and it rescales the observed count.
mod_matrix <- model.matrix(binom)
neg_binom_coef <- coef(binom)
neg_binom_coef_zero <- replace(neg_binom_coef,
                               c("black", "hisp", "asian", "oth"),
                               0)
y_hat <- (mod_matrix %*% neg_binom_coef) |> exp() |> as.vector()
y_hat_zero <- (mod_matrix %*% neg_binom_coef_zero) |> exp() |> as.vector()
mod_data$adjusted_freq_tract <- mod_data$freq_tract * (y_hat / y_hat_zero)

# Attach adjusted counts to the mobile phone data
mpt_dat <- mpt_dat |>
  left_join(select(mod_data, home, adjusted_freq_tract),
            by = join_by(home))

# Save data --------------------
vroom_write(mpt_dat,
            here("data",
                 "czdata_homect",
                 "cz_home_ct_cznames_adjusted_obs.csv.gz"),
            delim = ",")
