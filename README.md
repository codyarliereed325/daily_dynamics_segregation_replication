# Replication materials: Daily Dynamics of Urban Racial Segregation in the United States

Cody Arlie Reed

This folder shows, step by step, how the paper measures hourly racial segregation with mobile phone location data, and lets anyone run the same analysis code. The phone data used in the paper are confidential and cannot be shared, so **every person, phone, ping, and demographic count in this folder is simulated**. The only real inputs are public: census tract boundaries and commuting-zone definitions. The simulated data have the same structure as the paper's cleaned data, so the analysis scripts are the paper's code, reorganized for readability (see [How this code differs from the paper's code](#how-this-code-differs-from-the-papers-code)).

## Disclaimer
This file and the replication package was mostly written by the Opus 5.5 large language model. Opus 5.5 was instructed to mirror the (non-LLM generated) code I wrote for the paper analysis. I did check to make sure the package faithfully resembled the steps without providing sensitive data. The code in this replication folder resembles the code pretty accurately, including some of my bad coding habits.

## What is real and what is simulated

| Input | Status | Source |
|---|---|---|
| Census tract boundaries for the Chicago (CZ 58) and New York City (CZ 134) commuting zones | Real, public domain | U.S. Census Bureau, 2020 cartographic boundary files (tracts, 1:500,000) |
| Counties in each commuting zone | Real, public | USDA Economic Research Service, 2000 commuting zones |
| Tract demographics (2020 Census and ACS tables) | **Simulated** | Made-up parameters, in the layout of the NHGIS extracts used in the paper |
| Phones and pings | **Simulated** | Made-up daily routines; no real phone data of any kind |

The simulated demographics do not describe the real neighborhoods of Chicago or New York.

## Requirements

- R (tested with R 4.5.2 on macOS) and a C++ compiler for Rcpp (Xcode Command Line Tools on macOS, Rtools on Windows).
- R packages: dplyr, forcats, fs, ggplot2, here, lfe, MASS, Rcpp, RcppArmadillo, RcppParallel, scales, sf, stringr, tidyr, tidytable, tidyverse, vroom. Each script installs any missing package with its `ipak()` helper.
- Runtime: about 2 minutes on a laptop. For each hourly window, Step 6 compares every phone with every other phone, so time and memory grow with the square of the number of phones.

## How to run

Open `daily_dynamics_segregation_replication.Rproj` in RStudio and run `run_all.R`, or from this folder:

```
Rscript run_all.R
```

`run_all.R` runs `code/01`–`code/13` in order, each in its own R session. The simulation is seeded, so every run produces the same data and results.

## Steps

| Step | Script | What it does | Output | In the paper |
|---|---|---|---|---|
| 0 | `00_prepare_tract_shapes.R` | Downloads public tract shapes for the two CZs (already done; needs internet) | `data/shape_files/tracts_2020/` | |
| 1 | `01_simulate_census_data.R` | Simulates tract Census and ACS tables | `data/census_sim/` | Census and ACS data |
| 2 | `02_simulate_mpt_data.R` | Simulates cleaned phone data, each phone's home tract already assigned | `data/czdata_homect/cz_home_ct_cznames.csv.gz` | Cleaned phone data |
| 3 | `03_plotting_speed.R` | Share of observations by recorded speed | `figures/speed_distribution.png` | Figure 1 |
| 4 | `04_negative_binom.R` | Negative binomial model of phones per home tract; race-adjusted phone counts | `objects/neg_binomial_model.RDS`, `figures/home_tract_density.png` | Table 3, Figure 2 |
| 5 | `05_attaching_census_data_to_mpt_data.R` | Attaches home-tract population by race; one file per CZ | `data/mpt_home_by_cz/` | |
| 6 | `06_calculate_segregation.R` | Space-time weighting (1 km, 1 hour) and Theil index for 428 hourly windows per CZ | `data/seg_results_data/` | Hourly segregation |
| 7 | `07_cz_residential_segregation.R` | Residential Theil index with the same spatial weighting | `data/residential_spat_seg.csv.gz` | Residential baseline |
| 8 | `08_displacement_distributions.R` | Average distance from the home tract in each window | `data/displacements/` | Distance from home |
| 9 | `09_hourly_sample_sizes.R` | Observations used in each hourly estimate | `data/hourly_sample_sizes.csv.gz` | Table 4 |
| 10 | `10_segregation_tables.R` | Ratio of hourly to residential segregation | `figures/seg_ratio_boxplot.png` | Figure 3 |
| 11 | `11_fe_models.R` | Fixed-effects models by hour, day of week, and hour × day | `figures/day_hour_facet_fig.png`, `figures/hour_day_interaction_fig.png` | Figures 4 and 5 |
| 12 | `12_temporal_pattern_contrasts.R` | Daytime versus overnight and Friday versus other evenings, from the hour × day model | `data/temporal_pattern_contrasts.csv.gz` and two detailed tables | Table 5 |
| 13 | `13_disp_results_plots.R` | Distance from home by hour and day of week | `figures/displacement_hourly.png` | Figure 6 |

The C++ functions in `code/` (`main_function.cpp`, `main_function_tracts.cpp`, `theil_index.cpp`, `theil_multi.cpp`, `haversine2.cpp`) are the paper's code, unchanged. In both the hourly and residential measures, each phone or tract counts itself fully in its own local mix.

**Not included:** Table 1 is conceptual. Table 2 and the retention figures in the appendix require the raw data, before cleaning. The appendix's three sensitivity checks (the same number of phones in every window, fixed reference shares, and a wider or narrower time kernel) are described in the paper but not included here.

## Data format

**Cleaned phone data** (`data/czdata_homect/cz_home_ct_cznames.csv.gz`), one row per ping, with only the columns the analysis uses:

| Column | Description |
|---|---|
| `device_id` | Phone identifier (simulated IDs look like `SYN-000001`) |
| `home` | Home tract (GISJOIN), assigned before this step from nighttime pings |
| `altitude` | Meters |
| `vertical_accuracy` | Meters; pings with 20 or more are dropped |
| `speed` | Meters per second; pings outside 0–6.67 (24 km/h) are dropped |
| `day`, `hour`, `minute`, `second` | Local time; `day` is the day of the month in January 2020 |
| `latitude`, `longitude` | Degrees (WGS84) |
| `GISJOIN` | Tract the ping falls in |
| `cz2000` | Commuting zone (2000 definition) |
| `metro_name` | Commuting zone name |

**Census and ACS tables** (`data/census_sim/`) use the NHGIS codes of the paper's extract. Only the codes the analysis uses are included:

| File | Codes | Table |
|---|---|---|
| `census_2020_tract_sim.csv.gz` | `U7L003`–`U7L010` | 2020 Census, P5 (Hispanic or Latino origin by race) |
| | `U7S001`, `U7S003`–`U7S049` | 2020 Census, P12 (sex by age) |
| `acs_20205_tract_sim.csv.gz` | `AMRZE001`–`AMRZE025` | ACS 2016–2020 5-year, B15003 (educational attainment) |
| | `AMR7E001`–`AMR7E017` | ACS 2016–2020 5-year, B19001 (household income) |

## Using real data

- **Census and ACS:** download the tables above for 2020 census tracts from IPUMS NHGIS (<https://www.nhgis.org>) and point the file paths in Steps 4, 5, and 7 to your extract. NHGIS terms do not allow redistributing their files, which is why this folder simulates them. With real data, skip the simulation steps (1 and 2).
- **Tract shapes:** a tract shapefile with a `GISJOIN` column, saved as `data/shape_files/tracts_2020/US_tract_2020.shp`.
- **Phone data:** cleaned phone data in the format above, saved as `data/czdata_homect/cz_home_ct_cznames.csv.gz`, and your CZs in `data/cz_names.csv.gz`.
- **Other dates:** the hourly windows (`time_vec` in Steps 6 and 8) and the days of the week (Steps 11–13) follow the paper's calendar (January 3–19 and 24–26, 2020, local time) and need to be changed for other dates.

## How this code differs from the paper's code

- It starts from cleaned phone data with home tracts already assigned. The cleaning steps (time zones, tract and commuting-zone assignment, home detection) are described in the paper and are not included.
- It reads simulated Census and ACS tables instead of the NHGIS extract. The employment table is not simulated: in the paper it was only used to drop a few tracts with implausible values, not in the model.
- Only the columns the analysis uses are kept, and the phone identifier is called `device_id`.
- The paper drops the San Francisco–Oakland commuting zone; the example has only Chicago and New York, so those filters are not needed.
- Data are saved as `.csv.gz`; only fitted models are saved as `.RDS`.
- The scripts were reorganized for readability: the native pipe `|>`, one CZ name file, CZs read from the data, the model formula written out, fixed checks that assumed 18 CZs replaced by checks on each CZ's 428 windows, and no pauses, unused variables, unused models, or unused plots. The home-density figure chooses its axis breaks automatically. For Steps 4–8, 10, 11, and 13, the edited scripts and line-by-line copies of the paper's code were run on the same simulated data: the results are identical (differences below 10⁻¹²). Steps 3, 9, and 12 follow the paper's code nearly line by line.

## Notes

- The simulated routines (weekday commutes downtown, evening and weekend outings) are made up. Patterns in the figures come from those routines; they are not evidence for or against the paper's findings.
- With two commuting zones, the clustered standard errors in Steps 11 and 12 are not meaningful (`lfe` warns about this). The paper uses 18 commuting zones.

## License

The code is released under the MIT License (see `LICENSE`). The tract shapes and commuting-zone tables are public-domain U.S. government data.

## Citation

Reed, Cody Arlie. "Daily Dynamics of Urban Racial Segregation in the United States."

Real Census and ACS inputs should be cited as described at <https://www.nhgis.org/citation-and-use-nhgis-data>.
