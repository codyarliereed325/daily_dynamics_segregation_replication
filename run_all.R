##### This file runs the full replication in order ---------
# Open daily_dynamics_segregation_replication.Rproj (or run `Rscript run_all.R` from this folder).
# Each script runs in its own R session, as if it were run by itself.
# Step 0 (code/00_prepare_tract_shapes.R) downloads public tract shapes; its output is already included.
here::i_am("run_all.R")

scripts <- c("01_simulate_census_data.R",
             "02_simulate_mpt_data.R",
             "03_plotting_speed.R",
             "04_negative_binom.R",
             "05_attaching_census_data_to_mpt_data.R",
             "06_calculate_segregation.R",
             "07_cz_residential_segregation.R",
             "08_displacement_distributions.R",
             "09_hourly_sample_sizes.R",
             "10_segregation_tables.R",
             "11_fe_models.R",
             "12_temporal_pattern_contrasts.R",
             "13_disp_results_plots.R")

for (script in scripts) {
  message(paste0("===== Running ", script, " ====="))
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    shQuote(here::here("code", script)))
  if (status != 0) {
    stop(paste0("Stopped: ", script, " did not finish."))
  }
}
message("===== Done. Results are in data/, objects/, and figures/ =====")
