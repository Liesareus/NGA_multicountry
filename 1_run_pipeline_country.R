# -------------------------------
# Run pipeline for one country
# -------------------------------


# -------------------------------
# 2. Load options and config
# -------------------------------
source("0_options.R")
config <- yaml::read_yaml("study_config.yml")

if (!(opt$country %in% config$study$countries)) {
  stop(paste("Country", opt$country, "is not listed in config."))
}

if (is.null(opt$begin)) {
  stop("You must provide a beginning date (YYYY-MM-DD).")
}

# important study parameters
begin_date <- as.Date(opt$begin)
n_sample <- config$study$n_sample  # number of samples for contact matrices

# -------------------------------
# 3. Adjust to next Friday
# -------------------------------
# weekday() returns 1 = Sunday ... 7 = Saturday by default
# We'll set Monday = 1, Friday = 5 for convenience
wday_index <- lubridate::wday(begin_date, week_start = 1)  # Monday = 1
if (wday_index < 5) {
  # Move forward to Friday of same week
  adjusted_start <- begin_date + (5 - wday_index)
} else if (wday_index > 5) {
  # Move forward to next week's Friday
  adjusted_start <- begin_date + (7 - wday_index + 5)
} else {
  # Already Friday
  adjusted_start <- begin_date
}

# -------------------------------
# 4. Compute period boundaries
# -------------------------------
periods <- data.frame(
  period = seq_len(opt$nperiods),
  start = adjusted_start + weeks(2) * (0:(opt$nperiods - 1)),
  end = adjusted_start + weeks(2) * (1:opt$nperiods) - days(1)
)

# Print study setup
cat("\n=====================================\n")
cat("Study setup\n")
cat("Country:", opt$country, "\n")
cat("Start date (adjusted to Friday):", as.character(adjusted_start), "\n")
cat("Number of 2-week periods:", opt$nperiods, "\n")
cat("=====================================\n\n")

cat("Study periods for", opt$country, ":\n")
print(periods)
cat("\n")

# -------------------------------
# 5. Source pipeline scripts/functions
# -------------------------------
source("R_scripts/01_load_data.R")
source("R_scripts/02_contact_matrices.R")
source("R_scripts/03_incidence_data.R")
source("R_scripts/04_rt_data.R")
source("R_scripts/05_harmonize_data.R")
source("R_scripts/06_run_model.R")
source("R_scripts/utils.R")

# -------------------------------
# 6. Run pipeline step by step
# -------------------------------

# (A) Load data
raw_data <- load_data(country = opt$country,
                      contacts_path = config$data_paths$contacts,
                      periods)


# (B) Contact matrices
options(warn=-1) # surpress warnings from socialmixr (outputs a warning if age groups do not match age groups in population data, but it linearly interpolates so it is not a problem)
contact_mats <- build_contact_matrices(country = opt$country,
                                       raw_data = raw_data,
                                       age_groups = config$study$age_groups,
                                       n_sample = n_sample,
                                       boot = config$study$boot)
options(warn=0) # turn warnings back on

# (C) Incidence (one progress bar per period, it can take a while!)
incidence <- get_incidence(country = opt$country,   
                           incidence_path = config$data_paths$incidence,
                           age_groups = config$study$age_groups)

# (D) Reproduction number
rt <- get_rt(rt_path = config$data_paths$rt)

# (E) Harmonize data
inputs <- harmonize_data(measures_path = config$data_paths$measures,
                         country = opt$country,
                         contact_mats = contact_mats, 
                         incidence = incidence, 
                         rt = rt,
                         periods = periods)
# (F) Run model
results <- run_model(inputs = inputs, n_sample = n_sample)
if (is.null(names(results))) names(results) <- paste0("period", seq_along(results)) # if list elements dont have names, give them a period index

# -------------------------------
# 7. Save results
# -------------------------------
out_file_periods <- file.path(config$data_processed, paste0("periods_", opt$country,"_",max(periods$period),"_",min(periods$start),"_to_",max(periods$start),".rds"))
saveRDS(periods, out_file_periods)

out_file_inputs <- file.path(config$data_processed, paste0("inputs_", opt$country,"_",max(periods$period),"_",min(periods$start),"_to_",max(periods$start),".rds"))
saveRDS(inputs, out_file_inputs)

out_file_results <- file.path(config$data_processed, paste0("results_", opt$country,"_",max(periods$period),"_",min(periods$start),"_to_",max(periods$start),".rds"))
saveRDS(results, out_file_results)