# -------------------------------
# Run pipeline for one country
# -------------------------------

# Load libraries
suppressPackageStartupMessages({
  library(yaml)
  library(optparse) # for command line arguments
  library(tidyverse)
  library(ggplot2)
  library(EpiLPS)
  library(socialmixr)
  library(reshape2)
  library(parallel)
  library(RColorBrewer)
  library(progress)
  library(fuzzyjoin)
  library(expm)
  library(ggpmisc)
  library(ggrepel)
  library(directlabels)
  library(geomtextpath)
  library(scales)
})

# -------------------------------
# 1. Global parameters and options
# -------------------------------
opt=list()
opt$country <- "UK"
opt$begin <- "2020-12-24"
opt$nperiods <- 20


# -------------------------------
# 2. Load config
# -------------------------------
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
contact_mats <- build_contact_matrices(country = opt$country,
                                       raw_data = raw_data,
                                       age_groups = config$study$age_groups,
                                       n_sample = n_sample,
                                       boot = config$study$boot)

# (C) Incidence (one progress bar per period, it can take a while!)
incidence <- get_incidence(country = opt$country,   
                           incidence_path = config$data_paths$incidence,
                           age_groups = config$study$age_groups)

# (D) Reproduction number
rt <- get_rt(country = opt$country,
             rt_path = config$data_paths$rt)

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

save_opt <- file.path(config$data_processed, paste0("opt_", opt$country,"_",max(periods$period),"_",min(periods$start),"_to_",max(periods$start),".rds"))
saveRDS(opt, save_opt)


# tests 
# Extract ngm into a long data frame
ngm_long <- map_dfr(1:opt$nperiods, function(p) {
  map_dfr(1:config$study$n_sample, function(s) {
    mat <- results[[p]]$ngm[[s]]
    age_labels = rownames(mat)
    as.data.frame(mat) %>%
      mutate(from = age_labels) %>%
      pivot_longer(-from, names_to = "to", values_to = "value") %>%
      mutate(period = p, sample = s)
  })
})
age_labels = unique(ngm_long$from)
ngm_long <- ngm_long %>% mutate(from = factor(from, levels = age_labels),
                to = factor(to, levels = age_labels)) 

ngm_summary <- ngm_long %>%
  group_by(period, from, to) %>%
  summarise(
    m = mean(value, na.rm = TRUE),
    sdt   = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label = paste0(round(m, 2), " ± ", round(sdt, 2)))

# helper plot
plot_ngm_heatmap_text <- function(df) {
  df %>%
    ggplot(aes(x = to, y = from, fill = m)) +
    geom_tile(color = "white") +
    geom_text(aes(label = label), size = 2.5, color = "black") +
    scale_fill_gradient(low = "white", high = "#1565C0", name = "Mean NGM") +
    facet_wrap(~ period, ncol = 5, labeller = label_both) +
    labs(title = "NGM — Mean ± SD", x = "To", y = "From") +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      strip.background = element_rect(fill = "grey90", color = NA),
      panel.grid       = element_blank()
    )
}
plot_ngm_heatmap_text(ngm_summary)

