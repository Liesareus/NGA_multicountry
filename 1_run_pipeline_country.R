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



# # similar for vectors
# 
# # ── Helper: extract any vector result into a long data frame ──────────────────
# extract_vector <- function(result_name, element_labels) {
#   map_dfr(1:opt$nperiods, function(p) {
#     map_dfr(1:config$study$n_sample, function(s) {
#       vec <- results[[p]][[result_name]][[s]]
#       tibble(
#         period  = p,
#         sample  = s,
#         element = factor(element_labels, levels = element_labels),
#         value   = vec
#       )
#     })
#   })
# }
# 
# # ── Summary ───────────────────────────────────────────────────────────────────
# summarise_vector <- function(df) {
#   df %>%
#     group_by(period, element) %>%
#     summarise(
#       median  = median(value, na.rm = TRUE),
#       p2.5  = quantile(value, 0.025, na.rm = TRUE),
#       p97.5 = quantile(value, 0.975, na.rm = TRUE),
#       .groups = "drop"
#     )
# }
# 
# # ── Plot helper ───────────────────────────────────────────────────────────────
# plot_vector_summary <- function(df, title) {
#   df %>%
#     ggplot(aes(x = period, y = median, group = element, color = element, fill = element)) +
#     geom_ribbon(aes(ymin = p2.5, ymax = p97.5), alpha = 0.2, color = NA) +
#     geom_line(linewidth = 0.8) +
#     geom_point(size = 1.5) +
#     scale_x_continuous(breaks = 1:opt$nperiods) +
#     scale_color_manual(values = pal) +
#     scale_fill_manual(values = pal) +
#     labs(
#       title = title,
#       x     = "Period",
#       y     = "Value",
#       color = "Element",
#       fill  = "Element"
#     ) +
#     theme_minimal(base_size = 11) +
#     theme(
#       panel.grid.minor = element_blank(),
#       legend.position  = "bottom"
#     )
# }
# 
# pal=  c("#FFF2B2",
#         "#FFD772",
#         "#F9B74C",
#         "#DFA144",
#         "#A6B96A",
#         "#6FAF91",
#         "#4F9FAE",
#         "#2B7F8E")
# 
# # ── Extract ───────────────────────────────────────────────────────────────────
# q_susc_long     <- extract_vector("q_susc",     config$study$age_groups)
# cum_elasti_long <- extract_vector("cum_elasti", config$study$age_groups)
# 
# # ── Summarise ───────────────────────────────────────────────────────────────────
# q_susc_summary   <- summarise_vector(q_susc_long)
# elas_set_summary <- summarise_vector(cum_elasti_long)
# 
# # ── Plot ──────────────────────────────────────────────────────────────────────
# plot_vector_summary(q_susc_summary,     "q_susc — Median with 95% CI across samples")
# plot_vector_summary(elas_set_summary, "cum_elasti — Median with 95% CI across samples")
