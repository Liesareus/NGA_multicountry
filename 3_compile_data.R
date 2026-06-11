# -------------------------------
# compile all data for the main analysis, including: nga results, measure data, vaccination data, variant data
# -------------------------------

# Load options and config -------------------------------------------------------------
source("0_options.R")
config <- yaml::read_yaml("study_config.yml") # config file with all parameters
readRDS(file.path(config$data_processed, paste0("periods_",opt$country,"_20_2020-12-25_to_2021-09-17.rds"))) -> periods 

# Load processed data -------------------------------------------------------------
readRDS(file.path(config$results_path, paste0("result_summary_",opt$country,"_20_2020-12-25_to_2021-09-17.rds"))) -> results_summary
# -------------------------------
# 1. Measure data
# -------------------------------

# load oxford covid government response tracker data
measures = read.csv(paste0(config$data_paths$measures,paste0("OxCGRT_compact_national_v1.csv"))) 
measures <- measures %>% filter(CountryName == aux_country) # filter to country in study
measures$Date <- as.Date(as.character(measures$Date), format = "%Y%m%d") # transform dates in as.Date  
measures %>% filter(Date>=min(periods$start) & Date<=max(periods$end))->measures  # filter to the period in study
str(measures)

measures %>% select(CountryName,
                    Date,
                    C1M_School.closing,
                    C1M_Flag,
                    C2M_Workplace.closing,
                    C2M_Flag,
                    C3M_Cancel.public.events,
                    C3M_Flag,
                    C4M_Restrictions.on.gatherings,
                    C4M_Flag,
                    C5M_Close.public.transport,
                    C5M_Flag,
                    C6M_Stay.at.home.requirements,
                    C6M_Flag,
                    H2_Testing.policy,
                    H6M_Facial.Coverings,
                    H6M_Flag,
                    H8M_Protection.of.elderly.people,
                    H8M_Flag,
                    MajorityVaccinated,
                    StringencyIndex_Average)->measures

measures$CountryName=NULL
str(measures)

## add the period when schools were closed during summer holidays in 2021 (see https://op.europa.eu/en/publication-detail/-/publication/7260fb98-0dcc-11eb-bc07-01aa75ed71a1/language-en)
if (opt$country=="UK") {
  measures <- measures %>%
    mutate(C1M_School.closing_2 = ifelse(Date >= as.Date("2021-07-26") & Date <= as.Date("2021-09-06"), 4, C1M_School.closing), # 6 weeks between midjuly and first week of september 2021
           C1M_Flag_2 = ifelse(Date >= as.Date("2021-07-26") & Date <= as.Date("2021-09-06"), 1, C1M_Flag))  
}

# check the new variables
View(measures %>% select(C1M_School.closing,C1M_School.closing_2,C1M_Flag, C1M_Flag_2))

# Aggregate these measures to the period level 
measures_period <- measures %>%
  mutate(period = cut(Date, breaks = c(periods$start, max(periods$end)+1), labels = periods$period))

# select stringency index, the school closure variable and majority vaccinated for the main analysis
measures_period %>% select(period,
                    C1M_School.closing_2,
                    C1M_Flag_2,
                    StringencyIndex_Average,
                    MajorityVaccinated) -> measures_period


# function to calculate the mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
measures_period$MajorityVaccinated = factor(measures_period$MajorityVaccinated, levels = c("NV","V"), labels = c("NV","V"))
str(measures_period)
# calculate the mean for the stringency index, the majority vaccinated and the school closure variable (taking into account the flag) for each period
measures_period_agg <- measures_period %>%
                         group_by(period) %>%
                         summarise(school_closing_mode = Mode(C1M_School.closing_2),
                                   school_closing_flag_mode = Mode(C1M_Flag_2),
                                   stringency_index_mean = mean(StringencyIndex_Average, na.rm = TRUE),
                                   majority_vaccinated_mode = Mode(MajorityVaccinated))
measures_period_agg$period=as.numeric(measures_period_agg$period)
str(measures_period_agg)
# Aggreagate the measures with the results_summary
results_summary <- results_summary %>%
  left_join(measures_period_agg, by = "period")

