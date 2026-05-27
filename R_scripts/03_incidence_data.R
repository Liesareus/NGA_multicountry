# script to obtain incidence data for a specific country


get_incidence=function(country,incidence_path,age_groups){
  # UK specific data cleaning
  if(country=="UK"){
  
    # reads incidence data for a given country
    incidence = openxlsx::read.xlsx(paste0(incidence_path,paste0(country,"_incidence.xlsx")), sheet = "Figure1.C")
    
    # remove unnecessary columns
    incidence$X1=NULL
    
    # convert numeric dates to date format
    incidence$date = as.Date(incidence$date, origin = "1899-12-30")
    
    # substitute on variable age_group: where 43739 should be 10-19
    incidence$age_group[incidence$age_group == 43739] <- "10-19"
    
    # rename age_group to agegroup, rolling_cases to weekly_cases 
    colnames(incidence)[colnames(incidence) == "age_group"] <- "agegroup"
    colnames(incidence)[colnames(incidence) == "rolling_cases"] <- "cases"
  }
  
  if(country=="BE"){
    # reads incidence data for a given country
    incidence = read.csv(paste0(incidence_path,paste0(country,"_incidence.csv")), header = TRUE)
    
    # convert numeric dates to date format
    incidence$date = as.Date(incidence$DATE)
    
    # remove individuals with NA age group
    incidence %>% filter(!is.na(AGEGROUP)) -> incidence
    
    incidence %>% mutate(agegroup_gathered = case_when(
      AGEGROUP %in% c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69") ~ AGEGROUP,
      AGEGROUP %in% c("70-79", "80-89", "90+") ~ "70+")) -> incidence
    
    incidence %>% group_by(date, agegroup_gathered) %>% summarise(cases = sum(CASES)) -> incidence
    

    # rename age_group to agegroup, rolling_cases to weekly_cases 
    colnames(incidence)[colnames(incidence) == "agegroup_gathered"] <- "agegroup"
    colnames(incidence)[colnames(incidence) == "cases"] <- "cases"
  }
  
  if(country=="NL"){
    incidence = read.csv(paste0(incidence_path,paste0(country,"_incidence.csv")), header = TRUE, sep = ";")
    incidence$date = as.Date(incidence$Date_statistics, format="%Y-%m-%d")
    incidence %>% filter(!(Agegroup %in% c("<50","Unknown"))) -> incidence
    
    incidence %>% mutate(agegroup_gathered = case_when(
      Agegroup %in% c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69") ~ Agegroup,
      Agegroup %in% c("70-79", "80-89", "90+") ~ "70+")) -> incidence
    
    # Define full support
    all_dates <- seq(min(incidence$date), max(incidence$date), by = "day")
    all_ages  <- unique(incidence$agegroup_gathered)
    
    incidence <- incidence %>%
      count(date, agegroup_gathered, name = "cases") %>%
      complete(
        date = all_dates,
        agegroup_gathered = all_ages,
        fill = list(cases = 0)
      )
    
    colnames(incidence)[colnames(incidence) == "agegroup_gathered"] <- "agegroup"
    colnames(incidence)[colnames(incidence) == "cases"] <- "cases"
  }
  
  if(!all(age_groups==as.character(unique(incidence$agegroup)))){
    stop("Age groups in the config do not match the age groups in the incidence data")
  }
  incidence$agegroup = factor(incidence$agegroup, levels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"))
  
  return(incidence)
}