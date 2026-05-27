# script to harmonize incidence, rt and conctact data

harmonize_data = function(measures_path,country,contact_mats, incidence, rt, periods){

  # Calculate rt as the mean value for each period
  rt <- fuzzy_left_join(
    rt, periods,
    by = c("date" = "start", "date" = "end"),
    match_fun = list(`>=`, `<=`)
  )
  
  rt %>% filter(!is.na(period))->rt
  
  rt %>% group_by(period) %>%
    summarise(rt_mean = mean(Rt, na.rm = TRUE))-> rt
  
  rt <- split(rt$rt_mean, rt$period)
  
  # Calculate relative incidence for each period
  incidence <- fuzzy_left_join(
    incidence, periods,
    by = c("date" = "start", "date" = "end"),
    match_fun = list(`>=`, `<=`)
  )
  
  incidence <- filter(incidence, !is.na(period))
  
  incidence <- incidence %>%
    group_by(period, agegroup) %>%
    summarise(incidence = sum(cases, na.rm = TRUE)) %>%
    group_by(period) %>%
    mutate(rel_incidence = incidence / sum(incidence, na.rm = TRUE)) %>%
    ungroup() %>%
    select(period, agegroup, rel_incidence)
    
  incidence <- split(incidence[,-1], incidence$period)

  # extract only the necessary information to run the analysis
  contact_mats <- lapply(contact_mats, function(sublist) sublist[c(1, 4, 7, 10, 13, 16, 19)])
  
  
  # obtain measures data and calculate the weights to adjust the contact matrices by setting
  measures = read.csv(paste0(measures_path,paste0("OxCGRT_compact_national_v1.csv")))
  
  if(country=="UK"){
    aux_country="United Kingdom"
  } else if(country=="BE"){
    aux_country="Belgium"
  } else if(country=="NL"){
    aux_country="Netherlands"
  }
  
  # Select only the desired country
  measures <- measures %>%
    filter(CountryName %in% c(aux_country))
  
  # transform dates in as.Date  
  measures$Date <- as.Date(as.character(measures$Date), format = "%Y%m%d")
  
  # Remove region name, region code and jurisdiction name
  measures <- measures %>%
    select(-RegionName, -RegionCode, -Jurisdiction)
  
  
  measures %>% filter(Date>=min(periods$start) & Date<=max(periods$end))->measures  # filter to the period in study
  
  measures %>% select(CountryName,CountryCode,Date,H6M_Facial.Coverings,H6M_Flag)-> measures # select only variables related to the use of mask                                       
  
  # H6M_Facial.Coverings: Facial Covering policies
  # 0: No policy
  # 1: Recommended
  # 2: Required in some specified shared/public spaces outside the home with other people present, or some situations when social distancing not possible
  # 3: Required in all shared/public spaces outside the home with other people present, or all situations when social distancing not possible
  # 4: Required outside the home at all times regardless of location or presence of other people
  
  # 3,4 -> weights should affect all settings except home and leisure (where the effect of mask wearing is likely to be less pronounced)
  
  # H6M_Flag: Facial Covering policies flag
  # 0: No data
  # 1: General policy applies to the entire country
  # 2: General policy applies to some regions of the country
  
  # 1 -> weights only apply in this setting
  
  measures <- fuzzy_left_join(
    measures, periods,
    by = c("Date" = "start", "Date" = "end"),
    match_fun = list(`>=`, `<=`)
  )
  
  measures %>% group_by(period) %>%
    summarise(effect_adjust = (sum(H6M_Flag)/(sum(H6M_Flag)+sum(H6M_Flag==0))))-> measures  # calculate the proportion of days in the period with a facial covering policy in place to adjust their overall effect
  
  measures %>% mutate(weight = 1-effect_adjust*config$study$m_effect)-> measures
  measures %>% select(period, weight)-> measures
  weights <- split(measures$weight, measures$period)
  # combine outputs
  
  combined <- Map(function(lst,df,num1,num2){
    list(contacts=lst,incidence=df,rt=num1,weights=num2)
  },contact_mats,incidence,rt,weights)
  
  return(combined)

}

