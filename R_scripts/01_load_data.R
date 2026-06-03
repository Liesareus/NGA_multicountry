# script to obtain contact data for a specific country

load_data=function(country,contacts_path,periods){
  
  # reads contact data for a given country
  participant = read.csv(paste0(contacts_path,paste0(country,"_participant.csv")))
  
  if(country=="UK"){
  # necessary separation of age ranges in the UK
  participant %>%
     separate(part_age, into = c("part_age_est_min", "part_age_est_max"), sep = "-", convert = TRUE) -> participant
  participant$part_age_est_min <- as.numeric(participant$part_age_est_min)
  participant$part_age_est_max <- as.numeric(participant$part_age_est_max)
  participant$part_age_exact <- NA
  }
  
  if(country=="BE"){
  # necessary separation of age ranges in BE and cleaning of brackets
    participant %>%
      separate(part_age,
               into = c("part_age_est_min", "part_age_est_max"),
               sep = ",") %>%
      mutate(
        part_age_est_min = gsub("\\[", "", part_age_est_min),
        part_age_est_max = gsub("\\)", "", part_age_est_max),
        part_age_est_min = as.numeric(part_age_est_min),
        part_age_est_max = as.numeric(part_age_est_max)
      )-> participant
  participant$part_age_exact <- NA
  }
  
  if(country=="NL"){
    # rename part_age to part_age_exact
    participant <- participant %>%
      rename(part_age_exact = part_age)
  }
  
  # include the country name and save aux variable for country selected
  if(country=="UK"){
    participant$country=aux_country
  } else if(country=="BE"){
    participant$country=aux_country
  } else if(country=="NL"){
    participant$country=aux_country
  }
  
  # load contact data
  contact = read.csv(paste0(contacts_path,paste0(country,"_contact.csv")))
  
  # load sday data
  sday = read.csv(paste0(contacts_path,paste0(country,"_sday.csv")))
  sday$sday_id = as.Date(sday$sday_id, format = "%Y.%m.%d")
  sday$year= year(sday$sday_id)
  
  # join participant and sday data
  participant = participant %>%
    left_join(sday, by = c("part_id" = "part_id"))
  
  # join period data with participant data
  participant <- fuzzy_left_join(
    participant, periods,
    by = c("sday_id" = "start", "sday_id" = "end"),
    match_fun = list(`>=`, `<=`)
  )
  
  # check if all periods are represented in the participant data
  if (all(periods$period %in% participant$period) == FALSE) {
    stop("The study periods do not match the participant dates. Please check the period data.")
  }

  # filter the participant for the selected periods
  participant <- participant %>%
    filter(!is.na(period))
  
  return(contact_survey=list(country=country,
                             participants=participant,
                             contacts=contact))
  
}


