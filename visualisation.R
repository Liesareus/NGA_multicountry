
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
  library(cowplot)
})

# -------------------------------
# 1. Source pipeline scripts
# -------------------------------
source("R_scripts/07_build_df.R")
source("R_scripts/08_plots.R")

# -------------------------------
# 2. read model data
# -------------------------------

config <- yaml::read_yaml("study_config.yml") # config file with all parameters
country="UK" # change country here"

if(country=="UK"){
  aux_country="United Kingdom"
} else if(country=="BE"){
  aux_country="Belgium"
} else if(country=="NL"){
  aux_country="Netherlands"
}


readRDS(file.path(config$data_processed, paste0("inputs_",country,"_20_2020-12-25_to_2021-09-17.rds"))) -> inputs      # model inputs
readRDS(file.path(config$results_path, paste0("results_",country,"_20_2020-12-25_to_2021-09-17.rds"))) -> results      # model results
readRDS(file.path(config$data_processed, paste0("periods_",country,"_20_2020-12-25_to_2021-09-17.rds"))) -> periods     # periods of interest
periods
# -------------------------------
# 3. Diagnostics
# -------------------------------

pal=  c(
  "[0,10)" = "#FFF2B2",   # light gold
  "[10,20)" = "#FFD772",  # brighter golden yellow
  "[20,30)" = "#F9B74C",  # warm amber
  "[30,40)" = "#DFA144",  # muted orange-gold transitioning
  "[40,50)" = "#A6B96A",  # olive–green transition
  "[50,60)" = "#6FAF91",  # desaturated sea-green
  "[60,70)" = "#4F9FAE",  # mid teal
  "70+"     = "#2B7F8E"   # deep teal
)

# period labels
periods_duration = periods$end-periods$start+1
halfpoint = periods$start + periods_duration[1]/2

## incidence
incidence_list = list()
for(i in 1:length(inputs)){
 incidence_list[[i]] = inputs[[i]]$incidence
}
merge_incidence = bind_rows(incidence_list,.id="period")
merge_incidence$period=as.numeric(merge_incidence$period)
levels(merge_incidence$agegroup) <- c("[0,10)", "[10,20)", "[20,30)","[30,40)", "[40,50)","[50,60)", "[60,70)", "70+")

ggplot(merge_incidence, aes(x=period, y=rel_incidence, color=agegroup)) +
  geom_line(linewidth=1) +
  geom_point(size=2)+
  labs(title="Relative Incidence by age group and period",
       x="Period (halfpoint)",
       y="Relative incidence",
       color="Age group") +
  scale_x_continuous(
    breaks = seq(1,nrow(periods)),
    labels = halfpoint
  ) +
  scale_color_manual(values = pal)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1.5))

## contact matrices
contact_mats_list = list()
for(i in 1:length(inputs)){
  contact_mats_list[[i]] = inputs[[i]]$contacts$matrix_sample_total
}

# convert the list of contact matrices into a dataframe using the melt function
contact_mats_df = bind_rows(lapply(contact_mats_list, function(mat) {
  melt(mat)
}), .id = "period")
contact_mats_df$L1=NULL
contact_mats_df$period=as.numeric(contact_mats_df$period)

ggplot(contact_mats_df, aes(x=contact.age.group, y=age.group)) +
  geom_tile(aes(fill=value), color="white") +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "Spectral")),limits=c(0,7)) +
  labs(fill="contacts") +
  theme_minimal() +
  labs(title="Contact Matrix", x="Age group", y="Contact age group") +
  facet_wrap(~period) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")

## contact matrices (other)
contact_mats_list = list()
for(i in 1:length(inputs)){
  contact_mats_list[[i]] = inputs[[i]]$contacts$matrix_sample_other
}

# convert the list of contact matrices into a dataframe using the melt function
contact_mats_df = bind_rows(lapply(contact_mats_list, function(mat) {
  melt(mat)
}), .id = "period")
contact_mats_df$L1=NULL
contact_mats_df$period=as.numeric(contact_mats_df$period)

ggplot(contact_mats_df, aes(x=contact.age.group, y=age.group)) +
  geom_tile(aes(fill=value), color="white") +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "Spectral")),limits=c(0,0.5)) +
  labs(fill="contacts") +
  theme_minimal() +
  labs(title="Contact Matrix (other)", x="Age group", y="Contact age group") +
  facet_wrap(~period) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")+
  theme(legend.position = "right")

## rt
# extract the rt values along with each period
rt_list = list()
for(i in 1:length(inputs)){
  rt_list[[i]] = inputs[[i]]$rt
}
rt_df <- data.frame(
      period = seq_along(rt_list),
      rt = unlist(rt_list)
)
ggplot(rt_df, aes(x=period, y=rt)) +
  geom_line(linewidth=1) +
  geom_point(size=2)+
  geom_hline(yintercept=1, linetype="dashed", color="red") +
  labs(title="Rt by period",
       x="Period (halfpoint)",
       y="Rt") +
  scale_x_continuous(
    breaks = seq(1,nrow(periods)),
    labels = halfpoint
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1.5))

## qsusceptibility
age_groups=c("[0,10)", "[10,20)", "[20,30)","[30,40)", "[40,50)","[50,60)", "[60,70)", "70+")
qsusc_list = list()


for(i in 1:length(results)){
  qsusc_list[[i]]=data.frame(agegroups=age_groups,qsusc=unlist(results[[i]]$q_susc))
}
merge_qsusc = bind_rows(qsusc_list,.id="period")
merge_qsusc$period=as.numeric(merge_qsusc$period)

ggplot(merge_qsusc, aes(x=period, y=qsusc, color=agegroups)) +
  geom_line(linewidth=1) +
  geom_point(size=2)+
  labs(title="q-susceptibility by age group and period",
       x="Period (halfpoint)",
       y="Relative incidence",
       color="Age group") +
  scale_x_continuous(
    breaks = seq(1,nrow(periods)),
    labels = halfpoint
  ) +
  scale_y_continuous(limits=c(0,2))+
  scale_color_manual(values = pal)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1.5))

## NGM
ngm_mats_list = list()
for(i in 1:length(results)){
  ngm_mats_list[[i]] = results[[i]]$ngm
}

# convert the list of contact matrices into a dataframe using the melt function
ngm_df = bind_rows(lapply(ngm_mats_list, function(mat) {
  melt(mat)
}), .id = "period")
ngm_df$L1=NULL
ngm_df$period=as.numeric(ngm_df$period)
ngm_df %>% rename(age.group=Var1, contact.age.group=Var2, value=value)->ngm_df

ggplot(ngm_df, aes(x=contact.age.group, y=age.group)) +
  geom_tile(aes(fill=value), color="white") +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "Spectral")),limits=c(0,1.2)) +
  labs(fill="infections") +
  theme_minimal() +
  labs(title="NGM", x="Contact age group", y="Age group") +
  facet_wrap(~period) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")


# -------------------------------
# 4. Model results
# -------------------------------



#### Age groups

# Build data frames for plotting
df_age = build_final_df(inputs = inputs,
                        results = results,
                        x = "cum_elasti")  # final df with age group elasticities
df_age %>% rename(age_group=`cum_elasti`)->df_age

# Plots
l_age_p=plot_elas(df=df_age,
                  x="age_group",
                  quadrant_limit=1/8,
                  y_label="Median proportional contribution by age group \n",
                  title=expression("Age-specific proportional contribution to " * R[e]),
                  periods=periods)

l_age_p

#### Settings
df_setting = build_final_df(inputs = inputs,
                            results = results,
                            x = "elas_set")  # final df with setting elasticities
df_setting %>% rename(setting=`elas_set`)->df_setting

# recode setting into a factor with numerical values
unique(df_setting$setting)
df_setting %>% mutate(setting_X = case_when(
  setting == "E_home" ~ 6,
  setting == "E_work" ~ 5,
  setting == "E_school" ~ 4,
  setting == "E_transport" ~ 3,
  setting == "E_leisure" ~ 2,
  setting == "E_other" ~ 1
)) -> df_setting

df_setting$setting_X <- factor(df_setting$setting_X, levels = 1:6, labels = c("Other", "Leisure", "Transport", "School", "Work", "Home"))
# validate
table(df_setting$setting, df_setting$setting_X)

# remove old variable
df_setting$setting=df_setting$setting_X
df_setting$setting_X=NULL



str(df_setting)
l_setting_p = plot_elas(df=df_setting,
                        x="setting",
                        quadrant_limit=1/6,
                        y_label="Median proportional contribution by setting \n",
                        title=expression("Setting-specific contribution to " * R[e] * " (United Kingdom)"),
                        periods=periods)
l_setting_p


# -------------------------------
# 5. Measures
# -------------------------------

measures = read.csv(paste0(config$data_paths$measures,paste0("OxCGRT_compact_national_v1.csv"))) # load measures data

measures <- measures %>% filter(CountryName %in% c("United Kingdom", "Belgium", "Netherlands"))
measures$Date <- as.Date(as.character(measures$Date), format = "%Y%m%d") # transform dates in as.Date  


measures %>% filter(Date>=min(periods$start) & Date<=max(periods$end))->measures  # filter to the period in study
str(measures)

measures %>% 
  filter(CountryName==aux_country)->measures # change country here

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

measures %>% select(Date, C1M_School.closing, C1M_Flag)->measures_school
measures_school$yname= "school"

## add the period when schools were closed during summer holidays in 2021
if (country=="UK") {
measures_school <- measures_school %>%
    mutate(C1M_School.closing_SH = ifelse(Date >= as.Date("2021-07-23") & Date <= as.Date("2021-08-31"), 4, C1M_School.closing),
           C1M_Flag_SH = ifelse(Date >= as.Date("2021-07-23") & Date <= as.Date("2021-08-31"), 1, C1M_Flag))  
}

## add the period when schools were closed during summer holidays in 2021
if (country=="NL") {
  measures_school <- measures_school %>%
    mutate(C1M_School.closing_SH = ifelse(Date >= as.Date("2021-07-24") & Date <= as.Date("2021-08-22"), 4, C1M_School.closing),
           C1M_Flag_SH=ifelse(Date >= as.Date("2021-07-24") & Date <= as.Date("2021-08-22"), C1M_Flag,C1M_Flag))  # NOT WORKING!!!
           # C1M_Flag_SH = case_when(
           #   Date >= as.Date("2021-07-10") & Date <= as.Date("2021-07-24") ~ 0,
           #   Date >= as.Date("2021-07-25") & Date <= as.Date("2021-08-21") ~ 1,
           #   Date >= as.Date("2021-08-22") & Date <= as.Date("2021-09-05") ~ 0,
           #   Date < as.Date("2021-07-10") ~ C1M_Flag,
           #   Date > as.Date("2021-09-05") ~ C1M_Flag,
           # ))  
}

## add the period when schools were closed during summer holidays in 2021
if (country=="BE") {
  measures_school <- measures_school %>%
    mutate(C1M_School.closing_SH = ifelse(Date >= as.Date("2021-07-01") & Date <= as.Date("2021-08-31"), 4, C1M_School.closing),
           C1M_Flag_SH=ifelse(Date >= as.Date("2021-07-01") & Date <= as.Date("2021-08-31"), C1M_Flag,C1M_Flag))  # NOT WORKING!!!
  # C1M_Flag_SH = case_when(
  #   Date >= as.Date("2021-07-10") & Date <= as.Date("2021-07-24") ~ 0,
  #   Date >= as.Date("2021-07-25") & Date <= as.Date("2021-08-21") ~ 1,
  #   Date >= as.Date("2021-08-22") & Date <= as.Date("2021-09-05") ~ 0,
  #   Date < as.Date("2021-07-10") ~ C1M_Flag,
  #   Date > as.Date("2021-09-05") ~ C1M_Flag,
  # ))  
}


p_school=ggplot()+
  
  # closure heatmap
  geom_raster(data=measures_school, aes(x=Date,y=yname, fill=factor(C1M_School.closing_SH)))+
  geom_point(data=measures_school %>% filter(C1M_Flag_SH==0), aes(x=Date,y=yname,shape=factor(C1M_Flag_SH)),size=4,alpha=0.3 ,color="black",show.legend = FALSE)+
  scale_fill_manual(values=c(
    "0" = "#E8E8E8",  # very light grey
    "1" = "#FFF2B2",  # pastel yellow
    "2" = "#FFD8A8",  # pastel orange
    "3" = "#FFB3B3",  # soft pastel red
    "4" = "darkgrey"   # light grey (holiday)
  ),
                    breaks=c("0", "1", "2", "3","4"),
                    labels=c(
                      "0" = "No measures",
                      "1" = "Recommend closing",
                      "2" = "Require closing (some levels)",
                      "3" = "Require closing all levels",
                      "4" = "School closure for summer holidays"
                    ),
                    name="School closure level") +
  scale_shape_manual(values=c("0"=3),
                        labels=c("0"="targeted"),
                        name="Flag for geographic scope") +
  labs(x = "Date",
       y = "") +
  theme_cowplot()+
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week",expand=c(0,0)) +
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(angle = 90))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )+
  theme(
    legend.key.width = unit(1, "cm")
  )+
  theme(
    legend.text  = element_text(size = 5),
    legend.key.size = unit(0.15, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0),
    legend.title = element_blank()
  )

measures %>% select(Date, C2M_Workplace.closing, C2M_Flag)->measures_workplace
measures_workplace$yname= "workplace"

p_work=ggplot()+
  
  # closure heatmap
  geom_raster(data=measures_workplace, aes(x=Date,y=yname, fill=factor(C2M_Workplace.closing)))+
  geom_point(data=measures_workplace %>% filter(C2M_Flag==0), aes(x=Date,y=yname,shape=factor(C2M_Flag)),size=4,alpha=0.3 , color="black",show.legend = FALSE)+
  scale_fill_manual(values=c(
    "0" = "#E8E8E8",  # very light grey
    "1" = "#FFF2B2",  # pastel yellow
    "2" = "#FFD8A8",  # pastel orange
    "3" = "#FFB3B3",  # soft pastel red
    "4" = "darkgrey"   # light grey (holiday)
  ),
                    breaks=c("0", "1", "2", "3"),
                    labels=c(
                      "0" = "No measures",
                      "1" = "Recommend closing",
                      "2" = "Require closing (some sectors)",
                      "3" = "Require closing (except essential workers)"
                    ),
                    name="Workplace closure level") +
  scale_shape_manual(values=c("0"=3),
                     labels=c("0"="targeted"),
                     name="Flag for geographic scope") +
  labs(x = "Date",
       y = "") +
  theme_cowplot()+
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 week",expand=c(0,0)) +
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(angle = 90))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )+
  theme(
    legend.key.width = unit(1, "cm")
  )+
  theme(
    legend.text  = element_text(size = 5),
    legend.key.size = unit(0.15, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0),
    legend.title = element_blank()
  )

measures %>% select(Date, C4M_Restrictions.on.gatherings, C4M_Flag)->measures_gatherings
measures_gatherings$yname= "gatherings"

p_gatherings=ggplot()+
  
  # closure heatmap
  geom_raster(data=measures_gatherings, aes(x=Date,y=yname, fill=factor(C4M_Restrictions.on.gatherings)))+
  geom_point(data=measures_gatherings %>% filter(C4M_Flag==0), aes(x=Date,y=yname,shape=factor(C4M_Flag)),size=4,alpha=0.3 , color="black")+
  scale_fill_manual(values=c(
    "0" = "#E8E8E8",  # very light grey
    "1" = "#FFF2B2",  # pastel yellow
    "2" = "#FFD8A8",  # pastel orange
    "3" = "#FFB3B3",  # soft pastel red
    "4" = "#FF9999"   # light grey (holiday)
  ),
                    breaks=c("0", "1", "2", "3","4"),
                    labels=c(
                      "0" = "No restrictions",
                      "1" = "Large gathering restrictions (>1000)",
                      "2" = "Restrictions on gatherings between 101-1000 people",
                      "3" = "Restrictions on gatherings between 11-100 people",
                      "4" = "Restrictions on gatherings of <11 people"
                    ),
                    name="Gatherings restrictions") +
  scale_shape_manual(values=c("0"=3),
                     labels=c("0"="targeted"),
                     name="Flag for geographic scope") +
  labs(x = "Date",
       y = "") +
  theme_cowplot()+
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 week",expand=c(0,0)) +
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(angle = 90))+
  theme(
    legend.key.width = unit(1, "cm")
  )+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )+
  theme(
    legend.text  = element_text(size = 5),
    legend.key.size = unit(0.15, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0),
    legend.title = element_blank()
  )


measures %>% select(Date, H2_Testing.policy)->measures_testing
measures_testing$yname= "testing"

p_testing=ggplot()+
  
  # closure heatmap
  geom_raster(data=measures_testing, aes(x=Date,y=yname, fill=factor(H2_Testing.policy)))+
  scale_fill_manual(values=c("0"="#deebf7", "1"="#9ecae1", "2"="#3182bd", "3"="#08519c"),
                    breaks=c("0", "1", "2", "3"),
                    labels=c(
                      "0" = "No testing policy",
                      "1" = "Symptoms and testing for those who meet criteria",
                      "2" = "Testing of anyone showing COVID-19 symptoms",
                      "3" = "Open public testing (including asymptomatic people)"
                    ),
                    name="Testing policy") +
  labs(x = "Date",
       y = "") +
  theme_cowplot()+
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 week",expand=c(0,0)) +
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(angle = 90))+
  theme(
    legend.key.width = unit(1, "cm")
  )+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )+
  theme(
    legend.text  = element_text(size = 5),
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0),
    legend.title = element_blank()
  )


measures %>% select(Date, H6M_Facial.Coverings, H6M_Flag)->measures_facial_coverings
measures_facial_coverings$yname= "facial coverings"

p_facial_coverings=ggplot()+
  
  # closure heatmap
  geom_raster(data=measures_facial_coverings, aes(x=Date,y=yname, fill=factor(H6M_Facial.Coverings)))+
  geom_line(data=measures_facial_coverings, aes(x=Date,y=yname,linetype=factor(H6M_Flag)), color="black", size=1.2)+
  scale_fill_manual(values=c("0"="#D3D3D3", "1"="#FFD700", "2"="#FFA500", "3"="#FF4500", "4"="#FF0000"),
                    breaks=c("0", "1", "2", "3","4"),
                    labels=c(
                      "0" = "No policy",
                      "1" = "Recommended",
                      "2" = "Required in some specified/shared public spaces",
                      "3" = "Required in all shared/public spaces",
                      "4" = "Required outside the home at all times"
                    ),
                    name="facial convering policy") +
  scale_linetype_manual(values=c("1"="solid", "0"="dashed"),
                        breaks=c("0", "1"),
                        labels=c("0"="targeted", "1"="general"),
                        name="Flag for geographic scope") +
  labs(x = "",
       y = "") +
  theme_cowplot()+
  scale_x_date(date_labels = "%Y-%m-%d", breaks = seq(min(measures_facial_coverings$Date),max(measures_facial_coverings$Date),15),expand=c(0,0)) +
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  theme(
    legend.key.width = unit(1, "cm")
  )+
  theme(
    legend.text  = element_text(size = 5),
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0),
    legend.title = element_blank()
  )+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )


#### owid case date
owid_cases = read.csv(paste0(config$data_paths$incidence,"/owid_cases.csv")) # load owid cases data
owid_cases$date <- as.Date(owid_cases$Day) # transform dates in as.Date
owid_cases %>% filter(Entity==aux_country)->owid_cases # change country here
owid_cases %>% filter(date>=min(periods$start) & date<=max(periods$end))->owid_cases  # filter to the period in study
owid_cases %>% select(date, New.cases..per.1M.)->owid_cases

# second axis stuff
max_cases <- max(owid_cases$New.cases..per.1M., na.rm = TRUE)
scale_factor <- 100 / max_cases

p_stri = ggplot() +
  geom_line(data=owid_cases, aes(x=date, y=New.cases..per.1M.* scale_factor,color="Incidence per 1M"), size=1.2) +
  geom_ribbon(data=owid_cases, aes(x=date, ymin=0, ymax=New.cases..per.1M.* scale_factor), fill="#B22222", alpha=0.3) +
  geom_line(data=measures, aes(x=Date, y=StringencyIndex_Average,color="Stringency Index"),size=1.2) +
  labs(x="",
       y="Stringency \n Index") +
  theme_cowplot()+
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week",expand=c(0,0)) +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,90,30),sec.axis = sec_axis(~ . / scale_factor,
                        name = "Incidence \n per 1M",breaks=seq(0,700,140)),expand=c(0,0)) +
  scale_color_manual(values=c("Stringency Index"="#1F4E79", "Incidence per 1M"="#B22222"))+
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  theme(
    legend.key.width = unit(1, "cm")
  )+
  theme(
    axis.text.y  = element_text(size = 8),
    axis.title.y = element_text(size = 9),
    legend.text  = element_text(size = 5),
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0),
    legend.title = element_blank()
  )+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

## figure with just the dates
periods_duration = periods$end-periods$start+1
halfpoint = periods$start + periods_duration[1]/2

p_dates=ggplot(data.frame(x = seq(1,length(halfpoint)+1)), aes(x = x, y = 0)) +
  geom_blank() +
  theme_cowplot()+
  scale_x_continuous(
    breaks = seq(1,nrow(periods)+1),
    labels = c(periods$start, max(periods$start+14)),
    expand = c(0, 0)
  ) +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  labs(x="")+
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid   = element_blank(),
    axis.line.y  = element_blank(),
    axis.line.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

## variant
# assume variant is dominant >60% cases
# data from owid variants
measures_variants_UK = data.frame(date=measures_school$Date,country="United Kingdom")
measures_variants_UK %>% mutate(variant=case_when(
  date >= min(periods$start) & date < as.Date("2021-05-24") ~ 1, 
  date >= as.Date("2021-05-24") ~ 2
))->measures_variants_UK


measures_variants_BE = data.frame(date=measures_school$Date,country="Belgium")
measures_variants_BE %>% mutate(variant=case_when(
  date >= min(periods$start) & date < as.Date("2021-03-01") ~ 0, 
  date >= as.Date("2021-03-01") & date < as.Date("2021-07-05") ~ 1,
  date >= as.Date("2021-07-05") ~ 2
))->measures_variants_BE

measures_variants_NL = data.frame(date=measures_school$Date,country="Netherlands")
measures_variants_NL %>% mutate(variant=case_when(
  date >= min(periods$start) & date < as.Date("2021-02-15") ~ 0, 
  date >= as.Date("2021-02-15") & date < as.Date("2021-06-21") ~ 1,
  date >= as.Date("2021-06-21") ~ 2
))->measures_variants_NL

measures_variants = rbind(measures_variants_UK, measures_variants_BE, measures_variants_NL)
measures_variants$variant <- factor(measures_variants$variant, levels = c(0, 1, 2), labels = c("Pre-alpha", "Alpha", "Delta"))
measures_variants$yname = "variants"
measures_variants %>% filter(country==aux_country)->measure_variants # change country here

p_variants=ggplot()+
  
  # closure heatmap
  geom_raster(data=measures_variants, aes(x=date,y=yname, fill=factor(variant)),show.legend = F)+
  annotate("text", x = as.Date("2021-01-20"), y = 1, label = "Pre-alpha", color = "black", size = 5) +
  annotate("text", x = as.Date("2021-04-15"), y = 1, label = "Alpha", color = "black", size = 5) +
  annotate("text", x = as.Date("2021-08-15"), y = 1, label = "Delta", color = "black", size = 5) +
  scale_fill_manual(values=c(
    "Pre-alpha" = "#8ED1C6",  # very light grey
    "Alpha" = "#C6B7E2",  # pastel yellow
    "Delta" = "#9EC9E2"  # pastel orange
  ),name="Variant")+
  labs(x = "Date",
       y = "") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week",expand=c(0,0)) +
  theme_cowplot()+
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(angle = 90))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )+
  theme(
    legend.key.width = unit(1, "cm")
  )+
  theme(
    legend.text  = element_text(size = 5),
    legend.key.size = unit(0.15, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0),
    legend.title = element_blank()
  )

# gather all the plots
p_setting <- l_setting_p[[2]] + theme(plot.margin = margin(1, 0, 0, 0)) + labs(fill="Setting",title=expression("Absolute contributions to " * R[e]* " by setting (NL)"))
p_school <- p_school + theme(plot.margin = margin(0, 0, 0, 0))
p_work <- p_work + theme(plot.margin = margin(0, 0, 0, 0))
p_gatherings <- p_gatherings + theme(plot.margin = margin(0, 0, 0, 0))
p_testing <- p_testing + theme(plot.margin = margin(0, 0, 0, 0))
p_facial_coverings <- p_facial_coverings + theme(plot.margin = margin(0, 0, 0, 0))
p_stri <- p_stri + theme(plot.margin = margin(0, 0, 0, 0))
p_dates <- p_dates + theme(plot.margin = margin(0, 0, 0, 0))
p_variants<- p_variants + theme(plot.margin = margin(0, 0, 0, 0))
p_results=plot_grid(p_setting,p_stri,p_variants,p_school, p_work,p_gatherings,p_dates, ncol=1, align="v",axis="lr", rel_heights = c(10,2,0.8,0.8,0.8,0.8,2.8))
p_results
ggsave(p_results, file = file.path(config$results_path, paste0("plots_setting_", country, ".jpg")))

p_agegroup <- l_age_p[[2]] + theme(plot.margin = margin(1, 0, 0, 0)) + labs(fill="Age group",title=expression("Absolute contributions to " * R[e]* " by age group (NL)"))
p_school <- p_school + theme(plot.margin = margin(0, 0, 0, 0))
p_work <- p_work + theme(plot.margin = margin(0, 0, 0, 0))
p_gatherings <- p_gatherings + theme(plot.margin = margin(0, 0, 0, 0))
p_testing <- p_testing + theme(plot.margin = margin(0, 0, 0, 0))
p_facial_coverings <- p_facial_coverings + theme(plot.margin = margin(0, 0, 0, 0))
p_stri <- p_stri + theme(plot.margin = margin(0, 0, 0, 0))
p_variants<- p_variants + theme(plot.margin = margin(0, 0, 0, 0))
p_results=plot_grid(p_agegroup,p_stri,p_variants,p_school, p_work,p_gatherings,p_dates, ncol=1, align="v",axis="lr", rel_heights = c(10,2,0.8,0.8,0.8,0.8,2.8))
p_results
ggsave(p_results, file = file.path(config$results_path, paste0("plots_agegroup_", country, ".jpg")))




