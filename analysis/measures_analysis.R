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
  library(ggnewscale)
  library(ggpattern)
  library(cowplot)
})

country="UK"
config <- yaml::read_yaml("config/study_config.yml") # config file with all parameters
measures = read.csv(paste0(config$data_paths$measures,paste0("OxCGRT_compact_national_v1.csv"))) # load measures data

measures <- measures %>% filter(CountryName %in% c("United Kingdom", "Belgium", "Netherlands"))
measures$Date <- as.Date(as.character(measures$Date), format = "%Y%m%d") # transform dates in as.Date  

readRDS(file.path(config$data_processed, paste0("periods_",country,"_15_2021-01-08_to_2021-07-23.rds"))) -> periods     # periods of interest

measures %>% filter(Date>=min(periods$start) & Date<=max(periods$end))->measures  # filter to the period in study
str(measures)

measures %>% 
  filter(CountryName=="United Kingdom")->measures
  
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


p_school=ggplot()+

  # closure heatmap
  geom_raster(data=measures_school, aes(x=Date,y=yname, fill=factor(C1M_School.closing)))+
  geom_line(data=measures_school, aes(x=Date,y=yname,linetype=factor(C1M_Flag)), color="black", linewidth=1.2,show.legend = FALSE)+
  scale_fill_manual(values=c("0"="#D3D3D3", "1"="#FFD700", "2"="#FFA500", "3"="#FF4500"),
                    breaks=c("0", "1", "2", "3"),
                    labels=c(
                      "0" = "No measures",
                      "1" = "Recommend closing",
                      "2" = "Require closing (some levels)",
                      "3" = "Require closing all levels"
                    ),
                    name="School closure level") +
  scale_linetype_manual(values=c("0"="solid", "1"="dashed"),
                        breaks=c("0", "1"),
                        labels=c("0"="targeted", "1"="general"),
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
    legend.title = element_text(size = 6),
    legend.text  = element_text(size = 5),
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0)
  )

measures %>% select(Date, C2M_Workplace.closing, C2M_Flag)->measures_workplace
measures_workplace$yname= "workplace"

p_work=ggplot()+
  
  # closure heatmap
  geom_raster(data=measures_workplace, aes(x=Date,y=yname, fill=factor(C2M_Workplace.closing)))+
  geom_line(data=measures_workplace, aes(x=Date,y=yname,linetype=factor(C2M_Flag)), color="black", size=1.2,show.legend = FALSE)+
  scale_fill_manual(values=c("0"="#D3D3D3", "1"="#FFD700", "2"="#FFA500", "3"="#FF4500"),
                    breaks=c("0", "1", "2", "3"),
                    labels=c(
                      "0" = "No measures",
                      "1" = "Recommend closing",
                      "2" = "Require closing (some sectors)",
                      "3" = "Require closing (except essential workers)"
                    ),
                    name="Workplace closure level") +
  scale_linetype_manual(values=c("0"="solid", "1"="dashed"),
                        breaks=c("0", "1"),
                        labels=c("0"="targeted", "1"="general"),
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
    legend.title = element_text(size = 6),
    legend.text  = element_text(size = 5),
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0)
  )

measures %>% select(Date, C4M_Restrictions.on.gatherings, C4M_Flag)->measures_gatherings
measures_gatherings$yname= "gatherings"

p_gatherings=ggplot()+
  
  # closure heatmap
  geom_raster(data=measures_gatherings, aes(x=Date,y=yname, fill=factor(C4M_Restrictions.on.gatherings)))+
  geom_line(data=measures_gatherings, aes(x=Date,y=yname,linetype=factor(C4M_Flag)), color="black", size=1.2,show.legend = FALSE)+
  scale_fill_manual(values=c("0"="#D3D3D3", "1"="#FFD700", "2"="#FFA500", "3"="#FF4500", "4"="#FF0000"),
                    breaks=c("0", "1", "2", "3","4"),
                    labels=c(
                      "0" = "No restrictions",
                      "1" = "Large gathering restrictions (>1000)",
                      "2" = "Restrictions on gatherings between 101-1000 people",
                      "3" = "Restrictions on gatherings between 11-100 people",
                      "4" = "Restrictions on gatherings of <11 people"
                    ),
                    name="Gatherings restrictions") +
  scale_linetype_manual(values=c("0"="solid", "1"="dashed"),
                        breaks=c("0", "1"),
                        labels=c("0"="targeted", "1"="general"),
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
    legend.title = element_text(size = 6),
    legend.text  = element_text(size = 5),
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0)
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
    legend.title = element_text(size = 6),
    legend.text  = element_text(size = 5),
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0)
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
  scale_linetype_manual(values=c("0"="solid", "1"="dashed"),
                        breaks=c("0", "1"),
                        labels=c("0"="targeted", "1"="general"),
                        name="Flag for geographic scope") +
  labs(x = "",
       y = "") +
  theme_cowplot()+
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week",expand=c(0,0)) +
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  theme(
    legend.key.width = unit(1, "cm")
  )+
  theme(
    legend.title = element_text(size = 6),
    legend.text  = element_text(size = 5),
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0)
  )

p_stri = ggplot(measures, aes(x=Date, y=StringencyIndex_Average)) +
  geom_line(size=1.2) +
  labs(title="Stringency Index Over Time",
       x="Date",
       y="Stringency Index") +
  theme_cowplot()+
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week",expand=c(0,0)) +
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  theme(
    legend.key.width = unit(1, "cm")
  )+
  labs(x="")+
  theme(
    legend.title = element_text(size = 6),
    legend.text  = element_text(size = 5),
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0)
  )+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )
 


p_school <- p_school + theme(plot.margin = margin(0, 0, 0, 0))
p_work <- p_work + theme(plot.margin = margin(0, 0, 0, 0))
p_gatherings <- p_gatherings + theme(plot.margin = margin(0, 0, 2, 0))
p_testing <- p_testing + theme(plot.margin = margin(0, 0, 0, 0))
p_facial_coverings <- p_facial_coverings + theme(plot.margin = margin(0, 0, 0, 0))
p_stri <- p_stri + theme(plot.margin = margin(0, 0, 0, 0))
plot_grid(p_stri,p_school, p_work,p_gatherings,p_testing,p_facial_coverings, ncol=1, align="v",axis="lr", rel_heights = c(3,1,1,1,1,2.7))










# Stringency Index over time by country
# 
# ggplot(measures, aes(x=Date, y=CountryCode,fill=StringencyIndex_Average)) +
#   geom_tile(color="white") +
#   scale_fill_gradient(low="#D3D3D3", high="#FF4500", name="Stringency Index") +
#   labs(title="Stringency Index Over Time",
#        x="Date",
#        y="Country") +
#   theme_minimal() +
#   scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week",limits = c(min(measures$Date),max(measures$Date))) +
#   theme(panel.grid = element_blank())+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.6))

# ggplot(measures, aes(x=Date, y=StringencyIndex_Average,color=CountryCode)) +
#   geom_line() +
#   geom_point() +
#   labs(title="Stringency Index Over Time",
#        x="Date",
#        y="Stringency Index") +
#   theme_minimal() +
#   scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


 

  


