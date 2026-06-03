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
# 1. Global parameters and options - define defaults here
# -------------------------------
opt=list()
opt$country <- "UK"
opt$begin <- "2020-12-24"
opt$nperiods <- 20

  if(opt$country=="UK"){
    aux_country="United Kingdom"
  } else if(opt$country=="BE"){
    aux_country="Belgium"
  } else if(opt$country=="NL"){
    aux_country="Netherlands"
  }